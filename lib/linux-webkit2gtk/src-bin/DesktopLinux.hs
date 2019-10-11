{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module DesktopLinux where

import Debug.Trace (traceShowM,traceShowId)
import Control.Concurrent
import Control.Lens ((^.))
import Control.Exception (bracket_, bracket, try)
import Control.Monad (forever,void)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import Data.Maybe (isJust)
import Data.Foldable (for_, traverse_)
import Data.Proxy (Proxy(..))
import Data.String (IsString(..))
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt(..))
import Foreign.StablePtr (StablePtr)
import GHC.IO.Handle
import Language.Javascript.JSaddle.Types (JSM)
import qualified Language.Javascript.JSaddle as JS
import Obelisk.Backend
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Reflex.Dom
import Servant.API
import System.FilePath ((</>))
import System.IO
import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Network.Socket as Socket
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Wai
import qualified Servant.Server as Servant
import qualified Snap.Http.Server as Snap
import qualified System.Directory as Directory
import qualified System.Environment as Env
import qualified System.Process as Process

import Backend (serveBackendRoute)
import Frontend

data LinuxFFI = LinuxFFI
  { _linuxFFI_global_openFileDialog :: IO (Maybe String)
  , _linuxFFI_global_getHomeDirectory :: IO String
  }

getUserLibraryPath :: MonadIO m => LinuxFFI -> m FilePath
getUserLibraryPath ffi = liftIO $ do
  home <- _linuxFFI_global_getHomeDirectory ffi
  let lib = home </> ".obelisk-linux-exe"
  Directory.createDirectoryIfMissing True lib
  pure lib

-- | Redirect the given handles to Console.app
redirectPipes :: [Handle] -> IO a -> IO a
redirectPipes ps m = bracket setup hClose $ \r -> Async.withAsync (go r) $ \_ -> m
  where
    setup = do
      (r, w) <- Process.createPipe
      for_ ps $ \p -> hDuplicateTo w p <> hSetBuffering p LineBuffering
      hClose w
      pure r
      -- TODO figure out how to get the logs to come from the Pact process instead of syslog
    go r = forever $ hGetLine r >>= \l -> do
      Process.callProcess "syslog" ["-s", "-k", "Level", "Notice", "Message", "Pact: " <> l]

-- | Get a random free port. This isn't quite safe: it is possible for the port
-- to be grabbed by something else between the use of this function and the
-- ultimate use by the backend.
getFreePort :: IO Socket.PortNumber
getFreePort = Socket.withSocketsDo $ do
  addr:_ <- Socket.getAddrInfo (Just Socket.defaultHints) (Just "127.0.0.1") (Just "0")
  bracket (open addr) Socket.close Socket.socketPort
  where
    open addr = do
      sock <- Socket.socket (Socket.addrFamily addr) (Socket.addrSocketType addr) (Socket.addrProtocol addr)
      Socket.bind sock (Socket.addrAddress addr)
      pure sock

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    cfg' <- pure undefined -- TODO FIXME used by OAuth
    serve $ serveBackendRoute Nothing cfg'
  , _backend_routeEncoder = backendRouteEncoder
  }

type SigningApi = "v1" :> V1SigningApi
type V1SigningApi = "sign" :> ReqBody '[JSON] SigningRequest :> Post '[JSON] SigningResponse

main'
  :: LinuxFFI
  -> (BS.ByteString -> BS.ByteString -> (String -> IO ()) -> (FilePath -> IO Bool) -> JSM () -> IO ())
  -> IO ()
main' ffi runHTML = do
  exePath <- L.init . L.dropWhileEnd (/= '/') <$> Env.getExecutablePath

  let basePath :: (IsString a, Semigroup a) => a
      basePath = fromString exePath

  putStrLn $ "Executable path: " <> exePath
  port <- getFreePort

  let staticAssets = StaticAssets
        { _staticAssets_processed = basePath </> "static.assets"
        , _staticAssets_unprocessed = basePath </> "static"
        }
      backendEncoder = either (error "frontend: Failed to check backendRouteEncoder") id $
        checkEncoder backendRouteEncoder

      route :: (IsString s, Semigroup s) => s
      route = "http://localhost:" <> fromString (show port) <> "/"

      -- We don't need to serve anything useful here under Frontend
      b = runBackendWith
        (runSnapWithConfig $ Snap.setPort (fromIntegral port) Snap.defaultConfig)
        staticAssets
        backend
        (Frontend blank blank)
  fileOpenedMVar :: MVar T.Text <- liftIO newEmptyMVar
  -- Run the backend in a forked thread, and run jsaddle-wkwebview on the main thread
  putStrLn $ "Starting backend on port: " <> show port
  Async.withAsync b $ \_ -> do
    liftIO $ putStrLn "Starting jsaddle"
    let handleOpen f = try (T.readFile f) >>= \case
          Left (e :: IOError) -> do
            putStrLn $ "Failed reading file " <> f <> ": " <> show e
            pure False
          Right c -> do
            putStrLn $ "Opened file successfully: " <> f
            putMVar fileOpenedMVar c
            pure True
    signingLock <- liftIO newEmptyMVar -- Only allow one signing request to be served at once
    signingRequestMVar <- liftIO newEmptyMVar
    signingResponseMVar <- liftIO newEmptyMVar
    let runSign obj = do
          resp <- liftIO $ bracket_ (putMVar signingLock ()) (takeMVar signingLock) $ do
            putMVar signingRequestMVar obj -- handoff to app
            takeMVar signingResponseMVar
            -- bracket
            --   (do
            --       _linuxFFI_moveToForeground ffi
            --       _linuxFFI_global_requestUserAttention ffi
            --   )
            --   (\r -> do
            --       _linuxFFI_moveToBackground ffi
            --       _linuxFFI_global_cancelUserAttentionRequest ffi r
            --   )
            --   (\_ -> takeMVar signingResponseMVar)
          case resp of
            Left e -> throwError $ Servant.err409
              { Servant.errBody = LBS.fromStrict $ T.encodeUtf8 e }
            Right v -> pure v
        s = Warp.setPort 9467 Warp.defaultSettings
        laxCors _ = Just $ Wai.simpleCorsResourcePolicy
          { Wai.corsRequestHeaders = Wai.simpleHeaders }
        apiServer
          = Warp.runSettings s $ Wai.cors laxCors
          $ Servant.serve (Proxy @SigningApi) runSign
    _ <- Async.async $ apiServer
    runHTML "index.html" route putStrLn handleOpen $ do
      mInitFile <- liftIO $ tryTakeMVar fileOpenedMVar

      let frontendMode = FrontendMode
            { _frontendMode_hydrate = False
            , _frontendMode_adjustRoute = True
            }
          configs = M.fromList -- TODO don't embed all of these into binary
            [ ("common/route", route)
            , ("common/networks", "remote-source:https://pact.kadena.io/networks")
            , ("common/oauth/github/client-id", "") -- TODO remove
            ]
      liftIO $ putStrLn "Starting frontend"
      bowserMVar :: MVar () <- liftIO newEmptyMVar
      -- Run real obelisk frontend
      runFrontendWithConfigsAndCurrentRoute frontendMode configs backendEncoder $ Frontend
        -- TODO we shouldn't have to use prerender since we aren't hydrating
        { _frontend_head = prerender_ blank $ do
            bowserLoad <- newHead $ \r ->
              T.pack $ T.unpack route </> T.unpack (renderBackendRoute backendEncoder r)
            performEvent_ $ liftIO . putMVar bowserMVar <$> bowserLoad
        , _frontend_body = prerender_ blank $ do
          bowserLoad <- mvarTriggerEvent bowserMVar
          -- fileOpened <- mvarTriggerEvent fileOpenedMVar
          signingRequest <- mvarTriggerEvent signingRequestMVar
          (fileOpened, triggerOpen) <- openFileDialog
          let appCfg = AppCfg
                { _appCfg_gistEnabled = False
                , _appCfg_externalFileOpened = fileOpened
                , _appCfg_openFileDialog = JS.liftJSM triggerOpen
                , _appCfg_loadEditor = pure mInitFile
                -- DB 2019-08-07 Changing this back to False because it's just too convenient this way.
                , _appCfg_editorReadOnly = False
                , _appCfg_signingRequest = signingRequest
                , _appCfg_signingResponse = liftIO . putMVar signingResponseMVar
                , _appCfg_forceResize = never
                }
          -- Should I be making sure this can be saved on the machine itself, like the mac ?
          _ <- flip runStorageT browserStorage $ runWithReplace loaderMarkup $ app appCfg <$ bowserLoad
          pure ()
        }

-- | Push writes to the given 'MVar' into an 'Event'.
mvarTriggerEvent
  :: (PerformEvent t m, TriggerEvent t m, MonadIO m)
  => MVar a -> m (Event t a)
mvarTriggerEvent mvar = do
  (e, trigger) <- newTriggerEvent
  _ <- liftIO $ forkIO $ forever $ trigger =<< takeMVar mvar
  pure e
