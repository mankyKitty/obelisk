{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module LinuxApp where

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
-- import Obelisk.Backend
-- import Obelisk.Frontend
-- import Obelisk.Route.Frontend
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

import Language.Javascript.JSaddle.Webkit2Gtk (runLinuxWebView)

import Backend (backend)
import Frontend (frontend)

data LinuxFFI = LinuxFFI
  { _linuxFFI_global_openFileDialog :: IO (Maybe String)
  , _linuxFFI_global_getHomeDirectory :: IO String
  }

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

main :: IO ()
main = do
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

  -- Run the backend in a forked thread, and run webkit2gtk on the main thread
  putStrLn $ "Starting backend on port: " <> show port

  Async.withAsync b $ \_ -> do
    liftIO $ putStrLn "Starting jsaddle"

    runLinuxWebView route handleOpen $ do
      mInitFile <- liftIO $ tryTakeMVar fileOpenedMVar

      let frontendMode = FrontendMode
            { _frontendMode_hydrate = False
            , _frontendMode_adjustRoute = True
            }

      liftIO $ putStrLn "Starting frontend"

      -- Run real obelisk frontend
      runFrontendWithConfigsAndCurrentRoute frontendMode mempty backendEncoder frontend
