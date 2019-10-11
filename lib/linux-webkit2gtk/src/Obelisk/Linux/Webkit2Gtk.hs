{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Obelisk.Linux.Webkit2Gtk (runLinuxWebView) where

import Debug.Trace (traceShowM, traceShowId)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..), MonadIO)
import Control.Concurrent (forkIO, yield)

import System.Posix.Signals (installHandler, keyboardSignal, Handler(Catch))
import System.Posix.Process (forkProcess, executeFile)

import System.Directory (getCurrentDirectory)

import Debug.Trace (traceShowM)

import qualified System.Environment as Env
import qualified Data.List as L
import Data.Monoid ((<>))
import Data.ByteString.Lazy (toStrict, fromStrict)
import qualified Data.ByteString.Lazy as LB (ByteString)
import Data.Aeson (encode, decode)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Data.GI.Base.BasicTypes (GObject)
import Data.GI.Base.Signals
       (connectSignalFunPtr, SignalConnectMode(..), SignalConnectMode,
        SignalHandlerId)

import GI.GLib (timeoutAdd, idleAdd, pattern PRIORITY_HIGH, pattern PRIORITY_DEFAULT)
import qualified GI.Gtk as Gtk (main, init)
import GI.Gtk
       (windowSetPosition, windowSetDefaultSize, windowNew,
        scrolledWindowNew, noAdjustment, containerAdd,
        WindowType(..), WindowPosition(..), widgetDestroy,
        widgetGetToplevel, widgetShowAll, onWidgetDestroy,
        mainQuit)
import GI.Gio (noCancellable)
import qualified GI.Gio as Gio
import GI.JavaScriptCore (valueToString)
import qualified GI.WebKit2 as WK2
import GI.WebKit2
       (scriptDialogPromptSetText, scriptDialogPromptGetDefaultText,
        scriptDialogGetMessage, scriptDialogGetDialogType,
        onWebViewScriptDialog, WebView,webViewGetContext,
        setSettingsEnableWriteConsoleMessagesToStdout,
        setSettingsEnableJavascript, webViewNewWithUserContentManager,
        setSettingsAllowFileAccessFromFileUrls, setSettingsEnableHtml5LocalStorage,
        setSettingsAllowUniversalAccessFromFileUrls,
        userContentManagerNew,
        userContentManagerRegisterScriptMessageHandler,
        javascriptResultGetJsValue,
        webViewGetUserContentManager, webViewGetContext, webContextGetSecurityManager,
        securityManagerUriSchemeIsCorsEnabled, securityManagerUriSchemeIsLocal, 
        securityManagerRegisterUriSchemeAsSecure,
        securityManagerRegisterUriSchemeAsCorsEnabled,
        securityManagerRegisterUriSchemeAsLocal,
        mk_UserContentManagerScriptMessageReceivedCallback,
        wrap_UserContentManagerScriptMessageReceivedCallback,
        webViewRunJavascript, LoadEvent(..),
        UserContentManagerScriptMessageReceivedCallback, webViewLoadHtml, webViewLoadUri,
        onWebViewLoadChanged, setSettingsEnableDeveloperExtras,
        webViewSetSettings, webViewGetSettings, ScriptDialogType(..))

import Language.Javascript.JSaddle (JSM, Results, Batch)
import Language.Javascript.JSaddle.Run (runJavaScript)
import Language.Javascript.JSaddle.Run.Files (initState, runBatch, ghcjsHelpers)

quitWebView :: WebView -> IO ()
quitWebView wv = postGUIAsync $ do w <- widgetGetToplevel wv --TODO: Shouldn't this be postGUISync?
                                   widgetDestroy w

installQuitHandler :: WebView -> IO ()
installQuitHandler wv = void $ installHandler keyboardSignal (Catch (quitWebView wv)) Nothing

postGUIAsync :: IO () -> IO ()
postGUIAsync action =
  void . idleAdd PRIORITY_DEFAULT $ action >> return False

decidePolicyCallback
  :: WK2.PolicyDecision
  -> WK2.PolicyDecisionType
  -> IO Bool
decidePolicyCallback pd pt = case pt of
  WK2.PolicyDecisionTypeNewWindowAction -> openUsingLocal
  WK2.PolicyDecisionTypeNavigationAction -> openUsingLocal
  _ -> False <$ traceShowM pt
  where
    openUsingLocal = do
      np <- Gio.unsafeCastTo WK2.NavigationPolicyDecision pd
      na <- WK2.navigationPolicyDecisionGetNavigationAction np
      ntype <- traceShowId <$> WK2.navigationActionGetNavigationType na
      case ntype of
        WK2.NavigationTypeLinkClicked -> do
          requested <- WK2.uRIRequestGetUri =<< WK2.navigationActionGetRequest na
          forkProcess $ executeFile "xdg-open" True [T.unpack requested] Nothing
          pure True
        t ->
          False <$ traceShowM t

runLinuxWebView :: T.Text -> JSM () -> IO ()
runLinuxWebView route main = do
  _ <- Gtk.init Nothing
  window <- windowNew WindowTypeToplevel
  _ <- timeoutAdd PRIORITY_HIGH 10 (yield >> return True)
  windowSetDefaultSize window 900 600
  windowSetPosition window WindowPositionCenter
  scrollWin <- scrolledWindowNew noAdjustment noAdjustment
  contentManager <- userContentManagerNew
  webView <- webViewNewWithUserContentManager contentManager
  settings <- webViewGetSettings webView

  setSettingsAllowUniversalAccessFromFileUrls settings True
  setSettingsAllowFileAccessFromFileUrls settings True
  setSettingsEnableDeveloperExtras settings True
  setSettingsEnableJavascript settings True
  setSettingsEnableWriteConsoleMessagesToStdout settings True

  WK2.onWebViewDecidePolicy webView decidePolicyCallback

  webViewSetSettings webView settings

  window `containerAdd` scrollWin
  scrollWin `containerAdd` webView
  _ <- onWidgetDestroy window mainQuit
  widgetShowAll window
  pwd <- getCurrentDirectory
  void . onWebViewLoadChanged webView $ \case
      LoadEventFinished -> runInWebView main webView
      _ -> return ()

  -- webViewLoadUri webView route
  let html = "<html><head></head><body></body></html>"
  WK2.webViewLoadHtml webView html route

  installQuitHandler webView
  Gtk.main

runInWebView :: JSM () -> WebView -> IO ()
runInWebView f webView = do
    (processResults, syncResults, start) <- runJavaScript (\batch -> postGUIAsync $
        webViewRunJavascript webView (decodeUtf8 . toStrict $ "runJSaddleBatch(" <> encode batch <> ");") noCancellable Nothing)
        f

    addJSaddleHandler webView processResults syncResults
    webViewRunJavascript webView (decodeUtf8 $ toStrict jsaddleJs) noCancellable . Just $
        \_obj _asyncResult ->
            void $ forkIO start

onUserContentManagerScriptMessageReceived :: (GObject a, MonadIO m) => a -> UserContentManagerScriptMessageReceivedCallback -> m SignalHandlerId
onUserContentManagerScriptMessageReceived obj cb = liftIO $ connectUserContentManagerScriptMessageReceived obj cb SignalConnectBefore

connectUserContentManagerScriptMessageReceived :: (GObject a, MonadIO m) =>
                                                  a -> UserContentManagerScriptMessageReceivedCallback -> SignalConnectMode -> m SignalHandlerId
connectUserContentManagerScriptMessageReceived obj cb after = liftIO $ do
    let cb' = wrap_UserContentManagerScriptMessageReceivedCallback cb
    cb'' <- mk_UserContentManagerScriptMessageReceivedCallback cb'
    connectSignalFunPtr obj "script-message-received::jsaddle" cb'' after
#if MIN_VERSION_haskell_gi_base(0,23,0)
      Nothing
#endif

addJSaddleHandler :: WebView -> (Results -> IO ()) -> (Results -> IO Batch) -> IO ()
addJSaddleHandler webView processResult syncResults = do
    manager <- webViewGetUserContentManager webView
    _ <- onUserContentManagerScriptMessageReceived manager $ \result -> do
        arg <- javascriptResultGetJsValue result
        bs <- encodeUtf8 <$> valueToString arg
        mapM_ processResult (decode (fromStrict bs))
    _ <- onWebViewScriptDialog webView $ \dialog ->
        scriptDialogGetDialogType dialog >>= \case
            ScriptDialogTypePrompt ->
                scriptDialogGetMessage dialog >>= \case
                    "JSaddleSync" -> do
                        resultsText <- scriptDialogPromptGetDefaultText dialog
                        case decode (fromStrict $ encodeUtf8 resultsText) of
                            Just results -> do
                                batch <- syncResults results
                                scriptDialogPromptSetText dialog (decodeUtf8 . toStrict $ encode batch)
                                return True
                            Nothing -> return False
                    _ -> return False
            _ -> return False

    void $ userContentManagerRegisterScriptMessageHandler manager "jsaddle"

jsaddleJs :: LB.ByteString
jsaddleJs = ghcjsHelpers <> mconcat
    [ "runJSaddleBatch = (function() {\n"
    , initState
    , "\nreturn function(batch) {\n"
    , runBatch (\a -> "window.webkit.messageHandlers.jsaddle.postMessage(JSON.stringify(" <> a <> "));\n")
               (Just (\a -> "JSON.parse(window.prompt(\"JSaddleSync\", JSON.stringify(" <> a <> ")))"))
    , "};\n"
    , "})()"
    ]

#endif
