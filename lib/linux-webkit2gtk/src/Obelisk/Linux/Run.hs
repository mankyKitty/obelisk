import Control.Lens (_head, preview)
import Control.Monad ((<=<), (>=>))
import Data.ByteString (ByteString)
import Data.Default (Default(..))
import Data.Functor (void)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt(..))
import Foreign.StablePtr (StablePtr, newStablePtr)
import Language.Javascript.JSaddle.Types (JSM)

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE

import qualified System.Posix.User as PU

import qualified GI.Gtk as Gtk

import CustomRun (customRun)

openFileDialog :: IO (Maybe String)
openFileDialog = do
  filter <- Gtk.fileFilterNew
  Gtk.fileFilterAddPattern filter (T.pack "*.pact")

  chooser <- Gtk.fileChooserNativeNew
    (Just $ T.pack "Open Pact File")
    Gtk.noWindow
    Gtk.FileChooserActionOpen
    Nothing
    Nothing

  Gtk.fileChooserAddFilter chooser filter
  Gtk.fileChooserSetSelectMultiple chooser False

  res <- Gtk.nativeDialogRun chooser
  case toEnum (fromIntegral res) of
    Gtk.ResponseTypeAccept -> preview _head <$> Gtk.fileChooserGetFilenames chooser
    _ -> pure Nothing
  where
    floop :: (Gtk.IsGValue x, Gtk.IsGValue y) => x -> IO y
    floop = Gtk.toGValue >=> Gtk.fromGValue

getHomeDirectory :: IO String
getHomeDirectory = PU.getLoginName >>= fmap PU.homeDirectory . PU.getUserEntryForName

runLinux :: ByteString -> JSM () -> IO ()
runLinux route jsm = customRun (TE.decodeUtf8With TE.lenientDecode allowing) jsm
