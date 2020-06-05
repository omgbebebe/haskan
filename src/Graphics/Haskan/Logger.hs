{-# language FlexibleContexts #-}
module Graphics.Haskan.Logger
  ( logI
  , showT
  ) where

-- base
import Control.Monad.IO.Class (MonadIO)
import GHC.Stack (CallStack, HasCallStack, callStack)

-- co-log
import Colog ((<&))
import qualified Colog

-- text
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

showT :: Show a => a -> Text
showT = Text.pack . show
{-
processLinesLog :: (Colog.WithLog env Colog.Message m, MonadIO m) => m ()
processLinesLog = do
  Colog.logInfo "Enter some string"
  line <- liftIO TextIO.getLine
  case Text.length line of
    0 -> do
      Colog.logWarning "Empty input"
      processLinesLog
    n -> do
      Colog.logDebug "Correct line"
      Colog.logInfo $ "Line length: " <> Text.pack (show n)
-}
--withLogger :: (WithLog env Message m, MonadIO m) => m ()
withLogger a = let action = Colog.richMessageAction
  in Colog.usingLoggerT action a

logD :: (HasCallStack, MonadIO m) => Text -> m()
logD = logMsg callStack Colog.Debug

logI :: (HasCallStack, MonadIO m) => Text -> m()
logI = logMsg callStack Colog.Info

logW :: (HasCallStack, MonadIO m) => Text -> m()
logW = logMsg callStack Colog.Warning

logE :: (HasCallStack, MonadIO m) => Text -> m()
logE = logMsg callStack Colog.Error

logMsg :: (HasCallStack, MonadIO m) => CallStack -> Colog.Severity -> Text -> m ()
logMsg cs s msg = Colog.richMessageAction <& (Colog.Msg s cs msg)
