{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Prompt (Prompt (..), prompt) where

import Control.Monad.IO.Class
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

-- | This is used so we can quickly define a new way of getting information from the user.
class Prompt tag where
    -- | The message we present when prompting the user.
    message :: tag Text -> Text

    -- | The check we make on the input that the user enters.
    inputCheck :: (tag Text, Text) -> Either Text (tag (Text, Text))

-- | This is used to prompt the user for a specific value.
prompt :: (MonadIO m, MonadFail m, Prompt tag) => tag Text -> m (tag (Text, Text))
prompt taggedText =
    runUntil getLine $ do
        liftIO $ BS.putStr (T.encodeUtf8 $ message taggedText)
        return (inputCheck . (taggedText,))
  where
    getLine :: (MonadFail m, MonadIO m) => m Text
    getLine = liftIO $ do
        BS.getLine >>= either (\l -> fail ("Invalid utf8 in function getLine: " <> show l)) return . T.decodeUtf8'

    runUntil :: (MonadIO m) => m result -> m (result -> Either Text a) -> m a
    runUntil run prepareCheck = do
        check <- prepareCheck -- this is an action, so we can print stuff, for example
        _ <- liftIO $ BS.putStr (T.encodeUtf8 "\n> ")
        run >>= either handleCheckFailure return . check
      where
        handleCheckFailure l = do
            -- if its not valid we need to repeat the action
            liftIO $ BS.putStr (T.encodeUtf8 (l <> "\n")) -- print error message
            runUntil run prepareCheck -- tailcall
