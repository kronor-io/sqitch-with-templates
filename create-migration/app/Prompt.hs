module Prompt (Prompt (..), prompt, promptWithCompletion) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import System.Console.Haskeline

-- | This is used so we can quickly define a new way of getting information from the user.
class Prompt tag where
    -- | The message we present when prompting the user.
    message :: tag Text -> Text

    -- | The check we make on the input that the user enters.
    inputCheck :: (tag Text, Text) -> Either Text (tag (Text, Text))

-- | Prompt the user for a value, with line editing but no tab completion.
prompt :: (Prompt tag) => tag Text -> IO (tag (Text, Text))
prompt taggedText =
    runInputT defaultSettings (loop taggedText)

-- | Prompt the user for a value, with tab completion.
promptWithCompletion :: (Prompt tag) => CompletionFunc IO -> tag Text -> IO (tag (Text, Text))
promptWithCompletion completionFunc taggedText =
    runInputT (setComplete completionFunc defaultSettings) (loop taggedText)

loop :: (Prompt tag) => tag Text -> InputT IO (tag (Text, Text))
loop taggedText = do
    let promptStr = T.unpack (message taggedText) <> "\n> "
    mInput <- getInputLine promptStr
    case mInput of
        Nothing -> liftIO (throwIO (userError "Unexpected end of input"))
        Just raw ->
            case inputCheck (taggedText, T.strip (T.pack raw)) of
                Right result -> pure result
                Left err -> do
                    outputStrLn (T.unpack err)
                    loop taggedText
