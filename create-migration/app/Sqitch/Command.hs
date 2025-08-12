{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Sqitch.Command (Value (..), Path (..), buildCmd, Comment (..), Field (..)) where

import Data.String.Interpolate (iii)
import Data.Text (Text)
import Data.Text qualified as T
import Paths_create_migration
import ProgramOptions
import System.Directory (findExecutable)
import System.Path (takeSuperDirectory, (</>))
import System.Path qualified as Path
import System.Path.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)

-- | We use this to build the final command with all the information interpolated.
buildCmd :: Db -> Path -> Text -> Comment -> [(Field, Value)] -> IO Text
buildCmd db path template comment protoFields = do
    let builtVars = T.unwords (uncurry buildVariableSet <$> protoFields)
    withLocalSqitch <- findExecutable "sqitch"
    dataDir <- getDataDir
    return $ buildTop db withLocalSqitch ((Path.absDir dataDir) </> (Path.relDir "templates")) path template comment (FieldsResult builtVars)

{- | We use this to build the top level command, together with fields.
More like a helper.
-}
buildTop :: Db -> Maybe FilePath -> Path.AbsDir -> Path -> Text -> Comment -> FieldsResult -> Text
buildTop db withLocalSqitch templatePath (Path path) templateName (Comment comment) (FieldsResult fields) =
    [iii|
        #{sqitch}
            add '#{path}'
            '#{targetName db}'
            --template '#{templateName}'
            -n '#{templateName}: #{comment}' #{fields}
            #{templateDir}
    |]
  where
    sqitch :: Text
    sqitch = maybe "sqitch" T.pack withLocalSqitch
    templateDir :: Text
    templateDir = [iii|--template-directory '#{Path.toString $ Path.normalise $ templatePath}'|]

-- | Simply build the syntax for defining a variable.
buildVariableSet :: Field -> Value -> Text
buildVariableSet (Field field) (Value value) =
    case field of
        "description" -> "" -- already used somewhere else
        _ -> [iii| -s #{field}='#{value}' |]

-------------
-- HELPERS --
-------------

newtype Field = Field Text
newtype Value = Value Text
newtype Comment = Comment Text
newtype Path = Path Text
newtype FieldsResult = FieldsResult Text
