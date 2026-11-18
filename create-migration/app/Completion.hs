module Completion (
    -- * Haskeline completions (interactive prompts)
    schemaCompletion,
    tableCompletion,

    -- * optparse-applicative completers (bash/zsh/fish)
    dbCompleter,
    schemaCompleter,
    tableCompleter,

    -- * Shared
    listSchemas,
    listTables,
) where

import Control.Monad (filterM)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative (Completer, mkCompleter)
import SqitchConf
import System.Console.Haskeline
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))

-- | Complete schema names: the subdirectories of @<top_dir>/deploy@.
schemaCompletion :: FilePath -> CompletionFunc IO
schemaCompletion topDir =
    completeWord Nothing " \t" \prefix -> do
        schemas <- listSchemas topDir
        pure [simpleCompletion s | s <- schemas, prefix `isPrefixOf` s]

-- | Complete table names in the schema already entered.
tableCompletion :: FilePath -> Map Text Text -> CompletionFunc IO
tableCompletion topDir collectedVars =
    completeWord Nothing " \t" \prefix ->
        case Map.lookup "schema" collectedVars of
            Nothing -> pure []
            Just schema -> do
                tables <- listTables topDir (T.unpack schema)
                pure [simpleCompletion t | t <- tables, prefix `isPrefixOf` t]

{- | Schema directories under @<top_dir>/deploy@, minus the time-based
@migrations@ folder.
-}
listSchemas :: FilePath -> IO [String]
listSchemas topDir = do
    let path = topDir </> "deploy"
    exists <- doesDirectoryExist path
    if not exists
        then pure []
        else do
            entries <- listDirectory path
            filter (`notElem` ["migrations", "migration"])
                <$> filterM (doesDirectoryExist . (path </>)) entries

{- | Tables of a schema: @<top_dir>/deploy/<schema>/table/*.sql@, without
@sqitch rework@ snapshots (@name\@tag.sql@).
-}
listTables :: FilePath -> String -> IO [String]
listTables topDir schema = do
    let path = topDir </> "deploy" </> schema </> "table"
    exists <- doesDirectoryExist path
    if not exists
        then pure []
        else do
            entries <- listDirectory path
            pure
                [ take (length f - 4) f
                | f <- entries
                , ".sql" `isSuffixOf` f
                , '@' `notElem` f
                ]

-- | Target names from @sqitch.conf@.
dbCompleter :: Completer
dbCompleter = mkCompleter \prefix -> do
    targets <- listTargets <$> readSqitchConf
    pure [t | t <- targets, prefix `isPrefixOf` t]

schemaCompleter :: Completer
schemaCompleter = mkCompleter \prefix -> do
    topDir <- topDirFromCompWords
    schemas <- listSchemas topDir
    pure [s | s <- schemas, prefix `isPrefixOf` s]

-- | Table names in the schema given by @--schema@/@-s@ earlier on the line.
tableCompleter :: Completer
tableCompleter = mkCompleter \prefix -> do
    topDir <- topDirFromCompWords
    mSchema <- firstJustM lookupCompWord ["--schema", "-s"]
    case mSchema of
        Nothing -> pure []
        Just schema -> do
            tables <- listTables topDir schema
            pure [t | t <- tables, prefix `isPrefixOf` t]

{- | The value following a flag on the command line being completed.
optparse-applicative passes those words as @--bash-completion-word@ arguments.
-}
lookupCompWord :: String -> IO (Maybe String)
lookupCompWord flag = findAfter . compWords <$> getArgs
  where
    compWords ("--bash-completion-word" : w : rest) = w : compWords rest
    compWords (_ : rest) = compWords rest
    compWords [] = []

    findAfter (x : y : rest)
        | x == flag = Just y
        | otherwise = findAfter (y : rest)
    findAfter _ = Nothing

firstJustM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstJustM _ [] = pure Nothing
firstJustM f (x : xs) = f x >>= maybe (firstJustM f xs) (pure . Just)

topDirFromCompWords :: IO FilePath
topDirFromCompWords = do
    target <- lookupCompWord "--db"
    conf <- readSqitchConf
    pure (resolveTopDir conf target)
