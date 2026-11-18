module Main (main) where

import Completion
import Control.Monad (foldM, forM_)
import Data.List (nubBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time
import Options.Applicative
import Paths_create_migration (getDataDir)
import ProgramOptions
import Prompt
import Sqitch.Command
import Sqitch.Template
import SqitchConf
import System.FilePath ((</>))

main :: IO ()
main = do
    cmd <- execParser opts
    now <- getCurrentTime
    conf <- readSqitchConf
    templateDir <- maybe ((</> "templates") <$> getDataDir) pure cmd.templateDir
    let topDir = resolveTopDir conf cmd.db
        (template, cliVars) = resolveCommand cmd.migration
        specs = metaOf template
        toCollect = nubBy (\a b -> a.var == b.var) (concatMap (.vars) specs)

    vars <- foldM (collect topDir) cliVars toCollect

    forM_ specs \spec -> do
        let description = fromMaybe (error ("no description for " <> show spec.name)) (spec.description vars)
            path =
                fromMaybe
                    (error ("cannot lay out " <> show spec.name))
                    (spec.layout now (Map.insert "description" description vars))
        runSqitchAdd
            SqitchAdd
                { changePath = path
                , target = cmd.db
                , template = spec.name
                , note = description
                , variables = vars
                , templateDir
                }
  where
    opts =
        info
            (commandParser <**> helper)
            ( fullDesc
                <> progDesc "Create sqitch changes from migration templates"
                <> header "create-migration: sqitch add, with templates for safe Postgres migrations"
            )

{- | Fill in a variable that was not given on the command line: infer it if
possible, else ask for it.
-}
collect :: FilePath -> Map Text Text -> Source Text -> IO (Map Text Text)
collect topDir acc source
    | Map.member source.var acc = pure acc
    | Infer{infer} <- source, Just v <- infer acc = pure (Map.insert source.var v acc)
    | otherwise = do
        answer <- ask source
        pure (Map.insert source.var (snd answer.var) acc)
  where
    ask = case source.var of
        "schema" -> promptWithCompletion (schemaCompletion topDir)
        "table_name" -> promptWithCompletion (tableCompletion topDir acc)
        "trigger_table" -> promptWithCompletion (tableCompletion topDir acc)
        _ -> prompt
