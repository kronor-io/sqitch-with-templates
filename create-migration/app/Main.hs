{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Config
import Control.Monad (foldM, forM_, when)
import Data.Bifunctor (bimap)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.List (nubBy)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Options.Applicative hiding (command)
import ProgramOptions
import Prompt
import Sqitch.Command
import Sqitch.Template
import System.Exit
import System.Info (os)
import System.Process (callCommand, shell, waitForProcess, withCreateProcess)

main :: IO ()
main = do
    command <- execParser opts
    currentTime <- getCurrentTime
    case command of
        Create db (Trigger _placeholder) -> workWith db currentTime . parseTriggerType =<< prompt (TriggerCfg "type")
        Create db (Function _placeholder) -> workWith db currentTime . parseFunctionType =<< prompt (FunctionCfg "type")
        Create db template -> workWith db currentTime template
  where
    opts =
        info
            (commandParser <**> helper)
            ( fullDesc
                <> progDesc "Helper to create migration templates for Sqitch"
                <> header "kronor-dev: utility for regular kronor development"
            )

    workWith db currentTime template = do
        let specs = metaOf template
        let variablesToCollect =
                concatMap vars specs
                    & nubBy (\a b -> a.var == b.var)

        collectedVariables <- foldM buildVariablesMap mempty variablesToCollect

        let sqitchVariables =
                collectedVariables
                    & Map.toList
                    & map (bimap Field Value)

        forM_ specs \spec -> do
            let description = fromMaybe (error "no description for migration") (spec.description collectedVariables)
            let localMap = Map.insert "description" description collectedVariables

            let path = case spec.layout currentTime localMap of
                    Nothing -> error "layout failed"
                    Just a -> a

            cmd <- buildCmd db (Path path) spec.name (Comment description) sqitchVariables
            exitCode <- withCreateProcess (shell (T.unpack cmd)) \_ _ _ ph -> waitForProcess ph

            case exitCode of
                ExitSuccess -> return ()
                ExitFailure _ -> error "Failed to run sqitch command"

    buildVariablesMap :: Map Text Text -> Source Text -> IO (Map Text Text)
    buildVariablesMap acc source = do
        let variableExists = isJust . flip Map.lookup acc . var
        if variableExists source
            then return acc
            else case source of
                Infer _ _ infer -> do
                    let inferred = infer acc
                    case inferred of
                        Nothing -> do
                            x <- prompt source
                            return $ Map.insert source.var (snd x.var) acc
                        Just x -> return $ Map.insert source.var x acc
                _ -> do
                    x <- prompt source
                    return $ Map.insert source.var (snd x.var) acc
