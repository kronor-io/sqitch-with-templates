{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Config (Config (..)) where

import Data.String.Interpolate (iii)
import Prompt

data Config a
    = TriggerCfg {kind :: a}
    | FunctionCfg {kind :: a}

instance Prompt Config where
    message = \case
        TriggerCfg "type" -> [iii| Enter trigger type. \n1) Row\n2) Statement |]
        TriggerCfg _ -> error "Unknown TriggerCfg"
        FunctionCfg "type" -> [iii| Enter function type. \n1) General function\n2) Statechart trigger function |]
        FunctionCfg _ -> error "Unknown FunctionCfg"
    inputCheck = \case
        (TriggerCfg "type", "1") -> Right $ TriggerCfg ("type", "row")
        (TriggerCfg "type", "2") -> Right $ TriggerCfg ("type", "statement")
        (TriggerCfg _, _) -> Left "Invalid trigger type. Try again."
        (FunctionCfg "type", "1") -> Right $ FunctionCfg ("type", "general")
        (FunctionCfg "type", "2") -> Right $ FunctionCfg ("type", "statechart_trigger")
        (FunctionCfg _, _) -> Left "Invalid function type. Try again."
