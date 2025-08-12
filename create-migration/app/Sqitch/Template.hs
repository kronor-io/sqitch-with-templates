{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Sqitch.Template (
    TemplateSpec (..),
    TemplateEnum (..),
    CompositeMigration (..),
    metaOf,
    Trigger (..),
    FunctionType (..),
    Source (..),
    parseTriggerType,
    parseFunctionType,
) where

import Config
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.String.Interpolate (iii)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Data.Time qualified as Time
import Prompt

-- | `Global`'s come from the config file, `Local`'s are gathered from the user.
data Source a
    = Local {var :: a, desc :: Text}
    | Infer {var :: a, desc :: Text, infer :: Map a Text -> Maybe Text}

instance Prompt Source where
    message = \case
        Local var desc -> [iii| Enter #{var}'s value; #{desc}. |]
        Infer var desc _ -> [iii| Enter #{var}'s value; #{desc}. |]

    inputCheck = \case
        (Local var desc, res) ->
            if not . T.null $ res
                then Right (Local (var, res) desc)
                else Left "Empty value is not acceptable."
        (Infer var desc _, res) ->
            if not . T.null $ res
                then Right (Local (var, res) desc)
                else Left "Empty value is not acceptable."

parseTriggerType :: Config (Text, Text) -> TemplateEnum
parseTriggerType x = case x.kind of
    ("type", "row") -> Trigger Row
    ("type", "statement") -> Trigger Statement
    _ -> error "invalid trigger type"

parseFunctionType :: Config (Text, Text) -> TemplateEnum
parseFunctionType x = case x.kind of
    ("type", "general") -> Function GeneralFunction
    ("type", "statechart_trigger") -> Function StatechartTriggerFunction
    _ -> error "invalid trigger type"

---------------
-- TEMPLATES --
---------------

{- | Template enumeration.
TODO This can be fused with TemplateSpec
-}
data TemplateEnum
    = Table
    | View
    | Function FunctionType
    | Trigger Trigger
    | Domain
    | Migration
    | BulkUpdate
    | AddIndex
    | AddNotNullConstraint
    | ValidateConstraint
    | MakeColumnNotNull
    | AddColumn
    | RenameColumnStep1
    | RenameColumnStep2
    | Compose CompositeMigration

data CompositeMigration = NonNullableColumn

-- | Trigger sub type.
data Trigger = Row | Statement

data FunctionType = GeneralFunction | StatechartTriggerFunction

-- | A template specification.
data TemplateSpec = TemplateSpec
    { name :: Text
    , vars :: [Source Text]
    , layout :: UTCTime -> Map Text Text -> Maybe Text
    , description :: Map Text Text -> Maybe Text
    }

metaOf :: TemplateEnum -> [TemplateSpec]
metaOf =
    \case
        Table ->
            [ TemplateSpec
                { name = "table"
                , vars =
                    [ Local "schema" "name of the schema"
                    , Local "table_name" "name of the table"
                    ]
                , layout = \_ m -> schemaBasedLayout m do
                    tableName <- Map.lookup "table_name" m
                    return [iii| table/#{tableName} |]
                , description = \vars -> do
                    tableName <- Map.lookup "table_name" vars
                    return [iii| create table #{tableName} |]
                }
            ]
        View ->
            [ TemplateSpec
                { name = "view"
                , vars =
                    [ Local "schema" "name of the schema"
                    , Local "view_name" "name of the view"
                    ]
                , layout = \_ m -> schemaBasedLayout m do
                    viewName <- Map.lookup "view_name" m
                    return [iii| view/#{viewName} |]
                , description = \vars -> do
                    viewName <- Map.lookup "view_name" vars
                    schema <- Map.lookup "schema" vars
                    return [iii| create view #{schema}.#{viewName} |]
                }
            ]
        Function ft ->
            [ TemplateSpec
                { name = case ft of StatechartTriggerFunction -> "statechart_trigger"; _ -> "function"
                , vars =
                    [ Local "schema" "name of the schema"
                    , Local "function_name" "name of the function"
                    ]
                , layout = \_ m -> schemaBasedLayout m do
                    functionName <- Map.lookup "function_name" m
                    return [iii| function/#{functionName} |]
                , description = \vars -> do
                    schema <- Map.lookup "schema" vars
                    functionName <- Map.lookup "function_name" vars
                    return [iii| create function #{schema}.#{functionName} |]
                }
            ]
        Trigger r ->
            [ TemplateSpec
                { name = case r of Row -> "trigger_row"; _ -> "trigger_statement"
                , vars =
                    [ Local "schema" "name of the schema"
                    , Local "trigger_name" "name of the trigger function"
                    , Local "trigger_table" "which table activates trigger"
                    ]
                , layout = \_ m -> schemaBasedLayout m do
                    triggerName <- Map.lookup "trigger_name" m
                    triggerTable <- Map.lookup "trigger_table" m
                    return [iii| trigger/#{triggerTable}/#{triggerName} |]
                , description = \vars -> do
                    schema <- Map.lookup "schema" vars
                    triggerName <- Map.lookup "trigger_name" vars
                    triggerTable <- Map.lookup "trigger_table" vars
                    return [iii| create trigger #{schema}.#{triggerName} on #{triggerTable} |]
                }
            ]
        Domain ->
            [ TemplateSpec
                { name = "domain"
                , vars =
                    [ Local "file_name" "name of the file to hold types (no extension)"
                    ]
                , layout = \_ m -> do
                    fileName <- Map.lookup "file_name" m
                    return [iii| type/#{fileName} |]
                , description = \vars -> do
                    fileName <- Map.lookup "file_name" vars
                    return [iii| create domain #{fileName} |]
                }
            ]
        Migration ->
            [ TemplateSpec
                { name = "migration"
                , vars =
                    [ Local "description" "describe migration"
                    ]
                , layout = timeBasedLayout
                , description = Map.lookup "description"
                }
            ]
        BulkUpdate ->
            [ TemplateSpec
                { name = "bulk_update"
                , vars =
                    [ Local "schema" "name of the schema"
                    , Local "table_name" "name of the table"
                    ]
                , layout = timeBasedLayout
                , description = \vars -> do
                    tableName <- Map.lookup "table_name" vars
                    schema <- Map.lookup "schema" vars
                    return [iii| bulk update #{schema}.#{tableName} |]
                }
            ]
        AddIndex ->
            [ TemplateSpec
                { name = "add_index"
                , vars =
                    [ Local "schema" "name of the schema"
                    , Local "table_name" "name of the table"
                    , Local "index_name" "name of the index"
                    ]
                , layout = \_ m -> schemaBasedLayout m do
                    tableName <- Map.lookup "table_name" m
                    indexName <- Map.lookup "index_name" m
                    return [iii| table/#{tableName}/index/#{indexName} |]
                , description = \vars -> do
                    tableName <- Map.lookup "table_name" vars
                    indexName <- Map.lookup "index_name" vars
                    schema <- Map.lookup "schema" vars
                    return [iii| add index #{schema}.#{tableName}.#{indexName} |]
                }
            ]
        AddNotNullConstraint ->
            [ TemplateSpec
                { name = "add_not_null_constraint"
                , vars =
                    [ Local "schema" "name of the schema"
                    , Local "table_name" "name of the table"
                    , Local "column_name" "name of the column"
                    ]
                , layout = timeBasedLayout
                , description = \vars -> do
                    columnName <- Map.lookup "column_name" vars
                    schema <- Map.lookup "schema" vars
                    tableName <- Map.lookup "table_name" vars
                    return [iii| add not null constraint on #{schema}.#{tableName}.#{columnName} |]
                }
            ]
        ValidateConstraint ->
            [ TemplateSpec
                { name = "validate_constraint"
                , vars =
                    [ Local "schema" "name of the schema"
                    , Local "table_name" "name of the table"
                    , Infer "constraint_name" "name of the constraint" \m -> do
                        columnName <- Map.lookup "column_name" m
                        return [iii| #{columnName}_cannot_be_null |]
                    ]
                , layout = timeBasedLayout
                , description = \vars -> do
                    constraint <- Map.lookup "constraint_name" vars
                    schema <- Map.lookup "schema" vars
                    tableName <- Map.lookup "table_name" vars
                    return [iii| validate constraint #{schema}.#{tableName}.#{constraint} |]
                }
            ]
        MakeColumnNotNull ->
            [ TemplateSpec
                { name = "make_column_not_null"
                , vars =
                    [ Local "schema" "name of the schema"
                    , Local "table_name" "name of the table"
                    , Local "column_name" "name of the column"
                    ]
                , layout = timeBasedLayout
                , description = \vars -> do
                    columnName <- Map.lookup "column_name" vars
                    schema <- Map.lookup "schema" vars
                    tableName <- Map.lookup "table_name" vars
                    return [iii| make column #{schema}.#{tableName}.#{columnName} not null |]
                }
            ]
        AddColumn ->
            [ TemplateSpec
                { name = "add_column"
                , vars =
                    [ Local "schema" "name of the schema"
                    , Local "table_name" "name of the table"
                    , Local "column_name" "name of the column"
                    ]
                , layout = timeBasedLayout
                , description = \vars -> do
                    columnName <- Map.lookup "column_name" vars
                    schema <- Map.lookup "schema" vars
                    tableName <- Map.lookup "table_name" vars
                    return [iii| add column #{schema}.#{tableName}.#{columnName} |]
                }
            ]
        RenameColumnStep1 ->
            [ TemplateSpec
                { name = "rename_column_step_1"
                , vars =
                    [ Local "schema" "name of the schema"
                    , Local "table_name" "name of the table"
                    , Local "old_column_name" "old name of the column"
                    , Local "new_column_name" "new name of the column"
                    , Local "type" "type of the column"
                    , Local "batch_column" "name of the column used for batch updates"
                    , Local "batch_column_type" "type of the column used for batch updates"
                    ]
                , layout = timeBasedLayout
                , description = \vars -> do
                    schema <- Map.lookup "schema" vars
                    tableName <- Map.lookup "table_name" vars
                    oldColumnName <- Map.lookup "old_column_name" vars
                    newColumnName <- Map.lookup "new_column_name" vars
                    return [iii| part one of renaming #{schema}.#{tableName}.#{oldColumnName} to #{newColumnName} |]
                }
            ]
        RenameColumnStep2 ->
            [ TemplateSpec
                { name = "rename_column_step_2"
                , vars =
                    [ Local "schema" "name of the schema"
                    , Local "table_name" "name of the table"
                    , Local "old_column_name" "old name of the column"
                    , Local "new_column_name" "new name of the column"
                    ]
                , layout = timeBasedLayout
                , description = \vars -> do
                    schema <- Map.lookup "schema" vars
                    tableName <- Map.lookup "table_name" vars
                    oldColumnName <- Map.lookup "old_column_name" vars
                    newColumnName <- Map.lookup "new_column_name" vars
                    return [iii| part two of renaming #{schema}.#{tableName}.#{oldColumnName} to #{newColumnName} |]
                }
            ]
        Compose NonNullableColumn ->
            concat
                [ metaOf AddColumn
                , metaOf AddNotNullConstraint
                , metaOf BulkUpdate
                , metaOf ValidateConstraint
                , metaOf MakeColumnNotNull
                ]
  where
    schemaBasedLayout :: Map Text Text -> Maybe Text -> Maybe Text
    schemaBasedLayout m original = do
        schema <- Map.lookup "schema" m
        name <- original
        return [iii| #{schema}/#{name} |]

    timeBasedLayout :: UTCTime -> Map Text Text -> Maybe Text
    timeBasedLayout t0 m = do
        let year :: String = Time.formatTime defaultTimeLocale "%Y" t0
        let month :: String = Time.formatTime defaultTimeLocale "%m" t0
        let dateFormated :: String = Time.formatTime defaultTimeLocale "%F" t0
        let descFormated :: Text = T.replace " " "-" (fromJust (Map.lookup "description" m))
        return [iii| migrations/#{year}/#{month}/#{dateFormated}-#{descFormated} |]
