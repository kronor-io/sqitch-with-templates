{-# LANGUAGE ApplicativeDo #-}

{- HLINT ignore "Use <$>" -}

module ProgramOptions (Command (..), commandParser, resolveCommand) where

import Completion (dbCompleter, schemaCompleter, tableCompleter)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Options.Applicative
import Sqitch.Template

data Command = Command
    { db :: Maybe String
    -- ^ sqitch target; 'Nothing' leaves the choice to sqitch's default
    , templateDir :: Maybe FilePath
    -- ^ overrides the templates bundled with the executable
    , migration :: MigrationCommand
    }

newtype Schema = Schema (Maybe Text)
newtype TableName = TableName (Maybe Text)
newtype ViewName = ViewName (Maybe Text)
newtype TriggerName = TriggerName (Maybe Text)
newtype TriggerTable = TriggerTable (Maybe Text)
newtype FunctionName = FunctionName (Maybe Text)
newtype FileName = FileName (Maybe Text)
newtype Description = Description (Maybe Text)
newtype IndexName = IndexName (Maybe Text)
newtype Column = Column (Maybe Text)
newtype ColumnName = ColumnName (Maybe Text)
newtype ConstraintName = ConstraintName (Maybe Text)
newtype TargetSchema = TargetSchema (Maybe Text)
newtype TargetTableName = TargetTableName (Maybe Text)
newtype TargetTableColumn = TargetTableColumn (Maybe Text)
newtype OldColumnName = OldColumnName (Maybe Text)
newtype NewColumnName = NewColumnName (Maybe Text)
newtype ColumnType = ColumnType (Maybe Text)
newtype BatchColumn = BatchColumn (Maybe Text)
newtype BatchColumnType = BatchColumnType (Maybe Text)

data MigrationCommand
    = TableCmd Schema TableName
    | ViewCmd Schema ViewName
    | TriggerRowCmd Schema TriggerName TriggerTable
    | TriggerStatementCmd Schema TriggerName TriggerTable
    | FunctionGeneralCmd Schema FunctionName
    | FunctionStatechartCmd Schema FunctionName
    | DomainCmd FileName
    | MigrationCmd Description
    | BulkUpdateCmd Schema TableName
    | AddIndexCmd Schema TableName IndexName
    | AddIndexToPartitionedTableCmd Schema TableName Column
    | AddForeignKeyFromPartitionedTableCmd Schema TableName Column TargetSchema TargetTableName TargetTableColumn
    | ValidateConstraintCmd Schema TableName ColumnName ConstraintName
    | AddColumnCmd Schema TableName ColumnName
    | NonNullableColumnCmd Schema TableName ColumnName
    | RenameColumnStep1Cmd Schema TableName OldColumnName NewColumnName ColumnType BatchColumn BatchColumnType
    | RenameColumnStep2Cmd Schema TableName OldColumnName NewColumnName

resolveCommand :: MigrationCommand -> (TemplateEnum, Map Text Text)
resolveCommand = \case
    TableCmd (Schema s) (TableName t) ->
        (Table, collect [("schema", s), ("table_name", t)])
    ViewCmd (Schema s) (ViewName v) ->
        (View, collect [("schema", s), ("view_name", v)])
    TriggerRowCmd (Schema s) (TriggerName n) (TriggerTable t) ->
        (Trigger Row, collect [("schema", s), ("trigger_name", n), ("trigger_table", t)])
    TriggerStatementCmd (Schema s) (TriggerName n) (TriggerTable t) ->
        (Trigger Statement, collect [("schema", s), ("trigger_name", n), ("trigger_table", t)])
    FunctionGeneralCmd (Schema s) (FunctionName n) ->
        (Function GeneralFunction, collect [("schema", s), ("function_name", n)])
    FunctionStatechartCmd (Schema s) (FunctionName n) ->
        (Function StatechartTriggerFunction, collect [("schema", s), ("function_name", n)])
    DomainCmd (FileName f) ->
        (Domain, collect [("file_name", f)])
    MigrationCmd (Description d) ->
        (Migration, collect [("description", d)])
    BulkUpdateCmd (Schema s) (TableName t) ->
        (BulkUpdate, collect [("schema", s), ("table_name", t)])
    AddIndexCmd (Schema s) (TableName t) (IndexName i) ->
        (AddIndex, collect [("schema", s), ("table_name", t), ("index_name", i)])
    AddIndexToPartitionedTableCmd (Schema s) (TableName t) (Column c) ->
        (AddIndexToPartitionedTable, collect [("schema", s), ("table_name", t), ("column", c)])
    AddForeignKeyFromPartitionedTableCmd (Schema s) (TableName t) (Column c) (TargetSchema ts) (TargetTableName tt) (TargetTableColumn tc) ->
        ( AddForeignKeyFromPartitionedTable
        , collect
            [ ("schema", s)
            , ("table_name", t)
            , ("column", c)
            , ("target_schema", ts)
            , ("target_table_name", tt)
            , ("target_table_column", tc)
            ]
        )
    ValidateConstraintCmd (Schema s) (TableName t) (ColumnName cn) (ConstraintName cstr) ->
        (ValidateConstraint, collect [("schema", s), ("table_name", t), ("column_name", cn), ("constraint_name", cstr)])
    AddColumnCmd (Schema s) (TableName t) (ColumnName c) ->
        (AddColumn, collect [("schema", s), ("table_name", t), ("column_name", c)])
    NonNullableColumnCmd (Schema s) (TableName t) (ColumnName c) ->
        (Compose NonNullableColumn, collect [("schema", s), ("table_name", t), ("column_name", c)])
    RenameColumnStep1Cmd (Schema s) (TableName t) (OldColumnName old) (NewColumnName new_) (ColumnType ty) (BatchColumn bc) (BatchColumnType bct) ->
        ( RenameColumnStep1
        , collect
            [ ("schema", s)
            , ("table_name", t)
            , ("old_column_name", old)
            , ("new_column_name", new_)
            , ("type", ty)
            , ("batch_column", bc)
            , ("batch_column_type", bct)
            ]
        )
    RenameColumnStep2Cmd (Schema s) (TableName t) (OldColumnName old) (NewColumnName new_) ->
        ( RenameColumnStep2
        , collect
            [ ("schema", s)
            , ("table_name", t)
            , ("old_column_name", old)
            , ("new_column_name", new_)
            ]
        )
  where
    collect :: [(Text, Maybe Text)] -> Map Text Text
    collect = Map.fromList . mapMaybe (\(k, mv) -> (k,) <$> mv)

commandParser :: Parser Command
commandParser = do
    db <-
        optional
            ( strOption
                ( long "db"
                    <> metavar "TARGET"
                    <> completer dbCompleter
                    <> help "sqitch target to add the change to (default: sqitch's default target)"
                )
            )
    templateDir <-
        optional
            ( strOption
                ( long "template-directory"
                    <> metavar "DIR"
                    <> action "directory"
                    <> help "Use templates from DIR (with deploy/, revert/ and verify/) instead of the bundled ones"
                )
            )
    migration <- migrationParser
    pure Command{db, templateDir, migration}

migrationParser :: Parser MigrationCommand
migrationParser =
    subparser $
        mconcat
            [ command "table" (info (tableP <**> helper) (progDesc "Create a new table"))
            , command "view" (info (viewP <**> helper) (progDesc "Create a new view"))
            , command "trigger" (info (triggerP <**> helper) (progDesc "Create a new trigger"))
            , command "function" (info (functionP <**> helper) (progDesc "Create a new function"))
            , command "domain" (info (domainP <**> helper) (progDesc "Create a new domain type"))
            , command "migration" (info (migrationOptP <**> helper) (progDesc "Create a generic migration"))
            , command "bulk-update" (info (bulkUpdateP <**> helper) (progDesc "Bulk update a table"))
            , command "add-index" (info (addIndexP <**> helper) (progDesc "Add a new index"))
            , command "add-index-partitioned" (info (addIndexPartP <**> helper) (progDesc "Add index to partitioned table"))
            , command "add-fk-partitioned" (info (addFkPartP <**> helper) (progDesc "Add FK from partitioned table"))
            , command "validate-constraint" (info (validateConstraintP <**> helper) (progDesc "Validate a constraint"))
            , command "add-column" (info (addColumnP <**> helper) (progDesc "Add a nullable column"))
            , command "non-nullable-column" (info (nonNullableColumnP <**> helper) (progDesc "Add a non-nullable column"))
            , command "rename-column-step-1" (info (renameStep1P <**> helper) (progDesc "Rename a column, first deployment"))
            , command "rename-column-step-2" (info (renameStep2P <**> helper) (progDesc "Rename a column, second deployment"))
            ]

-- Helpers for common options

schemaOpt :: Parser Schema
schemaOpt = Schema <$> optional (strOption (long "schema" <> short 's' <> metavar "SCHEMA" <> completer schemaCompleter <> help "Name of the schema"))

tableNameOpt :: Parser TableName
tableNameOpt = TableName <$> optional (strOption (long "table-name" <> short 't' <> metavar "TABLE" <> completer tableCompleter <> help "Name of the table"))

columnNameOpt :: Parser ColumnName
columnNameOpt = ColumnName <$> optional (strOption (long "column-name" <> short 'c' <> metavar "COLUMN" <> help "Name of the column"))

-- Subcommand parsers

tableP :: Parser MigrationCommand
tableP = do
    schema <- schemaOpt
    tableName <- tableNameOpt
    pure $ TableCmd schema tableName

viewP :: Parser MigrationCommand
viewP = do
    schema <- schemaOpt
    viewName <- ViewName <$> optional (strOption (long "view-name" <> short 'v' <> metavar "VIEW" <> help "Name of the view"))
    pure $ ViewCmd schema viewName

triggerP :: Parser MigrationCommand
triggerP =
    subparser $
        mconcat
            [ command "row" (info (triggerRowP <**> helper) (progDesc "Row-level trigger"))
            , command "statement" (info (triggerStatementP <**> helper) (progDesc "Statement-level trigger"))
            ]

triggerOpts :: (Schema -> TriggerName -> TriggerTable -> a) -> Parser a
triggerOpts f = do
    schema <- schemaOpt
    triggerName <- TriggerName <$> optional (strOption (long "trigger-name" <> short 'n' <> metavar "NAME" <> help "Name of the trigger function"))
    triggerTable <- TriggerTable <$> optional (strOption (long "trigger-table" <> short 't' <> metavar "TABLE" <> completer tableCompleter <> help "Which table activates the trigger"))
    pure $ f schema triggerName triggerTable

triggerRowP :: Parser MigrationCommand
triggerRowP = triggerOpts TriggerRowCmd

triggerStatementP :: Parser MigrationCommand
triggerStatementP = triggerOpts TriggerStatementCmd

functionP :: Parser MigrationCommand
functionP =
    subparser $
        mconcat
            [ command "general" (info (functionGeneralP <**> helper) (progDesc "General function"))
            , command "statechart" (info (functionStatechartP <**> helper) (progDesc "Statechart trigger function"))
            ]

functionOpts :: (Schema -> FunctionName -> a) -> Parser a
functionOpts f = do
    schema <- schemaOpt
    functionName <- FunctionName <$> optional (strOption (long "function-name" <> short 'n' <> metavar "NAME" <> help "Name of the function"))
    pure $ f schema functionName

functionGeneralP :: Parser MigrationCommand
functionGeneralP = functionOpts FunctionGeneralCmd

functionStatechartP :: Parser MigrationCommand
functionStatechartP = functionOpts FunctionStatechartCmd

domainP :: Parser MigrationCommand
domainP = do
    fileName <- FileName <$> optional (strOption (long "file-name" <> short 'f' <> metavar "FILE" <> help "Name of the file to hold types (no extension)"))
    pure $ DomainCmd fileName

migrationOptP :: Parser MigrationCommand
migrationOptP = do
    description <- Description <$> optional (strOption (long "description" <> short 'd' <> metavar "DESC" <> help "Describe the migration"))
    pure $ MigrationCmd description

bulkUpdateP :: Parser MigrationCommand
bulkUpdateP = do
    schema <- schemaOpt
    tableName <- tableNameOpt
    pure $ BulkUpdateCmd schema tableName

addIndexP :: Parser MigrationCommand
addIndexP = do
    schema <- schemaOpt
    tableName <- tableNameOpt
    indexName <- IndexName <$> optional (strOption (long "index-name" <> short 'i' <> metavar "INDEX" <> help "Name of the index"))
    pure $ AddIndexCmd schema tableName indexName

addIndexPartP :: Parser MigrationCommand
addIndexPartP = do
    schema <- schemaOpt
    tableName <- tableNameOpt
    column <- Column <$> optional (strOption (long "column" <> short 'c' <> metavar "COLUMN" <> help "Column to index on"))
    pure $ AddIndexToPartitionedTableCmd schema tableName column

addFkPartP :: Parser MigrationCommand
addFkPartP = do
    schema <- schemaOpt
    tableName <- tableNameOpt
    column <- Column <$> optional (strOption (long "column" <> short 'c' <> metavar "COLUMN" <> help "Column that will reference another table"))
    targetSchema <- TargetSchema <$> optional (strOption (long "target-schema" <> metavar "SCHEMA" <> help "Schema of the referenced table"))
    targetTableName <- TargetTableName <$> optional (strOption (long "target-table-name" <> metavar "TABLE" <> help "Name of the referenced table"))
    targetTableColumn <- TargetTableColumn <$> optional (strOption (long "target-table-column" <> metavar "COLUMN" <> help "Column of the referenced table"))
    pure $ AddForeignKeyFromPartitionedTableCmd schema tableName column targetSchema targetTableName targetTableColumn

validateConstraintP :: Parser MigrationCommand
validateConstraintP = do
    schema <- schemaOpt
    tableName <- tableNameOpt
    columnName <- columnNameOpt
    constraintName <- ConstraintName <$> optional (strOption (long "constraint-name" <> metavar "CONSTRAINT" <> help "Name of the constraint (inferred from column name if omitted)"))
    pure $ ValidateConstraintCmd schema tableName columnName constraintName

addColumnP :: Parser MigrationCommand
addColumnP = do
    schema <- schemaOpt
    tableName <- tableNameOpt
    columnName <- columnNameOpt
    pure $ AddColumnCmd schema tableName columnName

nonNullableColumnP :: Parser MigrationCommand
nonNullableColumnP = do
    schema <- schemaOpt
    tableName <- tableNameOpt
    columnName <- columnNameOpt
    pure $ NonNullableColumnCmd schema tableName columnName

renameStep1P :: Parser MigrationCommand
renameStep1P = do
    schema <- schemaOpt
    tableName <- tableNameOpt
    oldColumnName <- OldColumnName <$> optional (strOption (long "old-column-name" <> metavar "NAME" <> help "Old name of the column"))
    newColumnName <- NewColumnName <$> optional (strOption (long "new-column-name" <> metavar "NAME" <> help "New name of the column"))
    columnType <- ColumnType <$> optional (strOption (long "type" <> metavar "TYPE" <> help "Type of the column"))
    batchColumn <- BatchColumn <$> optional (strOption (long "batch-column" <> metavar "COLUMN" <> help "Column used for batch updates"))
    batchColumnType <- BatchColumnType <$> optional (strOption (long "batch-column-type" <> metavar "TYPE" <> help "Type of the batch column"))
    pure $ RenameColumnStep1Cmd schema tableName oldColumnName newColumnName columnType batchColumn batchColumnType

renameStep2P :: Parser MigrationCommand
renameStep2P = do
    schema <- schemaOpt
    tableName <- tableNameOpt
    oldColumnName <- OldColumnName <$> optional (strOption (long "old-column-name" <> metavar "NAME" <> help "Old name of the column"))
    newColumnName <- NewColumnName <$> optional (strOption (long "new-column-name" <> metavar "NAME" <> help "New name of the column"))
    pure $ RenameColumnStep2Cmd schema tableName oldColumnName newColumnName
