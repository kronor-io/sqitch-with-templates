{-# LANGUAGE LambdaCase #-}

module ProgramOptions (Command (..), commandParser, Db (..), targetName) where

import Options.Applicative
import Sqitch.Template

newtype Db = Db {targetName :: String}

data Command
    = Create Db TemplateEnum

commandParser :: Parser Command
commandParser =
    Create
        <$> dbParser
        <*> ( tableParser
                <|> viewParser
                <|> functionParser
                <|> triggerParser
                <|> domainParser
                <|> migrationParser
                <|> bulkUpdateParser
                <|> addIndexParser
                <|> validateConstraintParser
                <|> nonNullableColumnParser
                <|> addColumnParser
                <|> renameColumnStep1Parser
                <|> renameColumnStep2Parser
            )
  where
    tableParser =
        flag'
            Table
            ( long "table"
                <> help "create a new table"
            )
    viewParser =
        flag'
            View
            ( long "view"
                <> help "create a new view"
            )
    functionParser =
        flag'
            (Function GeneralFunction)
            (long "function" <> help "create a new function")
    triggerParser =
        flag'
            (Trigger Row)
            (long "trigger" <> help "create a new trigger")
    domainParser =
        flag'
            Domain
            (long "domain" <> help "create a new domain")
    migrationParser =
        flag'
            Migration
            (long "migration" <> help "create a new migration")
    bulkUpdateParser =
        flag'
            BulkUpdate
            (long "bulk-update" <> help "bulk update a table")
    addIndexParser =
        flag'
            AddIndex
            (long "add-index" <> help "add a new index")
    validateConstraintParser =
        flag'
            ValidateConstraint
            (long "validate-constraint" <> help "validate a constraint")
    addColumnParser =
        flag'
            AddColumn
            (long "add-column" <> help "add a nullable column")
    nonNullableColumnParser =
        flag'
            (Compose NonNullableColumn)
            (long "non-nullable-column" <> help "add a new non-nullable column")
    renameColumnStep1Parser =
        flag'
            RenameColumnStep1
            (long "rename-column-step-1" <> help "rename a column, first deployment")
    renameColumnStep2Parser =
        flag'
            RenameColumnStep2
            (long "rename-column-step-2" <> help "rename a column, second deployment")

dbParser :: Parser Db
dbParser =
    argument
        ( Db <$> str
        )
        (metavar "DB" <> help "specify a database to run migration in")
