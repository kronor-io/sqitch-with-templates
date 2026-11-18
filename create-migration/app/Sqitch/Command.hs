-- | Turns a filled-in template spec into a @sqitch add@ invocation.
module Sqitch.Command (SqitchAdd (..), sqitchAddArgs, runSqitchAdd) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (findExecutable)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Process (proc, waitForProcess, withCreateProcess)

data SqitchAdd = SqitchAdd
    { changePath :: Text
    -- ^ change name, which is also its path under deploy/, revert/ and verify/
    , target :: Maybe String
    , template :: Text
    , note :: Text
    , variables :: Map Text Text
    , templateDir :: FilePath
    }

{- | Arguments for @sqitch@. Passed as an argument vector, never through a
shell, so quotes and spaces in values arrive intact.
-}
sqitchAddArgs :: SqitchAdd -> [String]
sqitchAddArgs a =
    ["add", T.unpack a.changePath]
        <> maybe [] pure a.target
        <> ["--template", T.unpack a.template]
        <> ["--template-directory", a.templateDir]
        <> ["-n", T.unpack (a.template <> ": " <> a.note)]
        <> concat
            [ ["-s", T.unpack (k <> "=" <> v)]
            | (k, v) <- Map.toList a.variables
            , -- already in the note
            k /= "description"
            ]

runSqitchAdd :: SqitchAdd -> IO ()
runSqitchAdd a = do
    sqitch <- findExecutable "sqitch"
    case sqitch of
        Nothing -> do
            hPutStrLn stderr "create-migration: no sqitch executable on PATH"
            exitWith (ExitFailure 127)
        Just exe -> do
            code <- withCreateProcess (proc exe (sqitchAddArgs a)) \_ _ _ ph -> waitForProcess ph
            case code of
                ExitSuccess -> pure ()
                ExitFailure _ -> do
                    hPutStrLn stderr ("create-migration: sqitch add failed for " <> T.unpack a.changePath)
                    exitWith code
