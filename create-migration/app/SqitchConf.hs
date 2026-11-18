{- | Just enough of @sqitch.conf@ (git-config syntax) to find target names and
each target's @top_dir@, for tab completion.
-}
module SqitchConf (
    readSqitchConf,
    listTargets,
    resolveTopDir,
    SqitchConf,
) where

import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isSuffixOf, stripPrefix)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import System.Directory (doesFileExist)

-- | Section name (e.g. @core@ or @target "prod"@) to its key/value pairs.
type SqitchConf = Map String (Map String String)

-- | Read @sqitch.conf@ from the current directory; empty when it is missing.
readSqitchConf :: IO SqitchConf
readSqitchConf = do
    exists <- doesFileExist "sqitch.conf"
    if exists
        then parse <$> readFile "sqitch.conf"
        else pure Map.empty

parse :: String -> SqitchConf
parse = go "" Map.empty . lines
  where
    go _ acc [] = acc
    go section acc (l : ls) =
        case trim l of
            "" -> go section acc ls
            c : _ | c `elem` ("#;" :: String) -> go section acc ls
            '[' : rest -> go (normaliseSection (takeWhile (/= ']') rest)) acc ls
            line ->
                let (k, v) = break (== '=') line
                    value = unquote (trim (drop 1 v))
                 in go section (Map.insertWith Map.union section (Map.singleton (trim k) value) acc) ls

    -- @[target "prod"]@ and @[target  "prod"]@ are the same section.
    normaliseSection s =
        let (name, sub) = break isSpace (trim s)
         in if null sub then name else name <> " " <> trim sub

    unquote ('"' : s) | not (null s), last s == '"' = init s
    unquote s = s

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Names of the @[target "..."]@ sections.
listTargets :: SqitchConf -> [String]
listTargets =
    mapMaybe (\k -> stripPrefix "target \"" k >>= stripSuffix "\"") . Map.keys
  where
    stripSuffix suf s
        | suf `isSuffixOf` s = Just (take (length s - length suf) s)
        | otherwise = Nothing

{- | The directory holding @deploy/@, @revert/@ and @verify/@ for a target:
the target's @top_dir@, else @[core] top_dir@, else the current directory
(sqitch's own default).
-}
resolveTopDir :: SqitchConf -> Maybe String -> FilePath
resolveTopDir conf target =
    fromMaybe "." (targetTopDir <|> coreTopDir)
  where
    lookupIn section = Map.lookup "top_dir" =<< Map.lookup section conf
    targetTopDir = target >>= \t -> lookupIn ("target \"" <> t <> "\"")
    coreTopDir = lookupIn "core"
