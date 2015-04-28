import Control.Applicative
import Control.Monad (filterM, mapM)
import System.Directory (getDirectoryContents, getCurrentDirectory, doesDirectoryExist)
import System.Environment
import System.FilePath
import System.Info
import Data.List (intercalate)
import Data.String.Utils (endswith)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen prd str = case dropWhile prd str of
                      "" -> []
                      str' -> word : wordsWhen prd str''
                              where (word, str'') = break prd str'

extensions :: [String]
extensions = if os == "mingw32"
             then ["", ".exe", ".cmd", ".bat"]
             else [""]

getMatch :: String -> [String] -> Maybe String
getMatch spec pathItems = let pathParts = wordsWhen (==pathSeparator) in
    case filter (\p -> any (\n -> last (pathParts p) == n) (map (spec++) extensions)) pathItems of
      [] -> Nothing
      (m:ms) -> Just m -- Get the first match

main :: IO ()
main = do
  args <- getArgs
  path <- getEnv "PATH"
  let spec = head args
  let pathDirs = "." : wordsWhen (==searchPathSeparator) path
  validPathDirs <- filterM doesDirectoryExist pathDirs
  pathItems <- concat <$> mapM (\pd -> map (pd </>) <$> getDirectoryContents pd) validPathDirs
  let filteredPathItems = filter (\p -> p `notElem` [".", ".."]) $ pathItems
  let match = getMatch spec filteredPathItems
  progName <- getProgName
  case match of
    Nothing -> putStrLn (progName ++ ": no " ++ spec ++ " in (" ++ (intercalate ","  pathDirs) ++ ")")
    Just m -> putStrLn m
