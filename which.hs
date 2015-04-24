import Control.Monad (forM, mapM)
import System.Directory (getDirectoryContents)
import System.Environment
import System.FilePath
import Data.String.Utils (endswith)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen prd str = case dropWhile prd str of
                      "" -> []
                      str' -> word : wordsWhen prd str''
                              where (word, str'') = break prd str'

isExecutable :: String -> Bool
isExecutable x = any (\p -> endswith p x) [".exe", ".com", ".bat", ".cmd"]

getMatch :: String -> [String] -> Maybe String
getMatch spec pathItems = case filter (\p -> isExecutable p && endswith spec p) pathItems of
                            [] -> Nothing
                            (m:ms) -> Just m -- Get the first match

main :: IO ()
main = do
  args <- getArgs
  path <- getEnv "PATH"
  let spec = head args
  let pathDirs = wordsWhen (==searchPathSeparator) path
  pathItems <- mapM getDirectoryContents pathDirs
  let filteredPathItems = filter (\p -> p `notElem` [".", ".."]) $ concat pathItems
  let match = getMatch spec filteredPathItems
  progName <- getProgName
  case match of
    Nothing -> putStrLn (progName ++ ": no " ++ spec ++ " in (" ++ path ++ ")")
    Just m -> putStrLn m
