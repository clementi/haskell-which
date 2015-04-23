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

getMatch :: String -> [String] -> Maybe String
getMatch spec pathItems = case matches of
                            [] -> Nothing
                            [something] -> Just something
                            _ -> Nothing
    where matches = filter (endswith spec) pathItems

main :: IO ()
main = do
  args <- getArgs
  path <- getEnv "PATH"
  let spec = head args
  let pathDirs = wordsWhen (==searchPathSeparator) path
  pathItems <- mapM getDirectoryContents pathDirs
  let filteredPathItems = filter (\p -> p `notElem` [".", ".."]) $ concat pathItems
--  putStrLn $ unlines filteredPathItems
  let match = getMatch spec filteredPathItems
  case match of
    Nothing -> putStrLn "Not found."
    Just m -> putStrLn m
