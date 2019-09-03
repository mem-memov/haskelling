import System.Directory
import Message

main = do
  createFile "db.txt"
  contents <- readFile "db.txt"
  let remembered = recall contents
  putStr (showFocus remembered)
  input <- getLine
  let heard = comprehend input
  let understood = add remembered heard
  writeFile "db.txt" . show $ understood
  main

createFile :: String -> IO ()
createFile file = do
  fileExist <- doesFileExist file
  if not fileExist
    then writeFile file ""
    else return ()
 