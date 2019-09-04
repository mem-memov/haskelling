import File
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
 