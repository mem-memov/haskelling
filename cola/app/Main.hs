module Main where

import qualified File (createFile)
import qualified Message (recall, comprehend)
import qualified Messages (add)
import qualified Focus (show)

main = do
  File.createFile "db.txt"
  contents <- readFile "db.txt"
  let remembered = Message.recall contents
  putStr (Focus.show remembered)
  input <- getLine
  let heard = Message.comprehend input
  let understood = Messages.add remembered heard
  writeFile "db.txt" . show $ understood
  main
