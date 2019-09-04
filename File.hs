module File(
  createFile
) where

import System.Directory

createFile :: String -> IO ()
createFile file = do
  fileExist <- doesFileExist file
  if not fileExist
    then writeFile file ""
    else return ()