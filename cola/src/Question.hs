module Question (hasWords, show) where

import MessageTypes
import Prelude hiding (show)

hasWords :: Question -> Words -> Bool
hasWords (Question _ questionWords _) words = words == questionWords

show :: Question -> String
show (Question _ questionWords _) = "?" ++ (getWords questionWords)
