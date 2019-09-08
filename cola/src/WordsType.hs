module WordsType (Words(..)) where

newtype Words = Words { getWords :: String } deriving (Eq, Show, Read)