module MessageTypes(
  Words(..),
  Question(..),
  Answer(..),
  Message(..),
  Pick(..)
) where

newtype Words = Words { getWords :: String } deriving (Eq, Show, Read)
data Question = Question (Maybe Answer) Words [Answer] deriving (Eq, Show, Read)
data Answer = Answer (Maybe Question) Words [Question] deriving (Eq, Show, Read)
data Message = Silence | Reference Question | Inquiry Answer deriving (Eq, Show, Read)
data Pick a = Pick (Maybe a) [a] deriving (Eq, Show)
