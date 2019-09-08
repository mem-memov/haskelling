module PickType (Pick(..)) where

data Pick a = Pick (Maybe a) [a] deriving (Eq, Show)