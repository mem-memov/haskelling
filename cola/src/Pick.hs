module Pick (make, hasContent, collect) where

import MessageTypes

make :: Pick a
make = Pick Nothing []

hasContent :: Pick a -> Bool
hasContent (Pick Nothing _) = False
hasContent _ = True

collect :: Pick a -> [a] -> (a -> Bool) -> Pick a
collect pick [] _ = pick
collect (Pick x xs) (y : ys) check 
  | check (y) = collect (Pick (Just y) xs) ys check
  | otherwise = collect (Pick x (y : xs)) ys check