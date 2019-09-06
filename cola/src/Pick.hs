module Pick (make, hold, append, hasContent) where

import MessageTypes

make :: Pick a
make = Pick Nothing []

hold :: Pick a -> a -> Pick a
hold (Pick _ items) newItem = Pick (Just newItem) items

append :: Pick a -> a -> Pick a
append (Pick item items) newItem = Pick item (newItem : items)

hasContent :: Pick a -> Bool
hasContent (Pick Nothing _) = False
hasContent _ = True