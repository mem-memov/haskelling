module Question (
  questionHasWords,
  showQuestion
) where

import MessageTypes

questionHasWords :: Question -> Words -> Bool
questionHasWords (Question _ questionWords _) words = words == questionWords

showQuestion :: Question -> String
showQuestion (Question _ questionWords _) = "?" ++ (getWords questionWords)
