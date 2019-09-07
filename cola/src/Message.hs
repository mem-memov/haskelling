module Message (
  comprehend,
  recall
) where

import MessageTypes
import qualified Reference (fromWords)
import qualified Inquiry (fromWords)

comprehend :: String -> Message
comprehend "" = Silence
comprehend ('?' : words) = Reference.fromWords (Words words)
comprehend words = Inquiry.fromWords (Words words)

recall :: String -> Message
recall contents
  | contents == "" = Silence
  | otherwise = read contents
