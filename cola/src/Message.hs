module Message (
  makeInquiry,
  makeReference,
  comprehend,
  recall
) where

import MessageTypes

makeInquiry :: Maybe Question -> Words -> [Question] -> Message
makeInquiry answerQuestion answerWords answerQuestions = Inquiry (Answer answerQuestion answerWords answerQuestions)

makeReference :: Maybe Answer -> Words -> [Answer] -> Message
makeReference questionAnswer questionWords questionAnswers = Reference (Question questionAnswer questionWords questionAnswers)

comprehend :: String -> Message
comprehend "" = Silence
comprehend ('?' : words) = makeReference Nothing (Words words) []
comprehend words = makeInquiry Nothing (Words words) []

recall :: String -> Message
recall contents
  | contents == "" = Silence
  | otherwise = read contents
