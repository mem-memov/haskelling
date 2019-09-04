module Message (
  add,
  showFocus,
  comprehend,
  recall
) where

newtype Words = Words { getWords :: String } deriving (Eq, Show, Read)

data Question = Question (Maybe Answer) Words [Answer] deriving (Show, Read)
data Answer = Answer (Maybe Question) Words [Question] deriving (Show, Read)
data Message = Silence | Reference Question | Inquiry Answer deriving (Show, Read)

findAnswer :: Words -> [Answer] -> (Maybe Answer, [Answer])
findAnswer words answers = 
  foldl 
    (\result answer@(Answer _ answerWords answerQuestions) -> 
      if words == answerWords 
        then (Just answer, snd result) 
        else (fst result, (answer : snd result))
    ) 
    (Nothing, []) 
    answers

findQuestion :: Words -> [Question] -> (Maybe Question, [Question])
findQuestion words questions = 
  foldl 
    (\result question@(Question _ questionWords questionAnswers) -> 
      if words == questionWords 
        then (Just question, snd result) 
        else (fst result, (question : snd result))
    ) 
    (Nothing, []) 
    questions

makeInquiry :: Maybe Question -> Words -> [Question] -> Message
makeInquiry answerQuestion answerWords answerQuestions = Inquiry (Answer answerQuestion answerWords answerQuestions)

makeReference :: Maybe Answer -> Words -> [Answer] -> Message
makeReference questionAnswer questionWords questionAnswers = Reference (Question questionAnswer questionWords questionAnswers)

add :: Message -> Message -> Message

add (Reference question@(Question questionAnswer questionWords questionAnswers)) (Inquiry answer@(Answer Nothing answerWords [])) = 
  case (findAnswer answerWords questionAnswers) of
    (Just (Answer foundAnswerQuestion foundAnswerWords foundAnswerQuestions), otherAnswers) -> 
      makeInquiry (Just question) foundAnswerWords foundAnswerQuestions
    (Nothing,  allAnswers) -> 
      makeReference questionAnswer questionWords (answer : questionAnswers)


add (Inquiry answer@(Answer answerQuestion answerWords answerQuestions)) (Reference question@(Question Nothing questionWords [])) =
  case (findQuestion questionWords answerQuestions) of
    (Just (Question foundQuestionAnswer foundQuestionWords foundQuestionAnswers), otherQuestions) -> 
      makeReference (Just answer) foundQuestionWords foundQuestionAnswers
    (Nothing, allQuestions) -> 
      makeInquiry answerQuestion answerWords (question : answerQuestions)

add message Silence = message 
add Silence reference@(Reference _) = reference
add Silence inquiry@(Inquiry _) = inquiry
add _ _ = Silence


showQuestion :: Question -> String
showQuestion (Question _ questionWords _) = "?" ++ (getWords questionWords)

showAnswer :: Answer -> String
showAnswer (Answer _ answerWords _) = (getWords answerWords)

showQuestions :: [Question] -> String
showQuestions [] = ""
showQuestions (question : otherQuestions) = "  " ++ (showQuestion question) ++ "\n" ++ (showQuestions otherQuestions)

showAnswers :: [Answer] -> String
showAnswers [] = ""
showAnswers (answer : otherAnswers) = "  " ++ (showAnswer answer) ++ "\n" ++ (showAnswers otherAnswers)

showAnswerFocus :: Answer -> String
showAnswerFocus answer@(Answer Nothing _ answerQuestions) = " -> " ++ (showAnswer answer) ++ "\n" ++ (showQuestions answerQuestions)
showAnswerFocus answer@(Answer (Just answerQuestion) _ answerQuestions) = (showQuestion answerQuestion) ++ "\n -> " ++ (showAnswer answer) ++ "\n" ++ (showQuestions answerQuestions)

showQuestionFocus :: Question -> String
showQuestionFocus question@(Question Nothing _ questionAnswers) = " -> " ++ (showQuestion question) ++ "\n" ++ (showAnswers questionAnswers)
showQuestionFocus question@(Question (Just questionAnswer) _ questionAnswers) = (showAnswer questionAnswer) ++ "\n -> " ++ (showQuestion question) ++ "\n" ++ (showAnswers questionAnswers)

showFocus :: Message -> String
showFocus Silence = "\n-"
showFocus (Reference question) = "\n" ++ showQuestionFocus question
showFocus (Inquiry answer) = "\n" ++ showAnswerFocus answer

comprehend :: String -> Message
comprehend "" = Silence
comprehend ('?' : words) = makeReference Nothing (Words words) []
comprehend words = makeInquiry Nothing (Words words) []

recall :: String -> Message
recall contents
  | contents == "" = Silence
  | otherwise = read contents
