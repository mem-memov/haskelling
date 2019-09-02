import System.Directory

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

add :: Message -> Message -> Message

add (Reference question@(Question questionAnswer questionWords questionAnswers)) (Inquiry answer@(Answer Nothing answerWords [])) = 
  case (findAnswer answerWords questionAnswers) of
    (Just (Answer foundAnswerQuestion foundAnswerWords foundAnswerQuestions), otherAnswers) -> 
      Inquiry (
        Answer 
          (Just question)
          foundAnswerWords 
          foundAnswerQuestions
      )
    (Nothing,  allAnswers) -> 
      Reference (
        Question 
          questionAnswer
          questionWords 
          (answer : questionAnswers)
      )

add (Inquiry answer@(Answer answerQuestion answerWords answerQuestions)) (Reference question@(Question Nothing questionWords [])) =
  case (findQuestion questionWords answerQuestions) of
    (Just (Question foundQuestionAnswer foundQuestionWords foundQuestionAnswers), otherQuestions) -> 
      Reference (
        Question 
          (Just answer)
          foundQuestionWords 
          foundQuestionAnswers
      )
    (Nothing, allQuestions) -> 
      Inquiry (
        Answer 
          answerQuestion
          answerWords 
          (question : answerQuestions)
      )

add Silence reference@(Reference _) = reference
add Silence inquiry@(Inquiry _) = inquiry
add _ _ = Silence

main = do
  createFile "db.txt"
  contents <- readFile "db.txt"
  let remembered = recall contents
  putStrLn (showFocus remembered)
  input <- getLine
  let heard = comprehend input
  putStrLn (show heard)
  writeFile "db.txt" . show $ add remembered heard
  main

showQuestion :: Question -> String
showQuestion (Question _ questionWords _) = (getWords questionWords) ++ "\n"

showAnswer :: Answer -> String
showAnswer (Answer _ answerWords _) = (getWords answerWords) ++ "\n"

showQuestions :: [Question] -> String
showQuestions [] = "\n"
showQuestions (question : otherQuestions) = (showQuestion question) ++ "\n" ++ (showQuestions otherQuestions)

showAnswers :: [Answer] -> String
showAnswers [] = "\n"
showAnswers (answer : otherAnswers) = (showAnswer answer) ++ "\n" ++ (showAnswers otherAnswers)

showAnswerFocus :: Answer -> String
showAnswerFocus answer@(Answer Nothing _ answerQuestions) = (showAnswer answer) ++ (showQuestions answerQuestions) ++ "\n"
showAnswerFocus answer@(Answer (Just answerQuestion) _ answerQuestions) = (showQuestion answerQuestion) ++ "\n" ++ (showAnswer answer) ++ "\n" ++ (showQuestions answerQuestions) ++ "\n"

showQuestionFocus :: Question -> String
showQuestionFocus question@(Question Nothing _ questionAnswers) = (showQuestion question) ++ (showAnswers questionAnswers) ++ "\n"
showQuestionFocus question@(Question (Just questionAnswer) _ questionAnswers) = (showAnswer questionAnswer) ++ "\n" ++ (showQuestion question) ++ (showAnswers questionAnswers) ++ "\n"

showFocus :: Message -> String
showFocus Silence = "-"
showFocus (Reference question) = showQuestionFocus question
showFocus (Inquiry answer) = showAnswerFocus answer

comprehend :: String -> Message
comprehend ('?' : words) = Reference (Question Nothing (Words words) [])
comprehend words = Inquiry (Answer Nothing (Words words) [])


recall :: String -> Message
recall contents
  | contents == "" = Silence
  | otherwise = read contents

createFile :: String -> IO ()
createFile file = do
  fileExist <- doesFileExist file
  if not fileExist
    then writeFile file ""
    else return ()
 