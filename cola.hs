import System.Directory

newtype Words = Words { words :: String } deriving (Eq, Show, Read)

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
  putStrLn (show remembered)
  input <- getLine
  let heard = comprehend input
  putStrLn (show heard)
  writeFile "db.txt" . show $ add remembered heard
  main

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
 