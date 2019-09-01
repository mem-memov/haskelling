import System.Directory

newtype Words = Words { words :: String } deriving (Eq, Show, Read)

data Question = Question Words [Answer] deriving (Show, Read)
data Answer = Answer Words [Question] deriving (Show, Read)
data Message = Silence | Reference Question | Inquiry Answer deriving (Show, Read)

findAnswer :: Words -> [Answer] -> (Maybe Answer, [Answer])
findAnswer words answers = 
  foldl 
    (\result answer@(Answer answerWords answerQuestions) -> 
      if words == answerWords 
        then (Just answer, snd result) 
        else (fst result, (answer : snd result))
    ) 
    (Nothing, []) 
    answers

findQuestion :: Words -> [Question] -> (Maybe Question, [Question])
findQuestion words questions = 
  foldl 
    (\result question@(Question questionWords questionAnswers) -> 
      if words == questionWords 
        then (Just question, snd result) 
        else (fst result, (question : snd result))
    ) 
    (Nothing, []) 
    questions

add :: Message -> Message -> Message

add (Reference question@(Question questionWords questionAnswers)) (Inquiry answer@(Answer answerWords [])) = 
  case (findAnswer answerWords questionAnswers) of
    (Just (Answer foundAnswerWords foundAnswerQuestions), otherAnswers) -> Inquiry (Answer foundAnswerWords (question : foundAnswerQuestions))
    (Nothing, allAnswers) -> Reference (Question questionWords (answer : questionAnswers))

add (Inquiry answer@(Answer answerWords answerQuestions)) (Reference question@(Question questionWords [])) =
  case (findQuestion questionWords answerQuestions) of
    (Just (Question foundQuestionWords foundQuestionAnswers), otherQuestions) -> Reference (Question foundQuestionWords (answer : foundQuestionAnswers))
    (Nothing, allQuestions) -> Inquiry (Answer answerWords (question : answerQuestions))

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
comprehend ('?' : words) = Reference (Question (Words words) [])
comprehend words = Inquiry (Answer (Words words) [])


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
 