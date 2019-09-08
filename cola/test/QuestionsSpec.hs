module QuestionsSpec (spec) where

import AnswerQuestionType
import WordsType
import PickType
import qualified Questions (find)

import Test.Hspec

spec :: Spec
spec = do
  describe "Question.find" $ do

    it "finds one question in a list of one question" $
      Questions.find  
        (Words "why") 
        [(Question Nothing (Words "why") [])] 
      `shouldBe` 
      Pick
        (Just (Question Nothing (Words "why") [])) 
        []

    it "finds no question in an empty list" $
      Questions.find  
        (Words "why") 
        [] 
      `shouldBe` 
      Pick
        Nothing
        []

    it "finds one question in a list of three questions" $
      Questions.find  
        (Words "why") 
        [
          (Question Nothing (Words "what") []),
          (Question Nothing (Words "why") []),
          (Question Nothing (Words "where") [])
        ] 
      `shouldBe` 
      Pick
        (Just (Question Nothing (Words "why") []))
        [
          (Question Nothing (Words "where") []),
          (Question Nothing (Words "what") [])
        ]
