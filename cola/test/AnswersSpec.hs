module AnswersSpec (spec) where

import MessageTypes
import Answer
import qualified Answers (find)

import Test.Hspec

spec :: Spec
spec = do
  describe "Answer.find" $ do

    it "finds one answer in a list of one answer" $
      Answers.find  
        (Words "dog") 
        [(Answer Nothing (Words "dog") [])] 
      `shouldBe` 
      (
        Just (Answer Nothing (Words "dog") []), 
        []
      )

    it "finds no answer in an empty list" $
      Answers.find  
        (Words "dog") 
        [] 
      `shouldBe` 
      (
        Nothing, 
        []
      )

    it "finds one answer in a list of three answers" $
      Answers.find  
        (Words "dog") 
        [
          (Answer Nothing (Words "cat") []),
          (Answer Nothing (Words "dog") []),
          (Answer Nothing (Words "pig") [])
        ] 
      `shouldBe` 
      (
        Just (Answer Nothing (Words "dog") []),
        [
          (Answer Nothing (Words "cat") []),
          (Answer Nothing (Words "pig") [])
        ]
      )
  
