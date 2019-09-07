module QuestionSpec (spec) where

import qualified Question (hasWords)
import MessageTypes

import Test.Hspec

spec :: Spec
spec = do
  describe "Question.hasWords" $ do

    it "has positiove result" $
      Question.hasWords 
        (Words "what")
        (Question Nothing (Words "what") []) 
      `shouldBe` True

    it "has negative result" $
      Question.hasWords 
        (Words "why") 
        (Question Nothing (Words "what") []) 
      `shouldBe` False
