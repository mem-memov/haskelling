
module QuestionSpec (spec) where

import Question
import MessageTypes

import Test.Hspec

spec :: Spec
spec = do
  describe "questionHasWords" $ do

    it "has positiove result" $
      questionHasWords (Question Nothing (Words "what") []) (Words "what") `shouldBe` True

    it "has negative result" $
      questionHasWords (Question Nothing (Words "what") []) (Words "why") `shouldBe` False
