
module AnswerSpec (spec) where

import Answer
import MessageTypes

import Test.Hspec

spec :: Spec
spec = do
  describe "answerHasWords" $ do

    it "has positiove result" $
      answerHasWords (Answer Nothing (Words "dog") []) (Words "dog") `shouldBe` True

    it "has negative result" $
      answerHasWords (Answer Nothing (Words "dog") []) (Words "cat") `shouldBe` False
