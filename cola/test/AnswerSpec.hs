module AnswerSpec (spec) where

import MessageTypes
import qualified Answer (hasWords)

import Test.Hspec

spec :: Spec
spec = do
  describe "Answer.hasWords" $ do

    it "has positiove result" $
      Answer.hasWords (Words "dog") (Answer Nothing (Words "dog") []) `shouldBe` True

    it "has negative result" $
      Answer.hasWords (Words "cat") (Answer Nothing (Words "dog") []) `shouldBe` False
