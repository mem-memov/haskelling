module AnswerSpec (spec) where

import MessageTypes
import qualified Answer (hasWords)

import Test.Hspec

spec :: Spec
spec = do
  describe "Answer.hasWords" $ do

    it "has positiove result" $
      Answer.hasWords (Answer Nothing (Words "dog") []) (Words "dog") `shouldBe` True

    it "has negative result" $
      Answer.hasWords (Answer Nothing (Words "dog") []) (Words "cat") `shouldBe` False
