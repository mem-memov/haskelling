module AnswersSpec (spec) where
{-# LANGUAGE TemplateHaskell #-}
import MessageTypes
import Answer
import Answers

import Test.Hspec

spec :: Spec
spec = do
  describe "findAnswer" $ do

    it "finds answer in a list of one answer" $
      findAnswer  (Words "dog") [(Answer Nothing (Words "dog") [])] `shouldMatch` (Just (Answer Nothing (Words "dog") []), [])
