module Test.Lens.CoreSpec where

import Rot.Data.Lens.Light.Core
import Test.Hspec

data Score = Score {
    _p1Score :: Int
  , _p2Score :: Int
  , _name    :: String
}

p1Score :: (Score -> (Int -> Score, Int))
p1Score = lens _p1Score (\x s -> s { _p1Score = x })

p2Score :: (Score -> (Int -> Score, Int))
p2Score = lens _p2Score (\x s -> s { _p2Score = x })

name :: (Score -> (String -> Score, String))
name = lens _name (\x s -> s { _name = x })

rec :: Score
rec = Score 5 10 "Tournament"

spec :: Spec
spec = do

  -- pure

  describe "test" $ do

    it "getL p1Score == 5" $
      getL p1Score rec `shouldBe` 5

    it "setL p2Score 4" $
      getL p2Score (setL p2Score 4 rec) `shouldBe` 4

    it "rec ^. p2Score == 10" $
      rec ^. p2Score `shouldBe` 10

    it "modL .." $
      True `shouldBe` True
