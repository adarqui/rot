module Test.State.LazySpec where

import Rot.Control.Monad.Trans.State.Lazy
import Test.Hspec

spec :: Spec
spec = do

  -- pure

  describe "runState" $ do
    it "((),False)" $
      runState (modify not) True `shouldBe` ((),False)

    it "(True,True)" $
      runState get True `shouldBe` (True,True)

    it "((),False)" $
      runState (put False) True `shouldBe` ((),False)

    it "correct" $
      evalState (returnState "correct") True `shouldBe` "correct"

    it "True" $
      execState (returnState "correct") True `shouldBe` True

  -- transformer

  describe "runStateT" $ do
    it "(True,True)" $
      runStateT (returnStateT "a" `bindStateT_` putT False `bindStateT_` liftStateT (return "b") `bindStateT_` getT) True
        `shouldReturn` (False,False)

    it "((),False)" $ do
      runStateT (returnStateT "a" `bindStateT_` putT False `bindStateT_` liftStateT (print "b")) True
        `shouldReturn` ((),False)

    it "(5,False)" $ do
      runStateT (returnStateT "a" `bindStateT_` putT False `bindStateT_` liftStateT (print "b") `bindStateT_` returnStateT (5 :: Int)) True
        `shouldReturn` (5,False)
