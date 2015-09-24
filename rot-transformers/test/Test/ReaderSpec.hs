module Test.ReaderSpec where

import Rot.Control.Monad.Trans.Reader
import Test.Hspec

spec :: Spec
spec = do

  -- pure

  describe "runReader" $ do
    it "True" $
      runReader (ask `bindReader` \v -> if v == "Reader" then (returnReader True) else (returnReader False)) "Reader"
        `shouldBe` True

  -- transformer

  describe "runReaderT" $ do
    it "True" $
      runReaderT (askT `bindReaderT` \v -> if v == "Reader" then (returnReaderT True) else (returnReaderT False)) "Reader"
        `shouldReturn` True
