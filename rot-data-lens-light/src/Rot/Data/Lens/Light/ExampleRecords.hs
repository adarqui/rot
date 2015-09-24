module Rot.Data.Lens.Light.ExampleRecords (
  Score (..),
  p1Score,
  p2Score,
  name,
  rec
) where

import Rot.Data.Lens.Light.Core

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
