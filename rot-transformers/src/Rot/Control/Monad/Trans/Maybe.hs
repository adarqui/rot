module Rot.Control.Monad.Trans.Maybe (
  runMaybeT,
  mapMaybeT
) where

runMaybeT :: (m (Maybe a)) -> m (Maybe a)
runMaybeT = id

mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> (m (Maybe a)) -> (n (Maybe b))
mapMaybeT f = f . runMaybeT
