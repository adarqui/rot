module Rot.Control.Monad.Trans.List (
  runListT,
  mapListT,
  liftT
) where

runListT :: forall (m :: * -> *) a.
     m [a]
  -> m [a]
runListT = id

mapListT :: forall (m :: * -> *) (n :: * -> *) a b.
     (m [a] -> n [b])
  -> m [a]
  -> n [b]
mapListT f m = f (runListT m)

liftT :: forall (m :: * -> *) a. (Monad m)
  => m a
  -> m [a]
liftT m = do
  a <- m
  return [a]
