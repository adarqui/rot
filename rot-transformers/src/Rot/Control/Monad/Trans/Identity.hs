module Rot.Control.Monad.Trans.Identity (
  runIdentity,
  runIdentityT,
  mapIdentityT
) where

runIdentity :: a -> a
runIdentity = id

runIdentityT :: f a -> f a
runIdentityT = id

mapIdentityT :: (m a -> n b) -> m a -> n b
mapIdentityT f = f . runIdentityT
