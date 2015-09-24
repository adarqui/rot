module Rot.Control.Monad.Trans.Except (
  runExceptT,
  mapExceptT,
  withExceptT,
  throwE,
  catchE
) where

runExceptT :: (m (Either e a)) -> m (Either e a)
runExceptT = id

mapExceptT :: (m (Either e a) -> n (Either e' b)) -> m (Either e a) -> n (Either e' b)
mapExceptT f m = f (runExceptT m)

withExceptT :: (Functor m) => (e -> e') -> m (Either e a) -> m (Either e' a)
withExceptT f = mapExceptT $ fmap $ either (Left . f) Right

throwE :: (Monad m) => e -> m (Either e a)
throwE = return . Left

catchE :: (Monad m) => m (Either e a) -> (e -> m (Either e' a)) -> m (Either e' a)
m `catchE` h = do
  a <- runExceptT m
  case a of
    Left l -> runExceptT (h l)
    Right r -> return (Right r)
