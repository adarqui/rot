module Rot.Control.Monad.Trans.Cont (
  cont,
  runCont,
  evalCont,
  mapCont,
  withCont,
  reset,
  shift,
  runContT,
  evalContT,
  mapContT,
  withContT,
  callCC,
  resetT,
  shiftT,
  liftLocal,
  liftT
) where

cont :: ((a -> r) -> r) -> ((a -> r) -> r)
cont f = \c -> f c

runCont :: ((a -> r) -> r) -> (a -> r) -> r
runCont = id

evalCont :: ((r -> r) -> r) -> r
evalCont m = runCont m id

mapCont :: (r -> r) -> ((a -> r) -> r) -> ((a -> r) -> r)
mapCont = undefined
--mapCont f m = f $ runCont m id

withCont :: ((b -> r) -> (a -> r)) -> ((a -> r) -> r) -> ((b -> r) -> r)
withCont = undefined
-- withCont f = runCont . f

reset :: ((r -> r) -> r) -> ((r -> r') -> r')
reset = undefined

shift :: ((a -> r) -> ((r -> r) -> r)) -> ((a -> r) -> r)
shift = undefined

runContT :: ((a -> m r) -> m r) -> (a -> m r) -> m r
-- runContT m next a = m (next a)
--runContT = undefined
runContT = id

evalContT :: (Monad m) => ((r -> m r) -> m r) -> m r
evalContT m = runContT m return

mapContT :: (m r -> m r) -> ((a -> m r) -> m r) -> ((a -> m r) -> m r)
mapContT f m = f . runContT m

withContT :: ((b -> m r) -> (a -> m r)) -> ((a -> m r) -> m r) -> ((b -> m r) -> m r)
withContT f m = runContT m . f

callCC :: ((a -> ((b -> m r) -> m r) -> ((a -> m r) -> m r))) -> ((a -> m r) -> m r)
-- callCC f = \c -> runContT (f (\x -> \_ -> c x)) c
callCC = undefined

resetT :: (Monad m) => ((r -> m r) -> m r) -> ((r -> m r') -> m r')
--resetT = undefined
resetT = liftT . evalContT

shiftT :: (Monad m) => ((a -> m r) -> ((r -> m r) -> m r)) -> ((a -> m r) -> m r)
shiftT f =  evalContT . f

liftT :: (Monad m) => m a -> (a -> m b) -> m b
liftT m = (m >>=)

liftLocal ::
  (Monad m)
  => m r'
  -> ((r' -> r') -> m r -> m r)
  -> (r' -> r')
  -> ((a -> m r) -> m r)
  -> ((a -> m r) -> m r)
liftLocal = undefined
