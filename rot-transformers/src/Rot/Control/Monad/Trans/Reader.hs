module Rot.Control.Monad.Trans.Reader (
  runReader,
  mapReader,
  withReader,
  ask,
  asks,
  local,
  returnReader,
  bindReader,
  bindReader_,
  runReaderT,
  mapReaderT,
  withReaderT,
  askT,
  asksT,
  localT,
  returnReaderT,
  bindReaderT,
  bindReaderT_,
  liftReaderT
) where

runReader :: (r -> a) -> r -> a
runReader = id

mapReader :: (a -> b) -> (r -> a) -> (r -> b)
mapReader f m = f . runReader m

withReader :: (r' -> r) -> (r -> a) -> (r' -> a)
withReader f m = runReader m . f

ask :: (r -> r)
ask = \r -> r

asks :: (r -> a) -> (r -> a)
asks = id

local :: (r -> r) -> (r -> a) -> (r -> a)
local = withReader

returnReader :: a -> (r -> a)
returnReader a = \_ -> a

bindReader :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
bindReader m k = \r -> let a = runReader m r in runReader (k a) r

bindReader_ :: (r -> a) -> (r -> b) -> (r -> b)
bindReader_ m k = \r -> let _ = runReader m r in runReader k r

--
-- Transformer
--

runReaderT :: (r -> m a) -> r -> m a
runReaderT = id

mapReaderT :: (m a -> n b) -> (r -> m a) -> (r -> n b)
mapReaderT f m = f . runReaderT m

withReaderT :: (r' -> r) -> (r -> m a) -> (r' -> m a)
withReaderT f m = runReaderT m . f

askT :: (Monad m) => (r -> m r)
askT = runReaderT return

asksT :: (Monad m) => (r -> a) -> (r -> m a)
asksT f = runReaderT (return . f)

localT :: (r -> r) -> (r -> m a) -> (r -> m a)
localT = withReaderT

returnReaderT :: (Monad m) => a -> (r -> m a)
returnReaderT a = \_ -> return a

bindReaderT :: (Monad m) => (r -> m a) -> (a -> (r -> m b)) -> (r -> m b)
bindReaderT m k = \r -> do
  a <- runReaderT m r
  runReaderT (k a) r

bindReaderT_ :: (Monad m) => (r -> m a) -> (r -> m b) -> (r -> m b)
bindReaderT_ m k = \r -> do
  _ <- runReaderT m r
  runReaderT k r

liftReaderT :: (Monad m) => m a -> (r -> (m a))
liftReaderT m = \_ -> do
  a <- m
  return a
