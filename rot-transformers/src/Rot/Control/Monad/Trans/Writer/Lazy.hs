module Rot.Control.Monad.Trans.Writer.Lazy (
  runWriter,
  execWriter,
  writer,
  tell,
  listen,
  listens,
  pass,
  censor,
  runWriterT,
  execWriterT,
  mapWriterT,
  writerT,
  tellT,
  listenT,
  listensT,
  passT,
  censorT,
  liftWriterT
) where

runWriter :: (a, w) -> (a, w)
runWriter = id

execWriter :: (a, w) -> w
execWriter m = let ~(_, w) = runWriter m in w

writer :: (a, w) -> (a, w)
writer = id

tell :: w -> ((), w)
tell w = writer ((), w)

listen :: (a, w) -> ((a, w), w)
listen m = let ~(a, w) = runWriter m in ((a, w), w)

listens :: (w -> b) -> (a, w) -> ((a, b), w)
listens f m = let ~(a, w) = runWriter m in ((a, f w), w)

pass :: ((a, w -> w), w) -> (a, w)
pass m = let ~((a, f), w) = runWriter m in (a, f w)

censor :: (w -> w) -> (a, w) -> (a, w)
censor f m = let ~(a, w) = runWriter m in (a, f w)

--
-- Transformer
--

runWriterT :: m (a, w) -> m (a, w)
runWriterT = id

execWriterT :: (Monad m) => m (a, w) -> m w
execWriterT m = do
  ~(_, w) <- runWriterT m
  return w

mapWriterT :: (m (a, w) -> n (b, w')) -> m (a, w) -> n (b, w')
mapWriterT f m = f (runWriterT m)

writerT :: (Monad m) => (a, w) -> m (a, w)
writerT = return

tellT :: (Monad m) => w -> m ((), w)
tellT w = writerT ((), w)

listenT :: (Monad m) => m (a, w) -> m ((a, w), w)
listenT m = do
  ~(a, w) <- runWriterT m
  return ((a, w), w)

listensT :: (Monad m) => (w -> b) -> m (a, w) -> m ((a, b), w)
listensT f m = do
  ~(a, w) <- runWriterT m
  return ((a, f w), w)

passT :: (Monad m) => m ((a, w -> w), w) -> m (a, w)
passT m = do
  ~((a, f), w) <- runWriterT m
  return (a, f w)

censorT :: (Monad m) => (w -> w) -> m (a, w) -> m (a, w)
censorT f m = do
  ~(a, w) <- runWriterT m
  return (a, f w)

liftWriterT :: (Monad m, Monoid w) => m a -> m (a, w)
liftWriterT m = do
  a <- m
  return (a, mempty)
