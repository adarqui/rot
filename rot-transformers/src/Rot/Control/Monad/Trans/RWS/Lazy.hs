module Rot.Control.Monad.Trans.RWS.Lazy (
  runRWST,
  evalRWST,
  execRWST,
  mapRWST,
  withRWST,
  readerT,
  askT,
  asksT,
  localT,
  writerT,
  tellT,
  listenT,
  listensT,
  passT,
  censorT,
  stateT,
  getT,
  putT,
  modifyT,
  getsT
) where

runRWST :: (r -> s -> m (a, s, w)) -> r -> s -> m (a, s, w)
runRWST = id

evalRWST :: (Monad m)
  => (r -> s -> m (a, s, w))
  -> r
  -> s
  -> m (a, w)
evalRWST m r s = do
  ~(a, _, w) <- runRWST m r s
  return (a, w)

execRWST :: (Monad m)
  => (r -> s -> m (a, s, w))
  -> r
  -> s
  -> m (s, w)
execRWST m r s = do
  ~(_, s', w) <- runRWST m r s
  return (s', w)

mapRWST ::
     (m (a, s, w) -> n (b, s, w'))
  -> (r -> s -> m (a, s, w))
  -> (r -> s -> n (b, s, w'))
mapRWST f m = \r s -> f (runRWST m r s)

withRWST ::
     (r' -> s -> (r, s))
  -> (r -> s -> m (a, s, w))
  -> (r' -> s -> m (a, s, w))
withRWST f m = \r s -> uncurry (runRWST m) (f r s)

--
-- reader ops
--

readerT :: (Monoid w, Monad m) => (r -> a) -> (r -> s -> m (a, s, w))
readerT = asksT

askT :: (Monoid w, Monad m) => (r -> s -> m (r, s, w))
askT = \r s -> return (r, s, mempty)

asksT :: (Monoid w, Monad m) => (r -> a) -> (r -> s -> m (a, s, w))
asksT f = \r s -> return (f r, s, mempty)

localT :: (r -> r) -> (r -> s -> m (a, s, w)) -> (r -> s -> m (a, s, w))
localT f m = \r s -> runRWST m (f r) s

--
-- writer ops
--

writerT :: (Monad m) => (a, w) -> (r -> s -> m (a, s, w))
writerT (a, w) = \_ s -> return (a, s, w)

tellT :: (Monad m) => w -> (r -> s -> m ((), s, w))
tellT w = \_ s -> return ((), s, w)

listenT :: (Monad m) => (r -> s -> m (a, s, w)) -> (r -> s -> m ((a, w), s, w))
listenT m = \r s -> do
  ~(a, s', w) <- runRWST m r s
  return ((a, w), s', w)

listensT :: (Monad m) => (w -> b) -> (r -> s -> m (a, s, w)) -> (r -> s -> m ((a, b), s, w))
listensT f m = \r s -> do
  ~(a, s', w) <- runRWST m r s
  return ((a, f w), s', w)

passT :: (Monad m) => (r -> s -> m ((a, w -> w), s, w)) -> (r -> s -> m (a, s, w))
passT m = \r s -> do
  ~((a, f), s', w) <- runRWST m r s
  return (a, s', f w)

censorT :: (Monad m) => (w -> w) -> (r -> s -> m (a, s, w)) -> (r -> s -> m (a, s, w))
censorT f m = \r s -> do
  ~(a, s', w) <- runRWST m r s
  return (a, s', f w)

--
-- state ops
--

stateT :: (Monoid w, Monad m) => (s -> (a, s)) -> (r -> s -> m (a, s, w))
stateT f = \_ s -> let (a, s') = f s in return (a, s', mempty)

getT :: (Monoid w, Monad m) => (r -> s -> m (s, s, w))
getT = \_ s -> return (s, s, mempty)

putT :: (Monoid w, Monad m) => s -> (r -> s -> m ((), s, w))
putT s = \_ _ -> return ((), s, mempty)

modifyT :: (Monoid w, Monad m) => (s -> s) -> (r -> s -> m ((), s, w))
modifyT f = \_ s -> return ((), f s, mempty)

getsT :: (Monoid w, Monad m) => (s -> a) -> (r -> s -> m (a, s, w))
getsT f = \_ s -> return (f s, s, mempty)
