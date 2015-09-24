module Rot.Control.Monad.Trans.State.Lazy (
  runState,
  evalState,
  execState,
  state,
  mapState,
  withState,
  get,
  gets,
  put,
  modify,
  modify',
  returnState,
  bindState,
  bindState_,
  runStateT,
  evalStateT,
  execStateT,
  stateT,
  mapStateT,
  withStateT,
  getT,
  getsT,
  putT,
  modifyT,
  modifyT',
  returnStateT,
  bindStateT,
  bindStateT_,
  liftStateT
) where

runState :: (s -> (a, s)) -> s -> (a, s)
runState = id

evalState :: (s -> (a, s)) -> s -> a
evalState m s = fst (runState m s)

execState :: (s -> (a, s)) -> s -> s
execState m s = snd (runState m s)

state :: (s -> (a, s)) -> (s -> (a, s))
state = id

mapState :: ((a, s) -> (b, s)) -> (s -> (a, s)) -> (s -> (b, s))
mapState f s = f . runState s

withState :: (s -> s) -> (s -> (a, s)) -> (s -> (a, s))
withState f s = runState s . f

get :: (s -> (s, s))
get = \s -> (s, s)

gets :: (s -> a) -> (s -> (a, s))
gets f = \s -> (f s, s)

put :: s -> (s -> ((), s))
put s = \_ -> ((), s)

modify :: (s -> s) -> (s -> ((), s))
modify f = \s -> ((), f s)

modify' :: (s -> s) -> (s -> ((), s))
modify' f = \s -> ((), f $! s)

returnState :: a -> (s -> (a, s))
returnState a = \s -> (a, s)

bindState :: (s -> (a, s)) -> (a -> (s -> (b, s))) -> (s -> (b, s))
bindState m k = \s -> let (a, s') = runState m s in runState (k a) s'

bindState_ :: (s -> (a, s)) -> (s -> (b, s)) -> (s -> (b, s))
bindState_ m k = \s -> let (_, s') = runState m s in runState k s'

--
-- Transformer
--

runStateT :: (Monad m) => (s -> m (a, s)) -> s -> m (a, s)
runStateT = id

evalStateT :: (Monad m) => (s -> m (a, s)) -> s -> m a
evalStateT m s = do
  ~(a, _) <- runStateT m s
  return a

execStateT :: (Monad m) => (s -> m (a, s)) -> s -> m s
execStateT m s = do
  ~(_, s') <- runStateT m s
  return s'

stateT :: (Monad m) => (s -> (a, s)) -> (s -> m (a, s))
stateT f = return . f

mapStateT :: (Monad m, Monad n) => (m (a, s) -> n (b, s)) -> (s -> m (a, s)) -> (s -> n (b, s))
mapStateT f m = f . runStateT m

withStateT :: (Monad m) => (s -> s) -> (s -> m (a, s)) -> (s -> m (a, s))
withStateT f m = runStateT m . f

getT :: (Monad m) => (s -> m (s, s))
getT = \s -> return (s, s)

getsT :: (Monad m) => (s -> a) -> (s -> m (a, s))
getsT f = \s -> return (f s, s)

putT :: (Monad m) => s -> (s -> m ((), s))
putT s = \_ -> return ((), s)

modifyT :: (Monad m) => (s -> s) -> (s -> m ((), s))
modifyT f = \s -> return ((), f s)

modifyT' :: (Monad m) => (s -> s) -> (s -> m ((), s))
modifyT' f = \s -> return ((), f $! s)

returnStateT :: (Monad m) => a -> (s -> m (a, s))
returnStateT a = \s -> return (a, s)

bindStateT :: (Monad m) => (s -> m (a, s)) -> (a -> (s -> m (b, s))) -> (s -> m (b, s))
bindStateT m k = \s -> do
  (a, s') <- runStateT m s
  runStateT (k a) s'

bindStateT_ :: (Monad m) => (s -> m (a, s)) -> (s -> m (b, s)) -> (s -> m (b, s))
bindStateT_ m k = \s -> do
  (_, s') <- runStateT m s
  runStateT k s'

liftStateT :: (Monad m) => m a -> (s -> m (a, s))
liftStateT m = \s -> do
  a <- m
  return (a, s)
