module Rot.Data.Lens.Light2.State (
  access,
  (^:=),
  (%=),
  (+=),
  (*=),
  (//=)
  {-,
  setL,
  (~=),
  (!=),
  (%=),
  (!%=)
  -}
) where

import           Rot.Control.Monad.Trans.State.Lazy
import           Rot.Data.Lens.Light2.Core

-- | Get the value of a lens into state
access :: (Monad m) => (s -> b, b -> s -> s) -> (s -> m (b, s))
access l = getsT (getL l)

-- | Stateful version o (^=)
infixr 4 ^:=
(^:=) :: (Monad m) => (s -> b, b -> s -> s) -> b -> (s -> m ((), s))
p ^:= b = modifyT (setL p b)

-- | Modify field p using f
infixr 4 %=
(%=) :: (Monad m) => (s -> b, b -> s -> s) -> (b -> b) -> (s -> m ((), s))
p %= f = access p `bindStateT` \b -> p ^:= (f b)

(+=) :: (Monad m, Num b) => (s -> b, b -> s -> s) -> b -> (s -> m ((), s))
p += x = p %= (+ x)

(*=) :: (Monad m, Num b) => (s -> b, b -> s -> s) -> b -> (s -> m ((), s))
-- p -= x = p %= (- x)
p *= x = p %= (* x)

(//=) :: (Monad m, Num b) => (s -> b, b -> s -> s) -> b -> (s -> m ((), s))
p //= x = p %= (* x)

{-
infixr 4 ~=
-- | Set a value using a lens into state
(~=) :: (Monad m) => (s -> (b -> s, b)) -> b -> (s -> m ((), s))
l ~= b = modifyT $ setL l b

infixr 4 !=
-- | Set a value using a lens into state
-- Forces both the value and the whole state
(!=) :: (Monad m) => (s -> (b -> s, b)) -> b -> (s -> m ((), s))
l != b = modifyT' $ setL l $! b

-- | Infix modification of a value through a lens into state
infixr 4 %=
(%=) :: (Monad m) => (s -> (b -> s, b)) -> (b -> b) -> (s -> m ((), s))
l %= f = modifyT $ modL l f

-- | Infix modification of a value through a lens into state
-- Forces both the function application and the whole state
infixr 4 !%=
(!%=) :: (Monad m) => (s -> (b -> s, b)) -> (b -> b) -> (s -> m ((), s))
l !%= f = modifyT' $ modL' l f
-}
