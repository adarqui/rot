module Rot.Data.Lens.Light.State (
  access,
  setL,
  (~=),
  (!=),
  (%=),
  (!%=)
) where

import           Rot.Control.Monad.Trans.State.Lazy (getsT, modifyT, modifyT')
import           Rot.Data.Lens.Light.Core

-- | Get the value of a lens into state
access :: (Monad m) => (s -> (b -> s, b)) -> (s -> m (b, s))
access l = getsT (getL l)

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
