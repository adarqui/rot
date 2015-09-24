module Rot.Data.Lens.Light2.Core (
  lens,
  iso,
  getL,
  setL,
  modL,
  (^.),
  (^=)
) where

lens :: (a -> b) -> (b -> a -> a) -> (a -> b, b -> a -> a)
lens = (,)

iso :: (a -> b) -> (b -> a) -> (a -> b, b -> a -> a)
iso f g = lens f (\x _ -> g x)

-- getL (g, _) a = g a
getL :: (a -> b, b -> a -> a) -> a -> b
getL = fst

-- setL (_, s) b a = s b a
setL :: (a -> b, b -> a -> a) -> b -> a -> a
setL = snd

modL :: (a -> b, b -> a -> a) -> (b -> b) -> a -> a
modL l f a = setL l (f (getL l a)) a

(^.) :: a -> (a -> b, b -> a -> a) -> b
a ^. p = getL p a

(^=) :: (a -> b, b -> a -> a) -> b -> a -> a
(p ^= b) a = setL p b a
