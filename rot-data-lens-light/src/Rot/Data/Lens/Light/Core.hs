module Rot.Data.Lens.Light.Core (
  runLens,
  lens,
  iso,
  getL,
  setL,
  modL,
  modL',
  (^.),
  (^=),
  vanLaarhoven
) where

-- | Lens
--
-- (a -> (b -> a, b))
--

runLens :: (a -> (b -> a, b)) -> (a -> (b -> a, b))
runLens = id

-- | Build a lens out of a getter and setter
lens :: (a -> b) -> (b -> a -> a) -> (a -> (b -> a, b))
lens get set = \a -> (flip set a, get a)

-- | Build a lens out of an isomorphism
iso :: (a -> b) -> (b -> a) -> (a -> (b -> a, b))
iso f g = lens f (\x _ -> g x)

-- | Get the getter function from a lens
getL :: (a -> (b -> a, b)) -> a -> b
getL l = snd . runLens l

-- | Get the setter function from a lens
setL :: (a -> (b -> a, b)) -> b -> a -> a
setL l = flip $ fst . runLens l

-- | Get the modifier function from a lens
modL :: (a -> (b -> a, b)) -> (b -> b) -> a -> a
modL l f a =
  case runLens l a of
    (setx, x) -> setx (f x)

-- | Get the modifer function from a lens, forcing function application
modL' :: (a -> (b -> a, b)) -> (b -> b) -> a -> a
modL' l f a =
  case runLens l a of
    (setx, x) -> setx $! f x

-- | Infix version of getL (with the arguments reversed)
infixl 9 ^.
(^.) :: b -> (b -> (c -> b, c)) -> c
(^.) = flip getL

-- | Infix version of 'setL' (with the reverse order of arguments)
infixl 9 ^=
(^=) :: (a -> (b -> a, b)) -> b -> a -> a
(p ^= b) a = setL p b a

-- | Convert a lens to its van Laarhoven representation
vanLaarhoven :: (Functor f) => (a -> (b -> a, b)) -> (b -> f b) -> (a -> f a)
vanLaarhoven l f a =
  let
    fb = f (a ^. l)
    fa = fmap (\b -> setL l b a) fb
  in
    fa
