import Prelude hiding (Functor , Monad , Maybe (..), fmap, (>>=),(>>))

class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- every functor must satisfies
-- fmap (id) = id
-- fmap (g) . fmap (h) = fmap (g . h)
--

instance Functor [] where
  fmap = map

data Maybe a = Nothing | Just a
instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap (Just a) = Just (f a)

class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> (f a -> f b)

(<$>) :: (Applicative f) => (a -> b) -> (f a -> f b)
(<$>) = fmap

instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]

instance Applicative Maybe where
  pure a = Just a
  (Just f) <*> (Just a) = Just (f a)


