{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a = Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) => Functor (Compose f g) where
  -- (<$>) :: (a -> b) -> Compose f g a -> Compose.hs f g b
  (<$>) f (Compose c) = Compose $ (\g -> f <$> g) <$> c

instance (Apply f, Apply g) => Apply (Compose f g) where
  -- (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  -- Compose f <*> Compose v = Compose (lift2 (<*>) f v)
  Compose op <*> Compose val = let func = ((\x -> (x <*> )) <$> op)
                               in Compose $ func <*> val

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure a = Compose (pure (pure a))

instance (Bind f, Bind g) => Bind (Compose f g) where
  -- Implement the (=<<) function for a Bind instance for Compose
  (=<<) = error "impossible"
