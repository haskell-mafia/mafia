{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Mafia.P (
  -- * Primitive types
  -- ** Bool
    Bool (..)
  , bool
  , (&&)
  , (||)
  , not
  , otherwise
  -- ** Char
  , Char
  -- ** Int
  , Integer
  , Int
  , Int8
  , Int16
  , Int32
  , Int64
  , div
  -- ** Word
  , Word64
  -- ** Real
  , fromIntegral
  , fromRational

  , Double

  -- * Algebraic structures
  -- ** Monoid
  , Monoid (..)
  , (<>)
  -- ** Functor
  , Functor (..)
  , (<$>)
  , ($>)
  , void
  , with
  -- ** Bifunctor
  , Bifunctor (..)
  -- ** Applicative
  , Applicative (..)
  , (<**>)
  , orA
  , andA
  , optional
  -- ** Alternative
  , Alternative (..)
  , asum
  -- ** Monad
  , Monad (..)
  , join
  , bind
  , when
  , unless
  , mapM_
  , forever
  , unlessM
  , whenM
  , ifM
  , guardM
  , filterM
  , (=<<)
  , liftM
  -- ** MonadPlus
  , MonadPlus (..)
  , guard
  , msum
  -- ** MonadIO
  , MonadIO (..)

  -- * Data structures
  -- ** Either
  , Either (..)
  , either
  -- ** Maybe
  , Maybe (..)
  , fromMaybe
  , maybe
  , isJust
  , mapMaybe
  , maybeToRight
  , catMaybes
  -- ** Tuple
  , fst
  , snd
  , curry
  , uncurry
  -- ** List
  , module List
  -- * Typeclasses
  -- ** Enum
  , Enum
  -- ** Num
  , Num (..)
  -- ** Eq
  , Eq (..)
  -- ** Read
  , Read (..)
  , readEither
  , readMaybe
  -- ** Show
  , Show (..)
  -- *** ShowS
  , ShowS
  , showString
  -- ** Foldable
  , Foldable (..)
  , for_
  , forM_
  , all
  , head
  , concat
  , concatMap
  -- ** Ord
  , Ord (..)
  , Ordering (..)
  , comparing
  -- ** Traversable
  , Traversable (..)
  , for
  , forM
  , traverse_

  -- * Combinators
  , id
  , (.)
  , ($)
  , ($!)
  , (&)
  , const
  , flip
  , fix
  , on
  , seq

  -- * Text functions
  , Text
  -- * Partial functions
  , undefined
  , error

  -- * Debugging facilities
  , trace
  , traceM
  , traceIO
  ) where


import           Control.Monad as Monad (
           Monad (..)
         , MonadPlus (..)
         , guard
         , join
         , msum
         , when
         , unless
         , guard
         , mapM_
         , forever
         , (=<<)
         , filterM
         , liftM
         )
import           Control.Monad.IO.Class (
           MonadIO (..)
         )
import           Control.Applicative as Applicative (
           Applicative (..)
         , (<**>)
         , Alternative (..)
         , empty
         , liftA2
         , optional
         )

import           Data.Bifunctor as Bifunctor (
           Bifunctor (..)
         )
import           Data.Bool as Bool (
           Bool (..)
         , bool
         , (&&)
         , (||)
         , not
         , otherwise
         )
import           Data.Char as Char (
           Char
         )
import           Data.Either as Either (
           Either (..)
         , either
         )
import           Data.Foldable as Foldable (
           Foldable (..)
         , asum
         , traverse_
         , for_
         , forM_
         , all
         , concat
         , concatMap
         )
import           Data.Function as Function (
           id
         , (.)
         , ($)
         , (&)
         , const
         , flip
         , fix
         , on
         )
import           Data.Functor as Functor (
           Functor (..)
         , (<$>)
         , ($>)
         , void
         )
import           Data.Eq as Eq (
           Eq (..)
         )
import           Data.Int as Int (
           Int
         , Int8
         , Int16
         , Int32
         , Int64
         )
import           Data.Maybe as Maybe (
           Maybe (..)
         , fromMaybe
         , maybe
         , isJust
         , mapMaybe
         , catMaybes
         )
import           Data.Monoid as Monoid (
           Monoid (..)
         , (<>)
         )
import           Data.Ord as Ord (
           Ord (..)
         , Ordering (..)
         , comparing
         )
import           Data.Traversable as Traversable (
           Traversable (..)
         , for
         , forM
         , mapM
         )
import           Data.Tuple as Tuple (
           fst
         , snd
         , curry
         , uncurry
         )
import           Data.Word as Word (
           Word64
         )
import           Data.List as List (
                     intercalate
                   , isPrefixOf
                   , drop
                   , splitAt
                   , break
                   , filter
                   , reverse
                   , any
#if (__GLASGOW_HASKELL__ < 710)
                   , length
                   , null
#endif
                   )
import qualified Debug.Trace as Trace
import           GHC.Types as Types (
           Double
         )
import           GHC.Real as Real (
           fromIntegral
         , fromRational
         , div
         )
#if MIN_VERSION_base(4,9,0)
import           GHC.Stack (HasCallStack)
#endif

import           Prelude as Prelude (
           Enum
         , Num (..)
         , Integer
         , seq
         , ($!)
         )
import qualified Prelude as Unsafe

import           System.IO as IO (
           IO
         )
import           Data.Text as Text (Text)
import           Text.Read as Read (
           Read (..)
         , readEither
         , readMaybe
         )
import           Text.Show as Show (
           Show (..)
         , ShowS
         , showString
         )

#if MIN_VERSION_base(4,9,0)
undefined :: HasCallStack => a
#else
undefined :: a
#endif
undefined =
  Unsafe.undefined
{-# WARNING undefined "'undefined' is unsafe" #-}

#if MIN_VERSION_base(4,9,0)
error :: HasCallStack => [Char] -> a
#else
error :: [Char] -> a
#endif
error =
  Unsafe.error
{-# WARNING error "'error' is unsafe" #-}

trace :: [Char] -> a -> a
trace =
  Trace.trace
{-# WARNING trace "'trace' should only be used while debugging" #-}

#if MIN_VERSION_base(4,9,0)
traceM :: Applicative f => [Char] -> f ()
#else
traceM :: Monad m => [Char] -> m ()
#endif
traceM =
  Trace.traceM
{-# WARNING traceM "'traceM' should only be used while debugging" #-}

traceIO :: [Char] -> IO ()
traceIO =
  Trace.traceIO
{-# WARNING traceIO "'traceIO' should only be used while debugging" #-}

with :: Functor f => f a -> (a -> b) -> f b
with =
  flip fmap
{-# INLINE with #-}

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m =
  p >>= flip when m

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p m =
  p >>= flip unless m

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y =
  p >>= \b -> if b then x else y

guardM :: MonadPlus m => m Bool -> m ()
guardM f = guard =<< f

-- | Identifier version of 'Control.Monad.=<<'.
bind :: Monad m => (a -> m b) -> m a -> m b
bind = (=<<)

head :: (Foldable f) => f a -> Maybe a
head = foldr (\x _ -> return x) Nothing

-- | Logical disjunction.
orA :: Applicative f => f Bool -> f Bool -> f Bool
orA =
  liftA2 (||)

-- | Logical conjunction.
andA :: Applicative f => f Bool -> f Bool -> f Bool
andA =
  liftA2 (&&)

infixl 8 `andA`, `orA`

maybeToRight :: l -> Maybe r -> Either l r
maybeToRight l = maybe (Left l) Right
