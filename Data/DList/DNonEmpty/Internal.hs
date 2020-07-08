-- Options passed to GHC
{-# OPTIONS_GHC -O2 #-}
-- Options passed to Haddock
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

#if !defined(__GLASGOW_HASKELL__)
#error "Your compiler is not GHC. Let us know if dlist can be made to work on it."
#endif

-- For the IsList and IsString instances
{-# LANGUAGE TypeFamilies #-}

-- CPP: GHC >= 7.8 for pattern synonyms, Safe Haskell, view patterns
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PatternSynonyms #-}

{-# LANGUAGE ViewPatterns #-}
#endif

-----------------------------------------------------------------------------

{-|

Module: Data.DList.DNonEmpty.Internal
Copyright: © 2006-2009 Don Stewart, 2013-2020 Sean Leather
License: BSD-3-Clause

Maintainer: sean.leather@gmail.com
Stability: stable

This module includes everything related to 'DNonEmpty'. It is not directly exposed
to users of the 'dlist' package.

-}

module Data.DList.DNonEmpty.Internal where

-----------------------------------------------------------------------------

-- CPP: base < 4.13 for MonadFail not exported from Control.Monad
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail(..))
#endif

-- CPP: GHC >= 7.8 for IsList
#if __GLASGOW_HASKELL__ >= 708
import qualified GHC.Exts as Exts
#endif

import Data.Semigroup (sconcat)
import qualified Control.Applicative as Applicative
import Control.DeepSeq (NFData (..))
import Data.DList (DList)
import qualified Data.DList as DList
import Control.Monad as Monad
import qualified Data.Foldable as Foldable
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.String (IsString (..))
import Prelude hiding (concat, foldr, head, map, replicate, tail)
import qualified Text.Read as Read

-----------------------------------------------------------------------------

{-|

A difference list is a function that, given a list, returns the original
contents of the difference list prepended to the given list.

This structure supports&#x00A0;\(\mathcal{O}\)(@1@) 'append' and 'snoc'
operations on lists, making it useful for replacing frequent applications of
'++' such as logging and pretty printing (esp. if those uses of '++' are
left-nested).

Here is an example using @DNonEmpty@ as the state type when printing a tree with
the @Writer@ monad:

@
import Control.Monad.Writer
import Data.DList.DNonEmpty

data Tree a = Leaf a | Branch (Tree a) (Tree a)

flatten_writer :: Tree x -> DNonEmpty x
flatten_writer = snd . runWriter . flatten
  where
    flatten (Leaf x)     = tell (singleton x)
    flatten (Branch x y) = flatten x >> flatten y
@

-}

data DNonEmpty a = DNonEmpty {
  head :: a,
  tail :: DList a
}

{-|

__@fromList xs@__ is a 'DNonEmpty' representing the list __@xs@__.

@fromList@ obeys the laws:

@
'toList' . __fromList__ = 'id'
__fromList__ . 'toList' = 'id'
@

This function is implemented with '++'. Repeated uses of @fromList@ are just as
inefficient as repeated uses of '++'. If you find yourself doing some (possibly
indirect) form of the following, you may not really be taking advantage of the
'DNonEmpty' representation and library:

@
__fromList__ . f . 'toList'
@

More likely, you will convert from a list, perform some operation on the
'DNonEmpty', and convert back to a list:

@
'toList' . g . __fromList__
@

-}

{-# INLINE fromNonEmpty #-}
fromNonEmpty :: NonEmpty a -> DNonEmpty a
fromNonEmpty (x :| xs) = DNonEmpty x $ DList.fromList xs

{-|

__@toList xs@__ is the list represented by __@xs@__.

@toList@ obeys the laws:

@
__toList__ . 'fromList' = 'id'
'fromList' . __toList__ = 'id'
@

Evaluating @toList xs@ may “collapse” the chain of function composition
underlying many 'DNonEmpty' functions ('append' in particular) used to construct
@xs@. This may affect any efficiency you achieved due to laziness in the
construction.

-}

{-# INLINE toNonEmpty #-}
toNonEmpty :: DNonEmpty a -> NonEmpty a
toNonEmpty (DNonEmpty x xs) = x :| DList.toList xs

-- CPP: GHC >= 7.8 for pattern synonyms
#if __GLASGOW_HASKELL__ >= 708

-- CPP: GHC >= 7.10 for pattern synonym signatures

{-|

A unidirectional pattern synonym for 'cons'. This is implemented with 'toNonEmpty'.

-}

#if __GLASGOW_HASKELL__ >= 710
pattern Cons :: a -> [a] -> DNonEmpty a
#endif
pattern Cons x xs <- (toNonEmpty -> x :| xs)

#endif

toDList :: DNonEmpty a -> DList a
toDList (DNonEmpty x xs) = DList.cons x xs

{-|

__@singleton x@__ is a 'DNonEmpty' with the single element __@x@__.

@singleton@ obeys the law:

@
'toList' (__singleton__ x) = [x]
@

-}

{-# INLINE singleton #-}
singleton :: a -> DNonEmpty a
singleton x = DNonEmpty x DList.empty

{-|

__@cons x xs@__ is a 'DNonEmpty' with the 'head' __@x@__ and the 'tail' __@xs@__.

\(\mathcal{O}\)(@1@).

@cons@ obeys the law:

@
'toList' (__cons__ x xs) = x : 'fromList' xs
@

-}

infixr 9 `cons`

{-# INLINE cons #-}
cons :: a -> DNonEmpty a -> DNonEmpty a
cons x (DNonEmpty y ys) = DNonEmpty x $ DList.cons y ys

infixl 9 `snoc`

{-|

__@snoc xs x@__ is a 'DNonEmpty' with the initial 'DNonEmpty' __@xs@__ and the
last element __@x@__.

\(\mathcal{O}\)(@1@).

@snoc@ obeys the law:

@
'toList' (__snoc__ xs x) = 'fromList' xs '++' [x]
@

Note that 'fromList' is implemented with '++', which means that the right-hand
side of the equality demonstrates a use of a left-nested append. This is the
sort of inefficiency that @snoc@ on 'DNonEmpty's avoids.

-}

{-# INLINE snoc #-}
snoc :: DNonEmpty a -> a -> DNonEmpty a
snoc (DNonEmpty x xs) y = DNonEmpty x $ DList.snoc xs y

{-|

__@append xs ys@__ is a 'DNonEmpty' obtained from the concatenation of the
elements of __@xs@__ and __@ys@__.

\(\mathcal{O}\)(@1@).

@append@ obeys the law:

@
'toList' (__append__ xs ys) = 'fromList' xs '++' 'fromList' ys
@

Note that 'fromList' is implemented with '++', which means that the right-hand
side of the equality demonstrates a use of a left-nested append. This is the
sort of inefficiency that @append@ on 'DNonEmpty's avoids.

-}

{-# INLINE append #-}
append :: DNonEmpty a -> DNonEmpty a -> DNonEmpty a
append (DNonEmpty x xs) (DNonEmpty y ys) = DNonEmpty x $ DList.append xs $ DList.cons y ys

{-|

__@unfoldr f z@__ is the 'DNonEmpty' constructed from the recursive application
of __@f@__. The recursion starts with the seed value __@z@__ and ends when, for
some @z' : b@, @f z' == 'Nothing'@.

\(\mathcal{O}\)(@'length' ('List.unfoldr' f z)@).

@unfoldr@ obeys the law:

@
'toList' (__unfoldr__ f z) = 'List.unfoldr' f z
@

-}

unfoldr :: (b -> (a, Maybe b)) -> b -> DNonEmpty a
unfoldr f z =
  case f z of
    (x, Nothing) -> singleton x
    (x, Just z') -> cons x $ unfoldr f z'

{-|

__@map f xs@__ is the 'DNonEmpty' obtained by applying __@f@__ to each element
of __@xs@__.

\(\mathcal{O}\)(@'length' ('toList' xs)@).

@map@ obeys the law:

@
'toList' (__map__ f xs) = 'List.map' f ('toList' xs)
@

-}

{-# INLINE map #-}
map :: (a -> b) -> DNonEmpty a -> DNonEmpty b
map f (DNonEmpty x xs) = DNonEmpty (f x) (DList.map f xs)

instance Eq a => Eq (DNonEmpty a) where
  (==) = (==) `on` toNonEmpty

instance Ord a => Ord (DNonEmpty a) where
  compare = compare `on` toNonEmpty

-- The 'Read' and 'Show' instances were adapted from 'Data.Sequence'.

instance Read a => Read (DNonEmpty a) where
  readPrec = Read.parens $ Read.prec 10 $ do
    Read.Ident "fromNonEmpty" <- Read.lexP
    dl <- Read.readPrec
    return $ fromNonEmpty dl
  readListPrec = Read.readListPrecDefault

instance Show a => Show (DNonEmpty a) where
  showsPrec p dl =
    showParen (p > 10) $
      showString "fromNonEmpty " . shows (toNonEmpty dl)

instance Functor DNonEmpty where
  {-# INLINE fmap #-}
  fmap = map

instance Applicative.Applicative DNonEmpty where
  {-# INLINE pure #-}
  pure = singleton

  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad DNonEmpty where
  {-# INLINE (>>=) #-}
  m >>= k = sconcat $ fmap k $ toNonEmpty m

  {-# INLINE return #-}
  return = Applicative.pure

instance Foldable.Foldable DNonEmpty where
  {-# INLINE fold #-}
  fold = Foldable.fold . toNonEmpty

  {-# INLINE foldMap #-}
  foldMap f = Foldable.foldMap f . toNonEmpty

  {-# INLINE foldr #-}
  foldr f x = Foldable.foldr f x . toNonEmpty

  {-# INLINE foldl #-}
  foldl f x = Foldable.foldl f x . toNonEmpty

  {-# INLINE foldr1 #-}
  foldr1 f = Foldable.foldr1 f . toNonEmpty

  {-# INLINE foldl1 #-}
  foldl1 f = Foldable.foldl1 f . toNonEmpty

-- CPP: GHC >= 7.6 for foldl', foldr' in Foldable
#if __GLASGOW_HASKELL__ >= 706
  {-# INLINE foldl' #-}
  foldl' f x = Foldable.foldl' f x . toNonEmpty

  {-# INLINE foldr' #-}
  foldr' f x = Foldable.foldr' f x . toNonEmpty
#endif

-- CPP: base >= 4.8 for toList in Foldable
#if MIN_VERSION_base(4,8,0)
  --{-# INLINE toList #-}
  --toList = Data.DList.DNonEmpty.Internal.toList
#endif

instance NFData a => NFData (DNonEmpty a) where
  {-# INLINE rnf #-}
  rnf = rnf . toNonEmpty

-- This is _not_ a flexible instance to allow certain uses of overloaded
-- strings. See tests/OverloadedStrings.hs for an example and
-- https://gitlab.haskell.org/ghc/ghc/-/commit/b225b234a6b11e42fef433dcd5d2a38bb4b466bf
-- for the same change made to the IsString instance for lists.
instance a ~ Char => IsString (DNonEmpty a) where
  {-# INLINE fromString #-}
  fromString = Exts.fromList

-- CPP: GHC >= 7.8 for IsList
#if __GLASGOW_HASKELL__ >= 708
instance Exts.IsList (DNonEmpty a) where
  type Item (DNonEmpty a) = a

  {-# INLINE fromList #-}
  fromList = fromNonEmpty . NonEmpty.fromList

  {-# INLINE toList #-}
  toList = DList.toList . toDList
#endif

instance Semigroup (DNonEmpty a) where
  {-# INLINE (<>) #-}
  (<>) = append
