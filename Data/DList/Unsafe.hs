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

-- The 'Data.DList.Unsafe' module exports 'UnsafeDList' and 'unsafeApplyDList',
-- which allow breaking the invariant of the 'DList' newtype. Therefore, we
-- explicitly mark 'Data.DList.Unsafe' as unsafe.
{-# LANGUAGE Unsafe #-}

{-# LANGUAGE ViewPatterns #-}
#endif

-----------------------------------------------------------------------------

{-|

Module: Data.DList.Unsafe
Copyright: © 2006-2009 Don Stewart, 2013-2020 Sean Leather
License: BSD-3-Clause

Maintainer: sean.leather@gmail.com
Stability: stable

This module exports everything related to 'DList', including the constructor,
'UnsafeDList', and the record label, 'unsafeApplyDList', both of which can be
used to create unsafe 'DList' values that break the invariant preserved by the
names exported from 'Data.DList'.

-}

module Data.DList.Unsafe where

-----------------------------------------------------------------------------

-- CPP: base >= 4.9 for Semigroup and MonadFail
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup(..), stimesMonoid)

-- CPP: base < 4.13 for MonadFail not exported from Control.Monad
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail(..))
#endif

#endif

-- CPP: GHC >= 7.8 for IsList
#if __GLASGOW_HASKELL__ >= 708
import qualified GHC.Exts
#endif

import qualified Control.Applicative as Applicative
import Control.DeepSeq (NFData (..))
import Control.Monad as Monad
import qualified Data.Monoid as Monoid
import qualified Data.Foldable as Foldable
import Data.Function (on)
import qualified Data.List as List
import Data.String (IsString (..))
import qualified Data.Traversable as Traversable
import Prelude hiding (concat, foldr, head, map, replicate, tail)
import qualified Text.Read as Read

-----------------------------------------------------------------------------

{-|

A difference list is a function that, given a list, returns the original
contents of the difference list prepended to the given list.

This structure supports \(\mathcal{O}\)(@1@) 'append' and 'snoc' operations on
lists, making it useful for frequent applications of '++' (esp. if they are
left-nested) with lists such as logging and pretty printing.

Here is an example using @DList@ as the state type when printing a tree with the
@Writer@ monad:

@
import Control.Monad.Writer
import Data.DList

data Tree a = Leaf a | Branch (Tree a) (Tree a)

flatten_writer :: Tree x -> DList x
flatten_writer = snd . runWriter . flatten
  where
    flatten (Leaf x)     = tell (singleton x)
    flatten (Branch x y) = flatten x >> flatten y
@

-}

newtype DList a = UnsafeDList {unsafeApplyDList :: [a] -> [a]}

{-|

__@fromList xs@__ is a 'DList' representing the list __@xs@__.

@fromList@ obeys the laws:

@
'toList' . __fromList__ = 'id'
__fromList__ . 'toList' = 'id'
@

This function is implemented with '++'. Repeated uses of @fromList@ are just as
inefficient as repeated uses of '++'. If you find yourself doing some (possibly
indirect) form of the following, you may not really be taking advantage of the
'DList' representation and library:

@
__fromList__ . f . 'toList'
@

More likely, you will convert from a list, perform some operation on the
'DList', and convert back to a list:

@
'toList' . g . __fromList__
@

-}

{-# INLINE fromList #-}
fromList :: [a] -> DList a
fromList = UnsafeDList . (++)

{-|

__@toList xs@__ is the list represented by __@xs@__.

@toList@ obeys the laws:

@
__toList__ . 'fromList' = 'id'
'fromList' . __toList__ = 'id'
@

Evaluating @toList xs@ may “collapse” the chain of function composition
underlying many 'DList' functions ('append' in particular) used to construct
@xs@. This may affect any efficiency you achieved due to laziness in the
construction.

-}

{-# INLINE toList #-}
toList :: DList a -> [a]
toList = ($ []) . unsafeApplyDList

-- CPP: GHC >= 7.8 for pattern synonyms
#if __GLASGOW_HASKELL__ >= 708

-- CPP: GHC >= 7.10 for pattern synonym signatures

{-|

A unidirectional pattern synonym for 'empty'. This is implemented with 'toList'.

-}

#if __GLASGOW_HASKELL__ >= 710
pattern Nil :: DList a
#endif
pattern Nil <- (toList -> [])

{-|

A unidirectional pattern synonym for 'cons'. This is implemented with 'toList'.

-}

#if __GLASGOW_HASKELL__ >= 710
pattern Cons :: a -> [a] -> DList a
#endif
pattern Cons x xs <- (toList -> x : xs)

#endif

{-|

__@apply xs ys@__ is the list represented by the __@xs@__ after appending
__@ys@__ to it.

\(\mathcal{O}\)(@1@).

@apply@ obeys the law:

@
__apply__ ('fromList' xs) ys = xs '++' ys
@

-}

{-# INLINE apply #-}
apply :: DList a -> [a] -> [a]
apply = unsafeApplyDList

{-|

__@empty@__ is a 'DList' with no elements.

@empty@ obeys the law:

@
'toList' __empty__ = []
@

-}

{-# INLINE empty #-}
empty :: DList a
empty = UnsafeDList id

{-|

__@singleton x@__ is a 'DList' with the single element __@x@__.

@singleton@ obeys the law:

@
'toList' (__singleton__ x) = [x]
@

-}

{-# INLINE singleton #-}
singleton :: a -> DList a
singleton = UnsafeDList . (:)

{-|

__@cons x xs@__ is a 'DList' with the 'head' __@x@__ and the 'tail' __@xs@__.

\(\mathcal{O}\)(@1@).

@cons@ obeys the law:

@
'toList' (__cons__ x xs) = x : 'fromList' xs
@

-}

infixr 9 `cons`

{-# INLINE cons #-}
cons :: a -> DList a -> DList a
cons x xs = UnsafeDList ((x :) . unsafeApplyDList xs)

infixl 9 `snoc`

{-|

__@snoc xs x@__ is a 'DList' with the initial 'DList' __@xs@__ and the last
element __@x@__.

\(\mathcal{O}\)(@1@).

@snoc@ obeys the law:

@
'toList' (__snoc__ xs x) = 'fromList' xs '++' [x]
@

Note that 'fromList' is implemented with '++', which means that the right-hand
side of the equality demonstrates a use of a left-nested append. This is the
sort of inefficiency that @snoc@ on 'DList's avoids.

-}

{-# INLINE snoc #-}
snoc :: DList a -> a -> DList a
snoc xs x = UnsafeDList (unsafeApplyDList xs . (x :))

{-|

__@append xs ys@__ is a 'DList' obtained from the concatenation of the elements
of __@xs@__ and __@ys@__.

\(\mathcal{O}\)(@1@).

@append@ obeys the law:

@
'toList' (__append__ xs ys) = 'fromList' xs '++' 'fromList' ys
@

Note that 'fromList' is implemented with '++', which means that the right-hand
side of the equality demonstrates a use of a left-nested append. This is the
sort of inefficiency that @append@ on 'DList's avoids.

-}

{-# INLINE append #-}
append :: DList a -> DList a -> DList a
append xs ys = UnsafeDList (unsafeApplyDList xs . unsafeApplyDList ys)

{-|

__@concat xss@__ is a 'DList' representing the concatenation of all 'DList's in
the list __@xss@__.

\(\mathcal{O}\)(@'length' xss@).

@concat@ obeys the law:

@
'toList' (__concat__ xss) = 'List.concat' ('List.map' 'toList' xss)
@

-}

{-# INLINE concat #-}
concat :: [DList a] -> DList a
concat = List.foldr append empty

{-|

__@replicate n x@__ is a 'DList' of length __@n@__ with __@x@__ as the value of
every element.

\(\mathcal{O}\)(@n@).

@replicate@ obeys the law:

@
'toList' (__replicate__ n x) = 'List.replicate' n x
@

-}

{-# INLINE replicate #-}
replicate :: Int -> a -> DList a
replicate n x = UnsafeDList $ \xs ->
  let go m
        | m <= 0 = xs
        | otherwise = x : go (m -1)
   in go n

{-|

__@list nl cn xs@__ is the case elimination of __@xs@__. If @xs@ is empty, the
result is __@nl@__. If @xs@ has the head @y@ and tail @ys@, the result is __@cn
y ys@__.

\(\mathcal{O}\)(@'List.length' ('toList' xs)@).

@list@ obeys the law:

@
__list__ 'empty' 'cons' = 'id'
@

Note that @list@ uses 'toList' to get the represented list and 'fromList' to
get the 'DList' of the tail.

-}

list :: b -> (a -> DList a -> b) -> DList a -> b
list nl cn dl =
  case toList dl of
    [] -> nl
    (x : xs) -> cn x (fromList xs)

{-|

__@head xs@__ is the first element of __@xs@__. If @xs@ is empty, an 'error' is
raised.

\(\mathcal{O}\)(@1@).

@head@ obeys the law:

@
__head__ ('fromList' (x : xs)) = x
@

Note that @head@ is implemented with 'list'.

-}

{-# INLINE head #-}
head :: DList a -> a
head = list (error "Data.DList.head: empty DList") const

{-|

__@tail xs@__ is a 'DList' excluding the first element of __@xs@__. If @xs@ is
empty, an 'error' is raised.

\(\mathcal{O}\)(@'length' ('toList' xs)@).

@tail@ obeys the law:

@
__tail__ ('fromList' (x : xs)) = xs
@

Note that @tail@ is implemented with 'list'.

-}

{-# INLINE tail #-}
tail :: DList a -> DList a
tail = list (error "Data.DList.tail: empty DList") (flip const)

{-|

__@unfoldr f z@__ is the 'DList' constructed from the recursive application of
__@f@__. The recursion starts with the seed value __@z@__ and ends when, for
some @z' : b@, @f z' == 'Nothing'@.

\(\mathcal{O}\)(@'length' ('List.unfoldr' f z)@).

@unfoldr@ obeys the law:

@
'toList' (__unfoldr__ f z) = 'List.unfoldr' f z
@

-}

unfoldr :: (b -> Maybe (a, b)) -> b -> DList a
unfoldr f z =
  case f z of
    Nothing -> empty
    Just (x, z') -> cons x (unfoldr f z')

{-|

__@foldr f z xs@__ is the right-fold of __@f@__ over __@xs@__.

\(\mathcal{O}\)(@'length' ('toList' xs)@).

@foldr@ obeys the law:

@
__foldr__ f z xs = 'List.foldr' f z ('toList' xs)
@

-}

{-# INLINE foldr #-}
foldr :: (a -> b -> b) -> b -> DList a -> b
foldr f z = List.foldr f z . toList

{-|

__@map f xs@__ is the 'DList' obtained by applying __@f@__ to each element of
__@xs@__.

\(\mathcal{O}\)(@'length' ('toList' xs)@).

@map@ obeys the law:

@
'toList' (__map__ f xs) = 'List.map' f ('toList' xs)
@

-}

{-# INLINE map #-}
map :: (a -> b) -> DList a -> DList b
map f = foldr (cons . f) empty

{-|

__@intercalate xs xss@__ is the concatenation of __@xss@__ after the insertion
of __@xs@__ between every pair of
elements.

\(\mathcal{O}\)(@'length' xss@).

@intercalate@ obeys the law:

@
'toList' (__intercalate__ xs xss) = 'List.intercalate' ('toList' xs) ('map' 'toList' xss)
@

-}

{-# INLINE intercalate #-}
intercalate :: DList a -> [DList a] -> DList a
intercalate sep = concat . List.intersperse sep

instance Eq a => Eq (DList a) where
  (==) = (==) `on` toList

instance Ord a => Ord (DList a) where
  compare = compare `on` toList

-- The 'Read' and 'Show' instances were adapted from 'Data.Sequence'.

instance Read a => Read (DList a) where
  readPrec = Read.parens $ Read.prec 10 $ do
    Read.Ident "fromList" <- Read.lexP
    dl <- Read.readPrec
    return (fromList dl)
  readListPrec = Read.readListPrecDefault

instance Show a => Show (DList a) where
  showsPrec p dl =
    showParen (p > 10) $
      showString "fromList " . shows (toList dl)

instance Monoid.Monoid (DList a) where
  {-# INLINE mempty #-}
  mempty = empty

  {-# INLINE mappend #-}
  mappend = append

instance Functor DList where
  {-# INLINE fmap #-}
  fmap = map

instance Applicative.Applicative DList where
  {-# INLINE pure #-}
  pure = singleton

  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Applicative.Alternative DList where
  {-# INLINE empty #-}
  empty = empty

  {-# INLINE (<|>) #-}
  (<|>) = append

instance Monad DList where
  {-# INLINE (>>=) #-}
  m >>= k =
    -- = concat (toList (fmap k m))
    -- = (concat . toList . fromList . List.map k . toList) m
    -- = concat . List.map k . toList $ m
    -- = List.foldr append empty . List.map k . toList $ m
    -- = List.foldr (append . k) empty . toList $ m
    foldr (append . k) empty m

  {-# INLINE return #-}
  return = Applicative.pure

-- CPP: base < 4.13 for fail in Monad
#if !MIN_VERSION_base(4,13,0)
  {-# INLINE fail #-}
  fail _ = empty
#endif

-- CPP: base >= 4.9 for MonadFail
#if MIN_VERSION_base(4,9,0)
instance MonadFail DList where
  {-# INLINE fail #-}
  fail _ = empty
#endif

instance MonadPlus DList where
  {-# INLINE mzero #-}
  mzero = empty

  {-# INLINE mplus #-}
  mplus = append

instance Foldable.Foldable DList where
  {-# INLINE fold #-}
  fold = Monoid.mconcat . toList

  {-# INLINE foldMap #-}
  foldMap f = Foldable.foldMap f . toList

  {-# INLINE foldr #-}
  foldr f x = List.foldr f x . toList

  {-# INLINE foldl #-}
  foldl f x = List.foldl f x . toList

  {-# INLINE foldr1 #-}
  foldr1 f = List.foldr1 f . toList

  {-# INLINE foldl1 #-}
  foldl1 f = List.foldl1 f . toList

-- CPP: GHC >= 7.6 for foldl', foldr' in Foldable
#if __GLASGOW_HASKELL__ >= 706
  {-# INLINE foldl' #-}
  foldl' f x = List.foldl' f x . toList

  {-# INLINE foldr' #-}
  foldr' f x = Foldable.foldr' f x . toList
#endif

-- CPP: base >= 4.8 for toList in Foldable
#if MIN_VERSION_base(4,8,0)
  {-# INLINE toList #-}
  toList = Data.DList.Unsafe.toList
#endif

instance Traversable.Traversable DList where
  {-# INLINE traverse #-}
  traverse f = foldr cons_f (Applicative.pure empty)
    where
      cons_f x = Applicative.liftA2 cons (f x)

instance NFData a => NFData (DList a) where
  {-# INLINE rnf #-}
  rnf = rnf . toList

-- This is _not_ a flexible instance to allow certain uses of overloaded
-- strings. See tests/OverloadedStrings.hs for an example and
-- https://gitlab.haskell.org/ghc/ghc/-/commit/b225b234a6b11e42fef433dcd5d2a38bb4b466bf
-- for the same change made to the IsString instance for lists.
instance a ~ Char => IsString (DList a) where
  {-# INLINE fromString #-}
  fromString = fromList

-- CPP: GHC >= 7.8 for IsList
#if __GLASGOW_HASKELL__ >= 708
instance GHC.Exts.IsList (DList a) where
  type Item (DList a) = a

  {-# INLINE fromList #-}
  fromList = fromList

  {-# INLINE toList #-}
  toList = toList
#endif

-- CPP: base >= 4.9 for Semigroup
#if MIN_VERSION_base(4,9,0)
instance Semigroup (DList a) where
  {-# INLINE (<>) #-}
  (<>) = append

  -- We use 'compare n 0' since the same expression is used in 'stimesMonoid'
  -- and we should get a lazy advantage. However, we prefer the error to be
  -- sourced here instead of 'stimesMonoid'.
  stimes n = case compare n 0 of
    LT -> error "Data.DList.stimes: negative multiplier"
    _ -> stimesMonoid n
#endif
