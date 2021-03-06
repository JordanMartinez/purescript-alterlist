module Data.AlterList.AlterListCons where

import Prelude

import Data.Bifoldable (class Bifoldable, bifoldMap, bifoldl, bifoldr)
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Bitraversable (class Bitraversable, bisequenceDefault, bitraverse)
import Data.List (List(..), foldl)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- | A non-empty list where the values' types alternate and the first and
-- | last values' types will be the same. Values are stored as a
-- | two-elements-per-node linked-list for `O(1)` access to the head.
-- |
-- | For example...
-- | ```
-- | -- AlterListCons String Int
-- |
-- |  Single 1
-- | (Double 1 "more" (Single 2)
-- | (Double 1 "more" (Double 2 "see" (Single 3)))
-- | ```
data AlterListCons inner outer
  = Single outer
  | Double outer inner (AlterListCons inner outer)

instance semigroupAlterListCons :: Semigroup a => Semigroup (AlterListCons b a) where
  append (Single a1) (Single a2) = Single (a1 <> a2)
  append (Single a1) (Double a2 b treeA) = Double (a1 <> a2) b treeA
  append (Double a1 b treeA) list = Double a1 b (treeA <> list)

instance monoidAlterListCons :: Monoid a => Monoid (AlterListCons b a) where
  mempty = Single mempty

instance functorAlterListCons :: Functor (AlterListCons b) where
  map = bimap identity

instance bifunctorAlterListCons :: Bifunctor AlterListCons where
  bimap g f = case _ of
    Single a -> Single (f a)
    Double a b list -> Double (f a) (g b) (bimap g f list)

instance bifoldableAlterListCons :: Bifoldable AlterListCons where
  bifoldl g f initial = case _ of
    Single a -> initial
    Double a b list -> bifoldl g f (g (f initial a) b) list

  bifoldr g f last = case _ of
    Single a -> last
    Double a b list -> f a (g b (bifoldr g f last list))

  bifoldMap g f = case _ of
    Single a -> f a
    Double a b list -> (f a) <> (g b) <> bifoldMap g f list

instance bitraversableAlterListCons :: Bitraversable AlterListCons where
  bitraverse g f = case _ of
    Single a -> map Single (f a)
    Double a b list -> ado
      newA <- f a
      newB <- g b
      list' <- bitraverse g f list
      in Double newA newB list'

  bisequence x = bisequenceDefault x

isSingle :: forall a b. AlterListCons b a -> Boolean
isSingle (Single _) = true
isSingle _ = false

isDouble :: forall a b. AlterListCons b a -> Boolean
isDouble (Double _ _ _) = true
isDouble _ = false

-- | Creates a list with only one element
singleton :: forall a b. a -> AlterListCons b a
singleton = Single

-- | Adds the next two elements to the front of the list
cons :: forall a b. a -> b -> AlterListCons b a -> AlterListCons b a
cons newA newB rest = Double newA newB rest

-- | Adds the next two elements to the end of the list
snoc :: forall a b. b -> a -> AlterListCons b a -> AlterListCons b a
snoc newB newA = case _ of
  Single a -> Double a newB (Single newA)
  Double a b treeA ->
    Double a b (snoc newB newA treeA)

-- | Splits the AlterListCons into a list of `a`s and `b`s. The `a` list
-- | will always have one more element than the `b` list.
splitList :: forall a b. AlterListCons b a -> Tuple (List a) (List b)
splitList =
  bifoldl (\tuple b -> rmap (Cons b) tuple) (\tuple a -> lmap (Cons a) tuple) (Tuple Nil Nil)

-- | Zips the two lists together until the one of the lists runs out of
-- | elements.
zipList :: forall a b. List a -> List b -> Maybe (AlterListCons b a)
zipList Nil _ = Nothing
zipList (Cons a _) Nil = Just (Single a)
zipList (Cons a tail1) (Cons b tail2) = ado
  rest <- zipList tail1 tail2
  in Double a b rest

-- | Similar to `intercalate`. The output is the reversed input list
-- | whose `b` value is the result of applying all values in the input list
-- | except for its first element.
-- |
-- | `mapAlter show (1 : 2 : 3 : Nil)`
-- | produces
-- | `Double 3 "3" (Double 2 "2") (Single 1)`
mapAlter :: forall a b. (a -> b) -> List a -> Maybe (AlterListCons b a)
mapAlter f = case _ of
  Nil -> Nothing
  Cons h tail -> Just (foldl (\tree a -> cons a (f a) tree) (Single h) tail)
