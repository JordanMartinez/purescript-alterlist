module Data.AlterList.NNilmpty where

import Prelude

import Data.Bifoldable (class Bifoldable, bifoldMap, bifoldl, bifoldr)
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Bitraversable (class Bitraversable, bisequenceDefault, bitraverse)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- | A possibly-empty list where the values' types alternate.
-- |
-- | For example...
-- | ```
-- | -- AlterList Int String
-- |
-- |  Cons 1  Nil
-- | (Cons 1 (Cons "more"  Nil))
-- | (Cons 1 (Cons "more" (Cons 2  Nil)))
-- | (Cons 1 (Cons "more" (Cons 2 (Cons "see" Nil))))
-- | ```
data AlterList a b
  = Nil
  | Cons a (AlterList b a)

instance bifunctorAlterList :: Bifunctor AlterList where
  bimap f g = case _ of
    Nil -> Nil
    Cons a list -> Cons (f a) (bimap g f list)

instance bifoldableAlterList :: Bifoldable AlterList where
  bifoldl f g initial = case _ of
    Nil -> initial
    Cons a list -> bifoldl g f (f initial a) list

  bifoldr f g last = case _ of
    Nil -> last
    Cons a list -> f a (bifoldr g f last list)

  bifoldMap f g = case _ of
    Nil -> mempty
    Cons a list -> (f a) <> bifoldMap g f list

instance bitraversableAlterList :: Bitraversable AlterList where
  bitraverse f g = case _ of
    Nil -> pure Nil
    Cons a list -> ado
      b <- f a
      list' <- bitraverse g f list
      in Cons b list'

  bisequence x = bisequenceDefault x

-- | Adds an element to the front of the list.
cons :: forall a b. b -> AlterList a b -> AlterList b a
cons b list = Cons b list

-- | Adds an element to the end of the list. Since we don't know whether
-- | the list will end with an `a` type (invalid `snoc`) or a `b` type
-- | (valid `snoc`), the list is wrapped in a `Maybe`
snoc :: forall a b. a -> AlterList a b -> Maybe (AlterList a b)
snoc newA = case _ of
  Nil -> Nothing
  Cons originalA listB -> case listB of
    Nil -> Nothing
    Cons originalB listA -> ado
      result <- snoc newA listA
      in Cons originalA (Cons originalB result)

-- | Adds an element to the end of the list. If the list ends with
-- | a value of the same type as `a`, the `defaultB` value is used instead.
snocDefault :: forall a b. a -> b -> AlterList a b -> AlterList a b
snocDefault newA defaultB = case _ of
  Nil -> Cons newA Nil
  Cons originalA listB -> case listB of
    Nil -> Cons originalA (Cons defaultB (Cons newA Nil))
    Cons b listA -> Cons originalA (Cons b (snocDefault newA defaultB listA))

-- | Adds an element to the end of the list when the types do not Consnate.
snoc' :: forall a. a -> AlterList a a -> AlterList a a
snoc' a = case _ of
  Nil -> Cons a Nil
  Cons a' listA -> Cons a' (snoc' a listA)

splitList :: forall a b. AlterList a b -> Tuple (List.List a) (List.List b)
splitList list =
  bifoldl (\tuple a -> lmap (List.Cons a) tuple) (\tuple b -> rmap (List.Cons b) tuple) (Tuple List.Nil List.Nil) list

zipList :: forall a b. List.List a -> List.List b -> AlterList a b
zipList List.Nil _ = Nil
zipList _ List.Nil = Nil
zipList (List.Cons h1 tail1) (List.Cons h2 tail2) =
  Cons h1 (Cons h2 (zipList tail1 tail2))
