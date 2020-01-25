module Data.AlterList.Empty where

import Prelude

import Data.Bifoldable (class Bifoldable, bifoldMap, bifoldl, bifoldr)
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Bitraversable (class Bitraversable, bisequenceDefault, bitraverse)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- | A possibly-empty list where the values' types alternate and we do not know
-- | what the final value's type will be.
-- |
-- | For example...
-- | ```
-- | -- AlterList String Int
-- |
-- |  Cons 1  Nil
-- | (Cons 1 (Cons "more"  Nil))
-- | (Cons 1 (Cons "more" (Cons 2  Nil)))
-- | (Cons 1 (Cons "more" (Cons 2 (Cons "see" Nil))))
-- | ```
data AlterList second first
  = Nil
  | Cons first (AlterList first second)

instance functorAlterList :: Functor (AlterList b) where
  map = bimap identity

instance bifunctorAlterList :: Bifunctor AlterList where
  bimap g f = case _ of
    Nil -> Nil
    Cons a list -> Cons (f a) (bimap f g list)

instance bifoldableAlterList :: Bifoldable AlterList where
  bifoldl g f initial = case _ of
    Nil -> initial
    Cons a list -> bifoldl f g (f initial a) list

  bifoldr g f last = case _ of
    Nil -> last
    Cons a list -> f a (bifoldr f g last list)

  bifoldMap g f = case _ of
    Nil -> mempty
    Cons a list -> (f a) <> bifoldMap f g list

instance bitraversableAlterList :: Bitraversable AlterList where
  bitraverse g f = case _ of
    Nil -> pure Nil
    Cons a list -> ado
      b <- f a
      list' <- bitraverse f g list
      in Cons b list'

  bisequence x = bisequenceDefault x

-- | Adds an element to the front of the list.
cons :: forall a b. b -> AlterList b a -> AlterList a b
cons b list = Cons b list

-- | Adds an element to the end of the list. Since we don't know whether
-- | the list will end with an `a` type (invalid `snoc`) or a `b` type
-- | (valid `snoc`), the list is wrapped in a `Maybe`
snoc :: forall a b. a -> AlterList b a -> Maybe (AlterList b a)
snoc newA = case _ of
  Nil -> Nothing
  Cons originalA listB -> case listB of
    Nil -> Nothing
    Cons originalB listA -> ado
      result <- snoc newA listA
      in Cons originalA (Cons originalB result)

-- | Adds an element to the end of the list. If the list ends with
-- | a value of the same type as `a`, the `defaultB` value is used instead.
snocDefault :: forall a b. a -> b -> AlterList b a -> AlterList b a
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

splitList :: forall a b. AlterList b a -> Tuple (List.List a) (List.List b)
splitList list =
  bifoldl (\tuple b -> rmap (List.Cons b) tuple) (\tuple a -> lmap (List.Cons a) tuple) (Tuple List.Nil List.Nil) list

-- | Returns an `AlterList` that only has as many elements as the shortest
-- | list in the arguments.
zipList :: forall a b. List.List a -> List.List b -> AlterList b a
zipList List.Nil _ = Nil
zipList _ List.Nil = Nil
zipList (List.Cons h1 tail1) (List.Cons h2 tail2) =
  Cons h1 (Cons h2 (zipList tail1 tail2))

-- | Same as `zipList'` but will include another element from `List a`
-- | if possible.
zipList' :: forall a b. List.List a -> List.List b -> AlterList b a
zipList' List.Nil _ = Nil
zipList' (List.Cons h1 _) List.Nil = Cons h1 Nil
zipList' (List.Cons h1 tail1) (List.Cons h2 tail2) =
  Cons h1 (Cons h2 (zipList tail1 tail2))
