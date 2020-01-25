module Data.AlterList.NonEmpty where

import Prelude

import Data.Bifoldable (class Bifoldable, bifoldMap, bifoldl, bifoldr)
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Bitraversable (class Bitraversable, bisequenceDefault, bitraverse)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- | A non-empty list where the values' types alternate.
-- |
-- | For example...
-- | ```
-- | -- NonEmptyAlterList Int String
-- |
-- |    One 1
-- | (Alter 1 (One   "more"))
-- | (Alter 1 (Alter "more" (One   2)))
-- | (Alter 1 (Alter "more" (Alter 2 (One "see"))))
-- | ```
-- |
-- | `NonEmptyAlterList a b` is isomorphic to `Tuple (List a) (List b)`
data NonEmptyAlterList a b
  = One a
  | Alter a (NonEmptyAlterList b a)

instance bifunctorNonEmptyAlterList :: Bifunctor NonEmptyAlterList where
  bimap f g = case _ of
    One a -> One (f a)
    Alter a list -> Alter (f a) (bimap g f list)

instance bifoldableNonEmptyAlterList :: Bifoldable NonEmptyAlterList where
  bifoldl f g initial = case _ of
    One a -> f initial a
    Alter a list -> bifoldl g f (f initial a) list

  bifoldr f g last = case _ of
    One a -> f a last
    Alter a list -> f a (bifoldr g f last list)

  bifoldMap f g = case _ of
    One a -> f a
    Alter a list -> (f a) <> bifoldMap g f list

instance bitraversableNonEmptyAlterList :: Bitraversable NonEmptyAlterList where
  bitraverse f g = case _ of
    One a -> map One (f a)
    Alter a list -> ado
      b <- f a
      list' <- bitraverse g f list
      in Alter b list'

  bisequence x = bisequenceDefault x

-- | Adds an element to the front of the list.
cons :: forall a b. b -> NonEmptyAlterList a b -> NonEmptyAlterList b a
cons b list = Alter b list

-- | Adds an element to the end of the list. Since we don't know whether
-- | the list will end with an `a` type (invalid `snoc`) or a `b` type
-- | (valid `snoc`), the list is wrapped in a `Maybe`
snoc :: forall a b. a -> NonEmptyAlterList a b -> Maybe (NonEmptyAlterList a b)
snoc newA = case _ of
  One originalA -> Nothing
  Alter originalA listB -> case listB of
    One b -> Just (Alter originalA (Alter b (One newA)))
    Alter b listA -> ado
      result <- snoc newA listA
      in Alter originalA (Alter b result)

-- | Adds an element to the end of the list. If the list ends with
-- | a value of the same type as `a`, the `defaultB` value is used instead.
snocDefault :: forall a b. a -> b -> NonEmptyAlterList a b -> NonEmptyAlterList a b
snocDefault newA defaultB = case _ of
  One originalA -> Alter originalA (One defaultB)
  Alter originalA listB -> case listB of
    One b -> Alter originalA (Alter b (One newA))
    Alter b listA -> Alter originalA (Alter b (snocDefault newA defaultB listA))

-- | Adds an element to the end of the list when the types do not alternate.
snoc' :: forall a. a -> NonEmptyAlterList a a -> NonEmptyAlterList a a
snoc' a = case _ of
  One a' -> Alter a' (One a)
  Alter a' listA -> Alter a' (snoc' a listA)

splitList :: forall a b. NonEmptyAlterList a b -> Tuple (List a) (List b)
splitList list =
  bifoldl (\tuple a -> lmap (Cons a) tuple) (\tuple b -> rmap (Cons b) tuple) (Tuple Nil Nil) list

zipList :: forall a b. List a -> List b -> Maybe (NonEmptyAlterList a b)
zipList Nil _ = Nothing
zipList _ Nil = Nothing
zipList (Cons h1 tail1) (Cons h2 tail2) = ado
      rest <- zipList tail1 tail2
      in Alter h1 (Alter h2 rest)
