module Data.AlterList.NonEmpty where

import Prelude

import Data.Bifoldable (class Bifoldable, bifoldMap, bifoldl, bifoldr)
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Bitraversable (class Bitraversable, bisequenceDefault, bitraverse)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- | A non-empty list where the values' types alternate and we do not know
-- | what the final value's type will be.
-- |
-- | For example...
-- | ```
-- | -- NonEmptyAlterList String Int
-- |
-- |    One 1
-- | (Alter 1 (One   "more"))
-- | (Alter 1 (Alter "more" (One   2)))
-- | (Alter 1 (Alter "more" (Alter 2 (One "see"))))
-- | ```
data NonEmptyAlterList second first
  = One first
  | Alter first (NonEmptyAlterList first second)

instance functorNonEmptyAlterList :: Functor (NonEmptyAlterList b) where
  map = bimap identity

instance bifunctorNonEmptyAlterList :: Bifunctor NonEmptyAlterList where
  bimap g f = case _ of
    One a -> One (f a)
    Alter a list -> Alter (f a) (bimap f g list)

instance bifoldableNonEmptyAlterList :: Bifoldable NonEmptyAlterList where
  bifoldl g f initial = case _ of
    One a -> f initial a
    Alter a list -> bifoldl f g (f initial a) list

  bifoldr g f last = case _ of
    One a -> f a last
    Alter a list -> f a (bifoldr f g last list)

  bifoldMap g f = case _ of
    One a -> f a
    Alter a list -> (f a) <> bifoldMap f g list

instance bitraversableNonEmptyAlterList :: Bitraversable NonEmptyAlterList where
  bitraverse g f = case _ of
    One a -> map One (f a)
    Alter a list -> ado
      b <- f a
      list' <- bitraverse f g list
      in Alter b list'

  bisequence x = bisequenceDefault x

-- | Adds an element to the front of the list.
cons :: forall a b. b -> NonEmptyAlterList b a -> NonEmptyAlterList a b
cons b list = Alter b list

-- | Adds an element to the end of the list. Since we don't know whether
-- | the list will end with an `a` type (invalid `snoc`) or a `b` type
-- | (valid `snoc`), the list is wrapped in a `Maybe`
snoc :: forall a b. a -> NonEmptyAlterList b a -> Maybe (NonEmptyAlterList b a)
snoc newA = case _ of
  One originalA -> Nothing
  Alter originalA listB -> case listB of
    One b -> Just (Alter originalA (Alter b (One newA)))
    Alter b listA -> ado
      result <- snoc newA listA
      in Alter originalA (Alter b result)

-- | Adds an element to the end of the list. If the list ends with
-- | a value of the same type as `a`, the `defaultB` value is used instead.
snocDefault :: forall a b. a -> b -> NonEmptyAlterList b a -> NonEmptyAlterList b a
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

splitList :: forall a b. NonEmptyAlterList b a -> Tuple (List a) (List b)
splitList list =
  bifoldl (\tuple b -> rmap (Cons b) tuple) (\tuple a -> lmap (Cons a) tuple) (Tuple Nil Nil) list

-- | Only returns a `NonEmptyAlterList` if both Lists' size are equal
zipList :: forall a b. List a -> List b -> Maybe (NonEmptyAlterList b a)
zipList Nil _ = Nothing
zipList _ Nil = Nothing
zipList (Cons h1 tail1) (Cons h2 tail2) = ado
      rest <- zipList tail1 tail2
      in Alter h1 (Alter h2 rest)

-- | Same as `zipList` but will return a `NonEmptyAlterList` as long as
-- | the first list has an element.
zipList' :: forall a b. List a -> List b -> Maybe (NonEmptyAlterList b a)
zipList' Nil _ = Nothing
zipList' (Cons h1 _) Nil = Just (One h1)
zipList' (Cons h1 tail1) (Cons h2 tail2) = ado
  rest <- zipList' tail1 tail2
  in Alter h1 (Alter h2 rest)
