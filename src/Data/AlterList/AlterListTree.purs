module Data.AlterListTree.Tree.Empty where

import Prelude

import Data.Bifoldable (class Bifoldable, bifoldMap, bifoldl, bifoldr)
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Bitraversable (class Bitraversable, bisequenceDefault, bitraverse)
import Data.List (List(..))
import Data.Tuple (Tuple(..))

-- | A non-empty list where the values' types alternate. Values are stored
-- | as an inverted tree for `O(1)` access to the head and tail.
-- |
-- | For example...
-- | ```
-- | -- AlterListTree String Int
-- |
-- |  Leaf   1
-- | (Branch 1 (Leaf   "more")      2
-- | (Branch 1 (Branch "more" (Leaf 2) "see") 3
-- | ```
data AlterListTree inner outer
  = Leaf outer
  | Branch outer (AlterListTree outer inner) outer

instance functorAlterListTree :: Functor (AlterListTree b) where
  map = bimap identity

instance bifunctorAlterListTree :: Bifunctor AlterListTree where
  bimap g f = case _ of
    Leaf a -> Leaf (f a)
    Branch a1 list a2 -> Branch (f a1) (bimap f g list) (f a2)

instance bifoldableAlterListTree :: Bifoldable AlterListTree where
  bifoldl g f initial = case _ of
    Leaf a -> initial
    Branch a1 list a2 -> f (bifoldl f g (f initial a1) list) a2

  bifoldr g f last = case _ of
    Leaf a -> last
    Branch a1 list a2 -> f a1 (bifoldr f g (f a2 last) list)

  bifoldMap g f = case _ of
    Leaf a -> f a
    Branch a1 list a2 -> (f a1) <> bifoldMap f g list <> (f a2)

instance bitraversableAlterListTree :: Bitraversable AlterListTree where
  bitraverse g f = case _ of
    Leaf a -> map Leaf (f a)
    Branch a1 list a2 -> ado
      b1 <- f a1
      list' <- bitraverse f g list
      b2 <- f a2
      in Branch b1 list' b2

  bisequence x = bisequenceDefault x

-- | Adds the next two elements to the front of the list
cons :: forall a b. a -> b -> AlterListTree b a -> AlterListTree b a
cons newA newB = case _ of
  Leaf a -> Branch newA (Leaf newB) a
  Branch a1 treeB a2 -> case treeB of
    Leaf b -> Branch newA (Branch newB (Leaf a1) b) a2
    Branch b1 treeA b2 ->
      Branch newA (Branch newB (cons a1 b1 treeA) b2) a2

-- | Adds the next two elements to the end of the list
snoc :: forall a b. b -> a -> AlterListTree b a -> AlterListTree b a
snoc newB newA = case _ of
  Leaf a -> Branch a (Leaf newB) newA
  Branch a1 treeB a2 -> case treeB of
    Leaf b -> Branch a1 (Branch b (Leaf a2) newB) newA
    Branch b1 treeA b2 ->
      Branch a1 (Branch b1 (snoc b2 a2 treeA) newB) newA

-- | Closest we can get to "treeA1 <|> treeB <|> treeA2"
interjectTree :: forall a b. AlterListTree b a -> AlterListTree a b -> AlterListTree b a -> AlterListTree b a
interjectTree left middle right = case left, right of
  Leaf a1, Leaf a2 -> Branch a1 middle a2
  Leaf a1, Branch a2 treeB2 a3 ->
    Branch a1 (interjectVal middle a2 treeB2) a3
  Branch a1 treeB1 a2, Leaf a3 ->
    Branch a1 (interjectVal treeB1 a2 middle) a3
  Branch a1 treeB1 a2, Branch a3 treeB3 a4 ->
    Branch a1 (interjectTree treeB1 (Branch a2 middle a3) treeB3) a4

-- | Closest we can get to "treeA1 `snoc` b `cons` treeA2"
interjectVal :: forall a b. AlterListTree b a -> b -> AlterListTree b a -> AlterListTree b a
interjectVal left newB right = case left, right of
  Leaf a1, Leaf a2 -> Branch a1 (Leaf newB) a2
  Leaf a1, Branch a2 treeB a3 -> Branch a1 (cons newB a2 treeB) a3
  Branch a1 treeB a2, Leaf a3 -> Branch a1 (snoc a2 newB treeB) a3
  Branch a1 treeB1 a2, Branch a3 treeB2 a4 ->
    Branch a1 (interjectTree treeB1 (Branch a2 (Leaf newB) a3) treeB2) a4

concat :: forall a b. Semigroup a => Semigroup b => AlterListTree b a -> AlterListTree b a -> AlterListTree b a
concat (Leaf a1) (Leaf a2) = Leaf (a1 <> a2)
concat (Leaf a1) (Branch a2 treeB a3) = Branch (a1 <> a2) treeB a3
concat (Branch a1 treeB a2) (Leaf a3) = Branch a1 treeB (a2 <> a3)
concat (Branch a1 treeB1 a2) (Branch a3 treeB2 a4) =
  Branch a1 (interjectVal treeB1 (a2 <> a3) treeB2) a4

splitList :: forall a b. AlterListTree b a -> Tuple (List a) (List b)
splitList =
  bifoldl (\tuple b -> rmap (Cons b) tuple) (\tuple a -> lmap (Cons a) tuple) (Tuple Nil Nil)
