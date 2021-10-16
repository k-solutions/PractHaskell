{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}

module TravelGuide where

import           Control.Lens
import           Data.Text    (Text)

import           DataTypes    (Person (..))

--- Types ---

type Title = Text
type Price = Double

data TravelGuide = TravelGuide
                 { _title   :: Title
                 , _authors :: [Person]
                 , _price   :: Price
                 } deriving (Show, Eq, Ord)
makeLenses '' TravelGuide

newtype TgByPrice = TgByPrice TravelGuide deriving Eq

instance Ord TgByPrice where
    (TgByPrice (TravelGuide t1 _ p1)) <= (TgByPrice (TravelGuide t2 _ p2)) = p1 <= p2 || (p1 == p2 && t1 <= t2)

data BinTree c a = Node c a (BinTree c a) (BinTree c a)
                 | Leaf
                 deriving (Show, Functor, Foldable)

toList :: BinTree c a -> [a]
toList = foldr (:) []

insert :: (Ord a, Monoid c) => a -> c -> BinTree c a -> BinTree c a
insert t c1 n@(Node c v l r) =
    case t `compare` v of
      EQ -> updateC c1 n
      LT ->
        let newL = insert t c l
            newC = c1 <> cached newL <> cached r
        in  Node newC v newL r
      GT ->
        let newR = insert t c r
            newC = c1 <> cached l <> cached newR
        in Node newC v l newR
  where
    updateC :: Monoid c => c -> BinTree c a -> BinTree c a
    updateC c' (Node c1' a1 l1 r1) = Node (c1' <> c') a1 l1 r1
    updateC _  _                   = error "bad path exposed"
insert t c Leaf = Node c t Leaf Leaf

cached :: Monoid c => BinTree c a -> c
cached (Node c _ _ _) = c
cached Leaf           = mempty
