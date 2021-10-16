{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}

module DataTypes
  ( Client (..)
  , Person (..)
  , Product (..)
  , ProductType (..)
  , Gender (..)
  , ClientKind (..)
  , HasName (..)
  , Name
  , mkPerson
  , mkClient
  , cName
  , gName
  , pGender
  , personName
  , cName
  , cContact
--  , clientName
--  , clasifyClients'
--  , clasifyClients
  ) where

import           Control.Lens
import           Data.Map     (Map)
import qualified Data.Map     as M
import           Data.Monoid  (Sum (..))
import           Data.Set     (Set)
import qualified Data.Set     as S
import           Data.Text    (Text)
import qualified Data.Text    as Tx (split)

type Name = Text
type Price = Double

data Gender = Mr
            | Ms
            | NA
            deriving (Show, Eq, Ord)

class HasName s a | s -> a where
    name :: Lens' s a

data Person = Person
            { _pFirstName :: !Name
            , _pLastName  :: !Name
            , _pGender    :: !Gender
            } deriving (Show, Eq, Ord)

makeLenses ''Person
makeFields ''Person

personName :: Lens' Person Name
personName = lens getter setter
  where
    personFullName Person {..} = mconcat [_pFirstName, " ", _pLastName]
    setPersonName p = setNm . Tx.split (== ' ')
      where
        setNm :: [Text] -> Person
        setNm (f:l:_) = p { _pFirstName = f, _pLastName = l }
        setNm _       = p
    getter :: Person -> Name
    getter = personFullName
    setter :: Person -> Name -> Person
    setter = setPersonName

instance HasName Person Name where
  name = personName

data Client = GovOrg
            { _gName :: !Name
            }
            | Company
            { _cName    :: !Name
            , _cContact :: !Person
            }
            | Ind !Person
            deriving (Show, Eq)

makeLenses ''Client

instance Ord Client where
    GovOrg n1           <= GovOrg n2    = n1 <= n2
    Ind n1              <= Ind n2       = n1 <= n2
    Company n1 _        <= Company n2 _ = n1 <= n2
    Ind (Person n1 _ _) <= Company n2 _ = n1 <= n2 || n1 == n2 && True
    Ind (Person n1 _ _) <= GovOrg n2    = n1 <= n2 || n1 == n2 && True
    Company n1 _        <= GovOrg n2    = n1 <= n2 || n1 == n2 && True
    _                   <= _            = False

data ProductType = TimeMachine
                 | TravelGuide
                 | Tool
                 | Trip
                 deriving (Show, Eq, Ord)


data Product = Product
             { _prId    :: Integer
             , _prTitle :: Name
             , _prPrice :: Price
             , _prType  :: ProductType
             } deriving (Show, Eq, Ord)

makeLenses ''Product

data ClientKind = GovOrgKind
                | CompanyKind
                | IndivKind
                deriving (Show, Eq, Ord)

--- Custom fields ---

clientName :: Lens' Client Name
clientName = lens getter setter
  where
    getter :: Client -> Name
    getter (GovOrg n)   = n
    getter (Ind p)      = p ^. personName
    getter Company {..} = _cName

    setter :: Client -> Name -> Client
    setter (GovOrg _) n        = GovOrg n
    setter (Ind p) newN        = Ind $ p & personName .~ newN
    setter c@Company {..} newN = c { _cName = newN}

--- Smart constructtors ---

mkPerson :: (Name, Name) -> Person
mkPerson (f, l) = Person f l NA

mkClient :: (Name, Maybe Name, Maybe Name) -> Client
mkClient = \case
  (o, Nothing, Nothing) -> GovOrg o
  (f, Just l, Nothing)  -> Ind $ mkPerson (f, l)
  (c, Just f, Just l)   -> Company c $ mkPerson (f, l)

--- Helper methods ---

clasifyClients' :: Set Client -> Map ClientKind (Set Client)
clasifyClients' = S.foldr accum newMap
  where
    mapInsert :: Client -> Set Client -> Maybe (Set Client)
    mapInsert it = Just . S.insert it

    accum e m = case e of
      g@(GovOrg _)    -> M.update (mapInsert g) GovOrgKind m
      i@(Ind _)       -> M.update (mapInsert i) IndivKind m
      c@(Company _ _) -> M.update (mapInsert c) CompanyKind m

    newMap = M.fromList
           [ (GovOrgKind, S.empty)
           , (CompanyKind, S.empty)
           , (IndivKind, S.empty)
           ]

clasifyClients :: Set Client -> Map ClientKind (Set Client)
clasifyClients cs = M.fromList [(GovOrgKind, setOrg), (CompanyKind, setCom), (IndivKind, setInd)]
  where
    (setOrg, setCom, setInd) = S.foldr accum (S.empty, S.empty, S.empty) cs
    accum e (sO, sC, sI) = case e of
      g@(GovOrg _)    -> (S.insert g sO, sC, sI)
      c@(Company _ _) -> (sO, S.insert c sC, sI)
      p@(Ind _)       -> (sO, sC, S.insert p sI)

discountProducts :: Foldable f => Double -> f Product -> [Product]
discountProducts percent ps = ps ^.. folded . to (discountProduct percent)

discountProduct :: Double -> Product -> Product
discountProduct percent p@Product {..} = p { _prPrice = newPrice }
  where
    newPrice :: Double
    newPrice = _prPrice * (1.0 - (percent / 100.0))

addTypeCounter :: Client -> (Sum Int, Sum Int, Sum Int) -- we count (GovOrg, Entity, Ind)
addTypeCounter = \case
  GovOrg _    -> (Sum 1, mempty, mempty)
  Company _ _ -> (mempty, Sum 1, mempty)
  Ind _       -> (mempty, mempty, Sum 1)

countClients :: Foldable f => f Client -> (Int, Int, Int)
countClients cs = (getSum a, getSum b, getSum c)
  where
    (a, b, c) = foldOf (folded . to addTypeCounter) cs

-- clientAnalytics :: []


