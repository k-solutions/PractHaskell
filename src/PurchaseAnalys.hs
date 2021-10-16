module PurchaseAnalys where

import           Control.Lens
import           Data.Set     (Set)
import qualified Data.Set     as S
import           Data.Text    (Text)

import           DataTypes

data Purchase = Purchase
              { _client    :: Client
              , _prroducts :: [Product]
              } deriving (Show, Eq, Ord)


data PurchaseInfo = InfoClientKind ClientKind
                  | InfoClientDuty Text
                  | InfoClientGender Gender
                  | InfoPurchaseProduct Integer
                  | InfoPurchaseProductType ProductType
                  deriving (Show, Eq, Ord)

newtype Transaction = Transaction (Set PurchaseInfo)
                      deriving (Eq, Ord)

--- Helpers ---

productsToPurchaseInfo :: [Product] -> Set PurchaseInfo
productsToPurchaseInfo = foldr fromProduct S.empty
  where
    fromProduct :: Product -> Set PurchaseInfo -> Set PurchaseInfo
    fromProduct (Product i _ _ t) = S.insert (InfoPurchaseProduct i)
                                . S.insert (InfoPurchaseProductType t)

clientToPurchaseInfo :: Client -> Set PurchaseInfo
clientToPurchaseInfo (Ind p) = S.insert (InfoClientGender $ p ^. pGender)
                             $ S.insert (InfoClientDuty "person")
                             $ S.singleton (InfoClientKind IndivKind)
clientToPurchaseInfo (GovOrg _) = S.singleton (InfoClientKind GovOrgKind)
clientToPurchaseInfo c          = S.insert (InfoClientGender $ head $ c ^.. cContact . pGender)
                                $ S.singleton (InfoClientKind CompanyKind)

purchaseToTtransaction :: Purchase -> Transaction
purchaseToTtransaction (Purchase c ps) = Transaction
                                      $ clientToPurchaseInfo c `S.union` productsToPurchaseInfo ps
