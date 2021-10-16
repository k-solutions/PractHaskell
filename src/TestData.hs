module TestData where

import qualified Data.Set       as S
import           Data.Text      (Text)
import qualified Data.Text      as Tx
import           System.CPUTime
import           System.Random
import           Text.Printf

import           DataTypes
import           TimeMachines

newtype TestInt = TestInt Int
                deriving Show

instance Bounded TestInt where
    minBound = TestInt 0
    maxBound = TestInt maxGenTest

instance Enum TestInt where
    fromEnum (TestInt x) = fromEnum x
    toEnum = TestInt . toEnum

instance Random TestInt where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random = randomR (minBound, maxBound)

data TestClient = TestOrg
                | TestCompany
                | TestPerson
                deriving (Show, Enum, Bounded)

instance Random TestClient where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
        (x, g') -> (toEnum x, g')
  random = randomR (minBound , maxBound)

maxGenTest :: Int
maxGenTest = 10000

--- IO Test Data API ---

genRandInts :: Int -> IO [TestInt]
genRandInts n = take n . randoms <$> newStdGen

genRandClients :: Int -> IO [Client]
genRandClients n = do
    g <- newStdGen
    let rClients :: [TestClient] = take n . randoms $ g
    let rInts :: [TestInt] = take n . randoms $ g
    pure $ zipWith mkTestClient rClients rInts

timeIt :: IO t -> IO t
timeIt  x = do
    start <- getCPUTime
    v <- x
    end <- getCPUTime
    let diff = fromIntegral (end - start) / fromIntegral timeDiv :: Double
    printf "Computation time: %0.3f sec\n"  diff
    pure v

--- Test Data API --

mkTestClient :: TestClient -> TestInt -> Client
mkTestClient tc (TestInt i) =
    case tc of
      TestPerson  -> mkClient ("TestFirst" <> newN, Just $ "TestLast" <> newN, Nothing)
      TestOrg     -> mkClient ("TestOrg" <> newN, Nothing, Nothing)
      TestCompany -> mkClient ("TestCo" <> newN, Just $ "TestName" <> newN, Just $ "TestLast" <> newN)

  where newN = mkText i

mkTestPerson :: Integer -> Person
mkTestPerson i = mkPerson ("First" <> mkText i, "Last" <> mkText i)

--- Helpers ---

find_ :: (a -> Bool) -> [a] -> Maybe a
find_ _ [] = Nothing
find_ fFn (x:xs) | fFn x = Just x
                 | otherwise = find_ fFn xs

mkText :: Show a => a -> Text
mkText = Tx.pack . show

timeDiv :: Num a => a
timeDiv = 10 ^ 12

--- Test Data ---

tm1 = TM "test1" 1010 100.10
tm2 = TM "test2" 1000 99.50
tm3 = TM "test3" 1900 88.88

tms = [ tm1
      , tm2
      , tm3
      ]

person1 = Person "First1" "Last1"  Mr
person2 = Person "First2" "Last2"  NA
person3 = Person "First3" "Last3"  Ms

company1 = Company "Test1 Co" person3
company2 = Company "Test2 Co" person2
company3 = Company "Test3 Co" person1

client1 = GovOrg "NASA"
client2 = company1
client3 = Ind person2
client4 = Ind person1
client5 = Ind person3

clients = S.fromList
        [ client1
        , client2
        , client3
        , client4
        , client5
        , company2
        , company3
        ]

product1 = Product 1 "Great Time Machine" 100.00 TimeMachine
product2 = Product 2 "Small Back in time machine" 20.00 Tool
product3 = Product 3 "Midle Feature traveller" 50.00 Trip

products = S.fromList
         [ product1
         , product2
         , product3
         ]

