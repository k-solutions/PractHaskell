{-# LANGUAGE MultiParamTypeClasses #-}

module KMeans where

import           Control.Lens
import           Control.Monad        (unless)
import           Control.Monad.RWS
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State
import           Data.List
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.STRef


--- Data Definitions ---

class Ord v => Vector v where
    distance :: v -> v -> Double
    centroid :: [v] -> v

instance Vector (Double, Double) where
    distance (a, b) (c, d) = sqrt $ (c - a) * (c - a) + (d - b) * (d - b)
    centroid ls = let (u, v) = foldr (\(a,b) (c,d) -> (a+c,b+d)) (0,0) ls
                      n = fromIntegral $ length ls
                  in (u / n, v / n)

class Vector v => Vectorizable e v where
    toVector :: e -> v

instance Vectorizable (Double, Double) (Double, Double) where
    toVector = id

--- State Data ---

type Threshold = Double

data KMeansState v = KMeansState
                   { _centers :: [v]
                   , _steps   :: Int
                   }

makeLenses ''KMeansState

type KMState v = State (KMeansState v) [v]

--- State Lenses API ---

kMeansState':: (Vector v, Vectorizable e v)
             => [e]
             -> Threshold
             -> KMState v
kMeansState' points t = do
    prevCents <- use centers
    let assignments = clsAssignState prevCents points
        newCents    = newCenters assignments
    centers .= newCents
    steps += 1
    let err = sum $ zipWith distance prevCents newCents
    if err < t then pure newCents else kMeansState' points t

kMeansWithState :: (Vector v, Vectorizable e v)
                 => Int
                 -> [e]
                 -> Double
                 -> [v]
kMeansWithState n pts t = evalState (kMeansState' pts t) $ initState n

--- State Helpers ---

initState ::  Int
           -> KMeansState v
initState = KMeansState []                   --- This is obviously wrong, we need to provide points and map toVector to them

clsAssignState :: (Vector v, Vectorizable e v)
               => [v] -> [e] -> Map v [e]
clsAssignState cs points =
    let initMap = M.fromList $ zip cs (repeat [])
    in foldr (adjustM cs) initMap points
  where
    compareDistance p x y = compare (distance x $ toVector p) (distance y $ toVector p)
    adjustM :: (Ord v, Vector v, Vectorizable e v) => [v] -> e -> Map v [e] -> Map v [e]
    adjustM cs' p m = let chosenCenter = minimumBy (compareDistance p) cs'
                     in M.adjust (p:) chosenCenter m

newCenters :: (Vector v, Vectorizable e v) => Map v [e] -> [v]
newCenters = M.elems
           . fmap (centroid . map toVector)

--- With Reader ---

data KMSettings e v =  KMSettings
                    { _initFn       :: Int -> [e] -> [v]              --- function to initialise the ceters
                    , _k            :: Int                            --- number of clusters
                    , _kmsThreshold :: Threshold                      --- threshold to break out of the algorithm cycle
                    }

makeLenses ''KMSettings

kMeansMain ::  (Vector v, Vectorizable e v)
            => [e]
            -> Reader (KMSettings e v) [v]
kMeansMain points = do
    k' <- view k
    t' <- view kmsThreshold
    pure $ kMeansWithState k' points t'

--- Strict State threads ST ---

kMeansST' ::  (Vector v, Vectorizable e v)
           => Double
           -> [e]
           -> STRef s (KMeansState v)
           -> ST s (KMeansState v)
kMeansST' t pts s = do
    st <- readSTRef s
    let centerMap = clusterAssign (st ^. centers) pts
        newCents = newCenters centerMap
        err = sum $ zipWith distance (st ^. centers) newCents
    writeSTRef s $ KMeansState newCents (st ^. steps + 1)
    if err < t then readSTRef s else kMeansST' t pts s

kMeansST ::  (Vector v, Vectorizable e v)
          => (Int -> [e] -> [v])
          -> Int
          -> [e]
          -> Double
          -> [v]
kMeansST iFn n pts t = view centers $ runST $ do
  s <- newSTRef $ KMeansState (iFn n pts) 0
  kMeansST' t pts s

--- RWS API ---

type KMeansRWS e v = RWS (KMSettings e v) () (KMeansState v)

kMeansRWS' ::  (Vector v, Vectorizable e v)
            => [e]
            -> KMeansRWS e v ()
kMeansRWS' points = do
    prevCents <- use centers
    let assignments = clusterAssign prevCents points
    centers .= newCenters assignments
    t <- view kmsThreshold
    newCents <- use centers
    let err = sum $ zipWith distance prevCents newCents
    unless (err < t) $ kMeansRWS' points

kMeansRWS ::  (Vector v, Vectorizable e v)
           => (Int -> [e] -> [v])
           -> Int
           -> [e]
           -> Double
           -> [v]
kMeansRWS iFn n pts t = view centers $ fst $ execRWS (kMeansRWS' pts) (KMSettings iFn n t) (KMeansState (iFn n pts) 0)

--- Main API ---

kMeans :: (Vector v, Vectorizable e v)
       => (Int -> [e] -> [v])     --- init function
       -> Int                     --- number of centroids
       -> [e]                     --- the information
       -> Double                  --- treshold
       -> [v]                     --- centroids after convergence
kMeans i k' points = kMeans' (i k' points) points

kMeans' ::  (Vector v, Vectorizable e v)
         => [v]                   --- initial vector
         -> [e]                   --- the information
         -> Double                --- thresholds
         -> [v]                   --- final centriods
kMeans' cs points thresholds =
    let assignment  = clusterAssign cs points
        oldNewCents = newCentrs assignment
        newCents    = map snd oldNewCents
    in if shouldStop oldNewCents thresholds
         then newCents
          else kMeans' newCents points thresholds

--- Helpers ---

shouldStop :: Vector v => [(v, v)] -> Double -> Bool
shouldStop cs thresholds = foldr (\(x, y) s -> s + distance x y) 0.0 cs < thresholds

initSimple :: Int -> [e] -> [(Double, Double)]
initSimple 0 _ = []
initSimple n v = (fromIntegral n, fromIntegral n) : initSimple (n - 1) v

clusterAssign :: (Ord v, Vector v, Vectorizable e v)
               => [v]
               -> [e]
               -> Map v [e]        -- the results are stored in the map
clusterAssign centroids points =
    let initMap = M.fromList $ zip centroids (repeat [])
    in foldr (adjustM centroids) initMap points
  where
    compareDistance p x y = compare (distance x $ toVector p) (distance y $ toVector p)
    adjustM :: (Ord v, Vector v, Vectorizable e v) => [v] -> e -> Map v [e] -> Map v [e]
    adjustM cs p m = let chosenCenter = minimumBy (compareDistance p) cs
                     in M.adjust (p:) chosenCenter m

newCentrs :: (Vector v, Vectorizable e v) => Map v [e] -> [(v, v)]
newCentrs = M.toList . fmap (centroid . map toVector)

