{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict #-}


module Numeric.Optima 
( newtRaph
, newtRP
, bPhase
, lineSearch
, lineOptima
, conjGradPR
, qsort
) where 

import Numeric.Base (dot,(+^), (-^), (*^), DVec)
import Data.Approx
import Data.Functor ((<&>))

import Control.Monad (when)

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MU

import qualified Numeric.Base as Nu

import Debug.Trace (trace)
debug = flip trace

newtRaph :: (Double->Double) -> Double -> Maybe Double
newtRaph f x = Just (0,x) >>= findSoln <&> snd where 
  findSoln :: (Int, Double) -> Maybe (Int, Double)
  findSoln (100, _) = Nothing
  findSoln (n,x0) 
    |f0'   =~ 0.0  = Nothing 
    |e1    =~ 0.0  = Just (n,x0-e1) 
    |otherwise     = Just (n+1,x0-e1) >>= findSoln 
    where 
      e1=f0/f0'; f0 = f x0; f0'= (f (x0 + 1e-5) - f0)/1e-5 

newtRP :: (Double->Double) -> Double -> Maybe Double
newtRP f x = do
  let 
    findSoln :: (Int, Double) -> Maybe (Int, Double)
    findSoln (100, _) = Nothing
    findSoln (n,x0) 
      |f0'   =~ 0.0  = Nothing 
      |e1    =~ 0.0  = Just (n,x0-e1) 
      |otherwise     = Just (n+1,x0-e1) >>= findSoln 
      where 
        e1=f0/f0'; f0 = f x0; f0'= (f (x0 + 1e-5) - f0)/1e-5 
  (_,xf) <- findSoln (0,x) -- ==let v=(0,x)==v <- return (0,x) Sugar>>Desugar
  return xf

fAbx :: (DVec -> Double) -> DVec -> DVec -> Double -> Double
fAbx f av bv s = f $ aBx av bv s

aBx :: DVec -> DVec -> Double -> DVec
aBx av bv s = U.zipWith (\ai bi -> ai + s*bi ) av bv

conjGradPR::(DVec -> Double) -> DVec -> Maybe DVec
conjGradPR f v0 = findSoln 0 d0 d0 v0 where
  n = U.length v0; d0 = Nu.negGrad f v0

  findSoln 150 _ _ _   = Nothing
  findSoln m d0 p0 x0
    |p_sq < 1e-10   = Just x0
    |otherwise     = do
      x1 <- lineOptima f x0 d0
      let 
        p1 = Nu.negGrad f x1
        d1 = if m `rem` n == n-1 then p1
        else p1 +^ d0 *^ (((p1 -^ p0) `dot` p1)/p_sq)
      findSoln (m+1) d1 p1 x1
    where p_sq = p0 `dot` p0

lineOptima :: (DVec -> Double) -> DVec -> DVec -> Maybe DVec
lineOptima f av bv = do (la, lb) <- bPhase f av bv; lineSearch f av bv la lb

lineSearch :: (DVec -> Double) -> DVec -> DVec -> Double -> Double -> Maybe DVec
lineSearch f av bv la lb = findSoln 0 la x1 x2 lb fa f1 f2 fb 
  where
  fx = fAbx f av bv; gr = 2.0/(sqrt 5.0 + 1); gs = 1.0 - gr
  nx = ceiling $ logBase gr (abs (1e-5/(lb-la)))
  x1=gr*la+gs*lb; x2=gs*la+gr*lb; fa=fx la; f1=fx x1; f2=fx x2; fb=fx lb

  findSoln n xa x1 x2 xb fa f1 f2 fb 
    |n == nx   = Just $ aBx av bv $ if f1 < f2 then x1 else x2
    |f1 > f2   = let xd = gs*x1 + gr*xb
                 in findSoln (n+1) x1 x2 xd xb f1 f2 (fx xd) fb 
    |otherwise = let xd = gr*xa + gs*x2
                 in findSoln (n+1) xa xd x1 x2 fa (fx xd) f1 f2 

bPhase :: (DVec -> Double) -> DVec -> DVec -> Maybe (Double, Double)
bPhase f av zv = if fm > fx 0.0 then Nothing else findSoln (0::Int) 0.0 m b d fm fb where   
  fx = fAbx f av zv; d = 1e-4; m = d; b = m + d; fm = fx m; fb = fx b

  findSoln 100 _ _ _ _ _ _ = Nothing
  findSoln n a m b d fm fb
    |fb > fm   =   Just (a, b)
    |otherwise =   findSoln (n+1) m b b' d' fb (fx b') 
    where d' = d * 2.0; b' = b + d'

qsort :: Ord a => V.Vector a -> V.Vector a
qsort = V.modify $ \x -> qHelper x 0 (MV.length x - 1) where

    usSwap v i j = do 
      xi <- MV.unsafeRead v i; xj <- MV.unsafeRead v j
      MV.unsafeWrite v j xi; MV.unsafeWrite v i xj

    qHelper v low high = when (low < high) $ do
      pivot <- MV.unsafeRead v high
      split <- partition v low (high-1) pivot high 
      qHelper v low (split-1) 
      qHelper v (split+1) high 
  
    partition v lo hi pv ph=if lo==hi then do usSwap v lo ph; return lo else do
      let 
        pLo l = MV.unsafeRead v l >>= \t -> if (l > hi) || (t >= pv) 
          then return l else pLo (l+1)

        pHi h = MV.unsafeRead v h >>= \t -> if (lo > h) || (t <= pv) 
          then return h else pHi (h-1)

      lo' <- pLo lo; hi' <- pHi hi 

      if lo' > hi' then do usSwap v lo' ph; return lo'      
      else do usSwap v lo' hi'; partition v (lo'+1) (hi'-1) pv ph

