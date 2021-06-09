{-# LANGUAGE NumericUnderscores, OverloadedStrings, Strict #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Spec where

import Numeric.Base (dot,(+^), (-^), (*^), (/^))
import qualified Numeric.Base as Nu
import qualified Numeric.Optima as Op
import Data.Approx ( Approx((=~), (/~)) )

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

import Data.List (foldl')


qTest :: Show a => a -> [Bool] -> IO ()
qTest nam x = putStrLn $ lJus 30 '.' nam ++" => Tests :"++ rJus 3 ' ' (p+f) ++fl f where
  (p,f)=foldl' (\(u,v) b -> if b then (u+1,v) else (u,v+1)) (0,0) x :: (Int,Int)
  fl 0 = " => Ok"
  fl z = " => +++++ << FAILED : " ++ show z ++ " >> +++++" 
  lJus n c xr = st ++ replicate (n - length st) c where st = show xr
  rJus n c xr = replicate (n - length st) c ++ st where st = show xr 

infix 3 `qCheck`
qCheck :: Show a => a -> Bool -> IO ()
qCheck nam False = putStrLn $ "*** Error *** : " ++ show nam ++ "\n"
qCheck _ _ = putStr ""




main :: IO ()
main = do 

  ("Newton Raphson"::String) `qTest` 
    [ Op.newtRaph (\x -> 12*x^4 -5*x^3 + 5) 400 =~ Nothing
    , Op.newtRaph (\x -> 12*x^4 -5*x^3) 400 =~ Just 0.416666666667
    , Op.newtRP (\x -> 12*x^4 -5*x^3 + 5) 400 =~ Nothing
    , Op.newtRP (\x -> 12*x^4 -5*x^3) 400 =~ Just 0.416666666667
    , Op.newtRaph (\x -> 12*x^4 -5*x^3 + 5) 400 =~ Nothing
    , Op.newtRaph (\x -> 12*x^4 -5*x^3) 400 =~ Just 0.416666666667
    , Op.newtRP (\x -> 12*x^4 -5*x^3 + 5) 400 =~ Nothing
    , Op.newtRP (\x -> 12*x^4 -5*x^3) 400 =~ Just 0.416666666667
    ]

  let v1 = U.fromList [1.2,3.4,4.5] :: U.Vector Double
  let v2 = U.fromList [2.5,3.6,1.2] :: U.Vector Double

  -- let p = V.fromList ([26,27,42,10,14,19,44,33,27,44,35,31,10,44]::[Integer]) 
  -- print $ Op.qsort p 
  -- print p

  ("Vector maths"::String) `qTest` 
    [ v1 `dot` v2 =~ 20.64

    , v1 +^ v2 =~ U.fromList [3.7,7.0,5.7]
    , v1 +^ v2 /~ U.fromList [3.75,7.0,5.7]

    , v1 -^ v2 =~ U.fromList [-1.3,-0.2,3.3]
    , v1 -^ v2 /~ U.fromList [-1.3,-0.2,3.2]

    , v1 *^ (-2.0) =~ U.fromList [-2.4,-6.8,-9.0]

    , v1 /^ 0.5 =~ U.fromList [2.4,6.8,9.0]

    , Nu.interp (U.fromList [2.0,3.0]) (U.fromList [10.0,15.0]) 0.25 =~ U.fromList [4.0,6.0]

    , Nu.grad (\x -> (x U.! 0) - (x U.! 1)*5.0 + (x U.! 2)**2) (U.fromList [5.0,2.0,-4.0]) =~ U.fromList [1.0,-5.0,-8.0]

    , Nu.negGrad (\x -> (x U.! 0) - (x U.! 1)*5.0 + (x U.! 2)**2) (U.fromList [5.0,2.0,-4.0]) =~ U.fromList [-1.0,5.0,8.0]
    ]

  ("Optima"::String) `qTest` 
    [ Op.bPhase (
    \x -> ((x U.! 0)-2.5)**2.0/25.0 + ((x U.! 1)-4.5)**2.0/100.0) 
      (U.fromList [3.5, 2.5]) (U.fromList [-1.0, 0.5]) =~ Just (0.4096,1.6384)

    , Op.lineSearch (
    \x -> ((x U.! 0)-2.5)**2.0/25.0 + ((x U.! 1)-4.5)**2.0/100.0)
    (U.fromList [3.5, 2.5]) (U.fromList [-1.0, 0.5]) 0.512 2.048 =~ Just (U.fromList [2.323530954719283,3.0882345226403585])

    , Op.lineOptima (
    \x -> ((x U.! 0)-2.5)**2.0/25.0 + ((x U.! 1)-4.5)**2.0/100.0)
      (U.fromList [3.5, 2.5]) (U.fromList [-1.0, 0.5]) =~ Just (U.fromList [2.3235300091300894,3.0882349954349553])

    , Op.conjGradPR (
    \x -> ((x U.! 0) - 3.0)**4.0 + ((x U.! 1) - 4.0)**2.0 + ((x U.! 2) - 2.0)**2.0 + ((x U.! 2) - 2.0)**4.0 + 10.0 ) (U.fromList [4.2,2.0,0.75]) =~ Just (U.fromList [2.9959575,4.0,2.0])
    ]

