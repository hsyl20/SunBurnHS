{-# LANGUAGE ExistentialQuantification #-}

module Graphics.SunBurn.Objects
   ( Hit (..)
   , Ray (..)
   , Color (..)
   , Vector3D (..)
   , Plane(..)
   , Sphere(..)
   , Hittable (..)
   , Object(..)
   , ViewPlane(..)
   , renderScene
   , Scene(..)
   )
   where

import Data.Maybe (mapMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)

data Hit = Hit
   { hitPoint  :: Vector3D
   , hitNormal :: Vector3D
   } deriving (Show)

data Vector3D = Vector3D Double Double Double deriving (Show)

infixl 7 *.
(*.) :: Double -> Vector3D -> Vector3D
(*.) c (Vector3D x y z) = Vector3D (c*x) (c*y) (c*z)

infixl 7 .*
(.*) :: Vector3D -> Double -> Vector3D
(.*) (Vector3D x y z) c = Vector3D (c*x) (c*y) (c*z)

infixl 7 ./
(./) :: Vector3D -> Double -> Vector3D
(./) (Vector3D x y z) c = Vector3D (x/c) (y/c) (z/c)


infixl 7 .*.
(.*.) :: Vector3D -> Vector3D -> Double
(.*.) (Vector3D a b c) (Vector3D x y z) = a*x + b*y + c*z


infixl 6 .-.
(.-.) :: Vector3D -> Vector3D -> Vector3D
(.-.) (Vector3D a b c) (Vector3D x y z) = Vector3D (a-x) (b-y) (c-z)

infixl 6 .+.
(.+.) :: Vector3D -> Vector3D -> Vector3D
(.+.) (Vector3D a b c) (Vector3D x y z) = Vector3D (a+x) (b+y) (c+z)

data Color
   = RGBColor Int Int Int
   deriving (Show)

data Ray = Ray
   { rayOrigin    :: Vector3D
   , rayDirection :: Vector3D
   } deriving (Show)

class Hittable o where
   hit :: o -> Ray -> Maybe (Double,Hit) -- ^ Indicate if the object is hit by the ray (returns hit info)


data Plane = Plane
   { planePoint   :: Vector3D
   , planeNormal  :: Vector3D
   , planeEpsilon :: Double
   } deriving (Show)

instance Hittable Plane where
   hit p ray = if t > planeEpsilon p
         then Just (t, Hit hp (planeNormal p))
         else Nothing
      where
         t = (planePoint p .-. rayOrigin ray) .*. planeNormal p / (rayDirection ray .*. planeNormal p)
         hp = rayOrigin ray .+. (t *. rayDirection ray)


data Sphere = Sphere
   { sphereCenter  :: Vector3D
   , sphereRadius  :: Double
   , sphereEpsilon :: Double
   } deriving (Show)

instance Hittable Sphere where
   hit s ray = case (disc,t1,t2) of
         (d,_,_) | d < 0.0             -> Nothing
         (_,t,_) | t > sphereEpsilon s -> Just (t, Hit (hp t) (nm t))
         (_,_,t) | t > sphereEpsilon s -> Just (t, Hit (hp t) (nm t))
         _                             -> Nothing
      where
         temp = rayOrigin ray .-. sphereCenter s
         a    = rayDirection ray .*. rayDirection ray
         b    = 2.0 * (temp .*. rayDirection ray)
         c    = temp .*. temp - sphereRadius s * sphereRadius s
         disc = b * b - 4.0 * a * c
         e    = sqrt disc
         deno = 2.0 * a
         t1   = (-b - e) / deno
         t2   = (-b + e) / deno
         nm t = (temp .+. t *. rayOrigin ray) ./ sphereRadius s
         hp t = rayOrigin ray .+. t *. rayDirection ray




data ViewPlane = ViewPlane
   { vpHorizontalResolution   :: Int
   , vpVerticalResolution     :: Int
   , vpPixelSize              :: Double
   , vpGamma                  :: Float
   , vpGammaInv               :: Float
   }


data Scene = Scene
   { sceneObjects          :: [Object]
   , sceneBackgroundColor  :: Color
   }

data Object = forall a. Hittable a => Object a

-- | Trace the ray and find the first hit (TODO: should return a colorable object)
traceMin :: [Object] -> Ray -> Maybe (Double,Hit)
traceMin os ray = headMaybe . sortBy (comparing fst) . mapMaybe hit' $ os
   where
      headMaybe [] = Nothing
      headMaybe (x:_) = Just x
      hit' (Object o) = hit o ray


renderScene :: Scene -> ViewPlane -> Double -> [[Color]]
renderScene scn vp zw = 
      fmap trace <$> [ [ (c,r) | c <- [0 .. vpHorizontalResolution vp - 1]] | r <- [0 .. vpVerticalResolution vp - 1]]
   where
      makeRay c r = Ray (Vector3D x y zw) (Vector3D 0.0 0.0 (-1.0))
         where
            x = vpPixelSize vp * (fromIntegral c - 0.5 * (fromIntegral (vpHorizontalResolution vp) - 1.0))
            y = vpPixelSize vp * (fromIntegral r - 0.5 * (fromIntegral (vpVerticalResolution   vp) - 1.0))

      trace (c,r) = case traceMin (sceneObjects scn) (makeRay c r) of
         Nothing -> sceneBackgroundColor scn
         Just _  -> RGBColor 255 0 0
