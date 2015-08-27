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
   , Scene(..)
   , renderScene
   , renderPixel
   , renderPng
   )
   where

import Data.Maybe (mapMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)

import Codec.Picture

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

data Hit = Hit
   { hitNormal   :: Vector3D
   , hitDistance :: Double
   } deriving (Show)

localHitPoint :: Ray -> Hit -> Vector3D
localHitPoint r h = rayOrigin r .+. (hitDistance h *. rayDirection r)

epsilon :: Double
epsilon = 10e-6

data Color
   = RGBColor Int Int Int
   deriving (Show)

data Ray = Ray
   { rayOrigin    :: Vector3D
   , rayDirection :: Vector3D
   } deriving (Show)

class Hittable o where
   hit :: o -> Ray -> Maybe Hit -- ^ Indicate if the object is hit by the ray (returns hit info)


data Plane = Plane
   { planePoint   :: Vector3D
   , planeNormal  :: Vector3D
   } deriving (Show)

instance Hittable Plane where
   hit p ray = if t > epsilon
         then Just $ Hit (planeNormal p) t
         else Nothing
      where
         t = (planePoint p .-. rayOrigin ray) .*. planeNormal p / (rayDirection ray .*. planeNormal p)


data Sphere = Sphere
   { sphereCenter  :: Vector3D
   , sphereRadius  :: Double
   } deriving (Show)

instance Hittable Sphere where
   hit s ray = case (d,t1,t2) of
         _ | d < 0.0      -> Nothing
         _ | t1 > epsilon -> Just $ Hit (norm t1) t1
         _ | t2 > epsilon -> Just $ Hit (norm t2) t2
         _                -> Nothing
      where
         r    = rayOrigin ray .-. sphereCenter s
         a    = rayDirection ray .*. rayDirection ray
         b    = 2.0 * (r .*. rayDirection ray)
         c    = r .*. r - sphereRadius s * sphereRadius s
         d    = b * b - 4.0 * a * c
         t1   = (-b - sqrt d) / (2.0 * a)
         t2   = (-b + sqrt d) / (2.0 * a)
         norm t = (r .+. t *. rayOrigin ray) ./ sphereRadius s




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
traceMin :: [Object] -> Ray -> Maybe Hit
traceMin os ray = headMaybe . sortBy (comparing hitDistance) . mapMaybe hit' $ os
   where
      headMaybe [] = Nothing
      headMaybe (x:_) = Just x
      hit' (Object o) = hit o ray


renderScene :: Scene -> ViewPlane -> Double -> [[Color]]
renderScene scn vp zw = fmap render <$> pxs
   where
      render = uncurry (renderPixel scn vp zw)
      pxs = [ [ (c,r) | c <- [0 .. vpHorizontalResolution vp - 1]] | r <- [0 .. vpVerticalResolution vp - 1]]

renderPixel :: Scene -> ViewPlane -> Double -> Int -> Int -> Color
renderPixel scn vp zw = curry trace
   where
      makeRay c r = Ray (Vector3D x y zw) (Vector3D 0.0 0.0 (-1.0))
         where
            x = vpPixelSize vp * (fromIntegral c - 0.5 * (fromIntegral (vpHorizontalResolution vp) - 1.0))
            y = vpPixelSize vp * (fromIntegral r - 0.5 * (fromIntegral (vpVerticalResolution   vp) - 1.0))

      trace (c,r) = case traceMin (sceneObjects scn) (makeRay c r) of
         Nothing -> sceneBackgroundColor scn
         Just _  -> RGBColor 255 0 0


renderPng :: Scene -> ViewPlane -> Double -> String -> IO ()
renderPng scn vp zw path = writePng path $ generateImage (\x y -> toPixel $ renderPixel scn vp zw x y) w h
   where 
      w = vpHorizontalResolution vp
      h = vpVerticalResolution vp
      toPixel (RGBColor r g b) = PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)
