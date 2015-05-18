module Graphics.SunBurn.Objects
   ( Hit (..)
   , Ray (..)
   , Color (..)
   , Vector3D (..)
   , Plane(..)
   , Sphere(..)
   , Hittable (..)
   )
   where


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
   hit :: o -> Ray -> Maybe (Double,Hit)


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



