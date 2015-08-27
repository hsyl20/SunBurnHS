import Graphics.SunBurn.Objects

main :: IO ()
main = renderPng scn vp 150 "test.png"
   where
      o1 = Sphere (Vector3D 0 0 20) 10
      o2 = Sphere (Vector3D 0 15 20) 15
      vp = ViewPlane 150 150 0.3 1 1
      scn = Scene [Object o1, Object o2] (RGBColor 0 255 0)
