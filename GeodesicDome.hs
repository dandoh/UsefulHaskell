import           System.Environment
import           System.Exit

tau :: Float
tau = 1.618033988749895

type Point = (Float, Float, Float)
type Triangle = (Point, Point, Point)

vertices :: [Point]
vertices =
    [
        (1, tau, 0), (-1, tau, 0), (1, -tau, 0), (-1, -tau, 0),
        (0, 1, tau), (0, -1, tau), (0, 1, -tau), (0, -1, -tau),
        (tau, 0, 1), (-tau, 0, 1), (tau, 0, -1), (-tau, 0, -1)
    ]

faces :: [Triangle]
faces = map toTriangle faceIndicies
  where
    toTriangle (ix, iy, iz) = (vertices !! ix, vertices !! iy, vertices !! iz)
    faceIndicies =
        [
            (0, 1, 4), (1, 9, 4), (4, 9, 5), (5, 9, 3), (2, 3, 7),
            (3, 2, 5), (7, 10, 2), (0, 8, 10), (0, 4, 8), (8, 2, 10),
            (8, 4, 5), (8, 5, 2), (1, 0, 6), (11, 1, 6), (3, 9, 11),
            (6, 10, 7), (3, 11, 7), (11, 6, 7), (6, 0, 10), (9, 1, 11)
        ]

interpolate :: Point -> Point -> Float -> Point
interpolate (x0, y0, z0) (x1, y1, z1) t = (x0 + (x1 - x0) * t, y0 + (y1 - y0) * t, z0 + (z1 - z0) * t)



projectToSphere :: Point -> Point
projectToSphere (x, y, z) = (x * t, y * t, z * t)
  where
    vlength = sqrt (x^2 + y^2 + z^2)
    t = 1 / vlength

subdivide :: Int -> Triangle -> [Triangle]
subdivide n (a, b, c) = concatMap makeTriangles . map fromIntegral $ [0..n - 1]
  where
    makeTriangles i =
        let
            f01                  = interpolate a b i
            f02                  = interpolate a c i
            f11                  = interpolate a b (i + 1)
            f12                  = interpolate a c (i + 1)
            makeTopTriangle j    = (interpolate f01 f02 j, interpolate f01 f02 (j + 1), interpolate f11 f12 (j + 1))
            makeBottomTriangle j = (interpolate f11 f12 j, interpolate f11 f12 (j + 1), interpolate f01 f02 j)
        in map makeTopTriangle [0..i - 1] ++ map makeBottomTriangle [0..i]


dome :: Int -> [Triangle]
dome n = map (onSphere) . concatMap (subdivide n) $ faces
  where
    onSphere (a, b, c) = (projectToSphere a, projectToSphere b, projectToSphere c)

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then do
        print "Please provide the number of subdivision, for example: ./GeodesicDome 5"
        exitWith (ExitFailure 1)
    else
        print . dome . read . (!! 0) $ args
