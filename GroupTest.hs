import Data.Set (fromList)
import Group

main :: IO ()
main = do
  let z4 = groupZ 4
  let z2 = groupZ 2
  let g = z4 `prod` z2
  print . map elements . cyclicSubgroups $ g
