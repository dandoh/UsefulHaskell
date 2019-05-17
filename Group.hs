import Data.Set (Set, toList, member, fromList)
import Data.List (find)

type Group a = (Set a, a -> a -> a)

isClosed :: Ord a => Group a -> Bool
isClosed (g, o) = all (flip member g . uncurry o) allPairs
  where
    lst = toList g
    allPairs = [(x, y) | x <- lst, y <- lst]

hasIdentityAndInverse :: Ord a => Group a -> Bool
hasIdentityAndInverse (g, o) = case find isIdentity lst of
    Just e ->
        all (propExistInverse e) lst
    Nothing ->
        False
  where
    lst = toList g
    propIdentity e x = e `o` x == e && x `o` e == e
    isIdentity x = all (propIdentity x) lst
    propExistInverse e x = any ((== e) . (o x)) lst


isValid :: Ord a => Group a -> Bool
isValid group = isClosed group && hasIdentityAndInverse group


main :: IO ()
main = do
    let st = fromList [0..5]
        x `o` y = (x + y) `div` 6
    print $ isValid (st, o)
