{-# LANGUAGE NamedFieldPuns #-}
module Group
    (elements, o, identity, Group, formGroup, inverseElement, allSubgroups)
    where

import           Data.List    (find)
import           Data.List.HT
import           Data.Set     as Set (Set, fromList, member, toList)
import           Debug.Trace
{- |

Group consist of a set G with a binary operation `o` that satisfies:
- Close: `o` :: G -> G -> G
- Identity: exists e such that for all a in G: a `o` e = e `o` a = a
- Inverse element: exist b: a `o` b = e
- Associativity: (a `o` b) `o` c = c `o` (b `o` a)
-}
data Group a = Group
    { elements :: Set a
    , o        :: a -> a -> a
    , identity :: a
    }

satAssociativity :: Ord a => (Set a, a -> a -> a) -> Bool
satAssociativity (g, o) = all propAssoc allTriples
  where
    lst = toList g
    allTriples = [(x, y, z) | x <- lst, y <- lst, z <- lst]
    propAssoc (x, y, z) = (x `o` y) `o` z == x `o` (y `o` z)

satClosed :: Ord a => (Set a, a -> a -> a) -> Bool
satClosed (g, o) = all (flip member g . uncurry o) allPairs
  where
    lst = toList g
    allPairs = [(x, y) | x <- lst, y <- lst]

satInverse :: Ord a => (Set a, a -> a -> a) -> Bool
satInverse (g, o) = case findIdentity (g, o) of
    Just e ->
        all (propExistInverse e) lst
    Nothing ->
        False
  where
    lst = toList g
    propExistInverse e x = any ((== e) . (x `o`)) lst

findIdentity :: Ord a => (Set a, a -> a -> a) -> Maybe a
findIdentity (g, o) = find isIdentity lst
  where
    lst = toList g
    propIdentity e x = e `o` x == x && x `o` e == x
    isIdentity x = all (propIdentity x) lst

inverseElement :: Ord a => Group a -> a -> a
inverseElement (Group elements o identity) x = case find ((== identity) . (x `o`)) . toList $ elements of
    Just res ->
        res
    Nothing ->
        error "This should't happen"

genSubgroup :: Ord a => Group a -> a -> Group a
genSubgroup (Group elements o identity) x =
    if member x elements then
        Group
            { elements = fromList . takeUntil (== identity) . scanl1 o . repeat $ x
            , identity = identity
            , o = o
            }
    else
        error "Element is not in the group set"


allSubgroups :: Ord a => Group a -> [Group a]
allSubgroups group@(Group xs o identity) =
    map toGroup . rmvDup . map (elements . genSubgroup group) . toList $ xs
  where
    rmvDup = toList . fromList
    toGroup els = Group els o identity


formGroup :: Ord a => (Set a, a -> a -> a) -> Maybe (Group a)
formGroup cp = case (findIdentity cp, satInverse cp && satClosed cp && satAssociativity cp) of
    (Just e, True) ->
        Just $ Group { elements = fst cp, o = snd cp, identity = e}
    _ ->
        Nothing

main :: IO ()
main = do
    let cp = (fromList [0..5], \x y -> (x + y) `mod` 6)
    let maybeGroup = formGroup cp
    case maybeGroup of
        Just group -> do
            print $ map elements . allSubgroups $ group
        _ ->
            print $ "Not a group"
