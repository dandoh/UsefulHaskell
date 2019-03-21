import           System.Random (randomRIO)

sampleOne :: [a] -> IO a
sampleOne xs = do 
    id <- randomRIO $ (0, length xs - 1)
    return (xs !! id)


sampleMany :: [a] -> Int -> IO [a]
sampleMany xs n = 
    let 
        candidates = [0..length xs - 1]
        go accum 0 = accum
        go accum n = do
            takens <- accum
            let notTakens = filter (not . flip elem takens) candidates
            if null notTakens then accum
            else do
                nxt <- sampleOne notTakens
                let newTakens = nxt : takens
                go (return newTakens) (n - 1)
    in do
        ids <- go (return []) n
        return . map (xs !!) $ ids

main :: IO ()
main = do
    xs <- sampleMany [1, 2, 3] 2
    print xs

