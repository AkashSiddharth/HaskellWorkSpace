module Rate where
import Data.List
rate :: (Ord a) => [a] -> [(Int, a)]
rank :: Int -> [Int] -> [[a]] -> [[Int]]
rate [] = []
rate list = zip (concat(rank 1 len occ)) slist
            where
              slist = sort list
              l = length list
              len = [1..l]
              occ = group slist
rank ind len (x:xs) =  (replicate (length x) ind) : rank (ind + (length x)) (drop (length x) len) xs