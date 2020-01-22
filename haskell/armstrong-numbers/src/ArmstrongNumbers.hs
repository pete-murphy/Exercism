module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n = 
  let ns = digits n
      len = length ns
   in n == sum (map (^ len) ns)

digits :: Integral a => a -> [a]
digits num = go num []
  where
    go n acc 
      | n < 10 = n : acc
      | otherwise = go n' (m : acc)
        where (n',m) = n `divMod` 10
