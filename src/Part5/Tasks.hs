module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = foldl f (f acc x) xs

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr prependMapped []
  where
    prependMapped x acc = f x : acc

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldr appendMapped []
  where
    appendMapped x acc = f x ++ acc

myConcat :: [[a]] -> [a]
myConcat = myFoldr appendList []
  where
    appendList xs acc = xs ++ acc

myReverse :: [a] -> [a]
myReverse = myFoldr prependReversed []
  where
    prependReversed x acc = acc ++ [x]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldr filterAcc []
  where
    filterAcc x acc
      | p x       = x : acc
      | otherwise = acc

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = foldr partitionAcc ([], [])
  where
    partitionAcc x (ts, fs)
      | p x       = (x : ts, fs)
      | otherwise = (ts, x : fs)
