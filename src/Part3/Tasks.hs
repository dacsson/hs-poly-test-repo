module Part3.Tasks where

import Util (notImplementedYet)
import Data.Char
import Data.List
import Data.Ord

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
-- finc f x = [f x | x <- [x..]] why this works too?
finc f x = f x : finc f (x + 1)

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
-- как записать версию с [ ] ...
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq numbers = fst (maximumBy (comparing snd) frequency)
    where
      digits = concatMap (map digitToInt . show) numbers
      frequency = map (\x -> (head x, length x)) (group (sort digits)) 

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
-- uniq (x:xs) = x : uniq (filter (/= x) xs)
uniq (x:xs) = foldl (\acc x -> if x `elem` acc then acc else acc ++ [x]) [] xs

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f lst = [(key, [x | x <- lst, f x == key]) | key <- nub (map f lst)]
