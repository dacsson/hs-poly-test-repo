{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
    zeroImpl :: Int -> Int -> mx
    eyeImpl ::  Int -> mx
    multiplyImpl :: mx -> mx -> mx
    determinantImpl :: mx -> Int

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
    zeroImpl _ _ = 0
    eyeImpl _ = 1
    
instance Matrix [[Int]] where
    zeroImpl cols rows = replicate rows (replicate cols 0)
    eyeImpl size = [ [if row == col then 1 else 0
            | col <- [0..size-1]]
            | row <- [0..size-1] ]

instance Matrix (SparseMatrix Int) where
    zeroImpl cols rows = SparseMatrix cols rows empty    
    eyeImpl size = SparseMatrix size size $ fromList [((idx, idx), 1) | idx <- [0..size-1]] 

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye = eyeImpl 
-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero = zeroImpl 
-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix = multiplyImpl

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant = determinantImpl
