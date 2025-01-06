module Part1.Tasks where

import Util(notImplementedYet)

-- Factorial 
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

normalizeAngle :: Double -> Double
normalizeAngle x = x - 2 * pi * fromIntegral (floor (x / (2 * pi)))

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = sum $ take 16 [((-1.0)**(fromIntegral n)) * ((normalizeAngle x)**(2*fromIntegral n + 1)) / fromIntegral (factorial (2*fromIntegral n + 1)) | n <- [0..]]

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = sum $ take 16 [((-1.0)**(fromIntegral n)) * ((normalizeAngle x)**(2*fromIntegral n)) / fromIntegral (factorial (2*fromIntegral n)) | n <- [0..]]

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD x y | y == 0    = abs x
          | otherwise = myGCD y (mod x y)

isLeapYear :: Integer -> Bool
isLeapYear year = (mod year 400 == 0) || (mod year 4 == 0 && mod year 100 /= 0)

getMonthLength :: Integer -> Integer -> Integer
getMonthLength 2 year
    | isLeapYear year = 29
    | otherwise = 28
getMonthLength month _
    | elem month [4, 6, 9, 11] = 30
    | otherwise = 31

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
    | month < 1 || month > 12 = False
    | day < 1 || day > (getMonthLength month year) = False
    | otherwise = True

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer

myPow _ 0 = 1
myPow x y = x * myPow x (y-1)

-- лист делителей числа н 
dividers n = [x | x <- [1..n], mod n x == 0]

-- является ли данное число простым?
-- идея: сделать список делитей и если в списке токмо 1 и н то всё нормально 
isPrime :: Integer -> Bool
isPrime x | x < 1     = False 
          | otherwise = dividers x == [1, x]

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
-- note: попробовать зипвис и анонимные функции ? 
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea points
  | length points < 3 = 0.0 
  | otherwise         = 0.5 * abs (shapeSum points)

shapeSum points = sum $ zipWith (\(x1, y1) (x2, y2) -> x1 * y2 - x2 * y1) points (tail (cycle points))

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
    | a + b <= c || a + c <= b || b + c <= a = -1
    | a^2 + b^2 == c^2 || a^2 + c^2 == b^2 || b^2 + c^2 == a^2 = 2
    | a^2 + b^2 < c^2 || a^2 + c^2 < b^2 || b^2 + c^2 < a^2 = 0
    | otherwise = 1
