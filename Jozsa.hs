{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{------------------------------------------------------------------------------}
{- | Модуль с описанием функций для реализации квантового алгоритма Дойча-Йожи.

   Организация: ООО «А-Я эксперт»
   Автор:       Душкин Р. В.
   Проект:      Квантовые вычисления и функциональное программирование
                                                                              -}
{------------------------------------------------------------------------------}

module Jozsa
(
  jozsa,
  main
)
where

{-[ СЕКЦИЯ ИМПОРТА ]-----------------------------------------------------------}

import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Data.Complex (Complex(..))
import Data.List (group, sort)

import Circuit
import Gate
import Qubit

{-[ ФУНКЦИИ ]------------------------------------------------------------------}

-- | Служебная функция для построения оракула из функции типа \[Bool] -> Bool\.
makeOracle :: ([Bool] -> Bool) -> Int -> Matrix (Complex Double)
makeOracle f n = matrixToComplex $ zipWith makeLine domain [1..2^n]
  where
    zeroes = replicate (2^n) 0
    domain = replicateM n [False, True]
    
    makeLine :: [Bool] -> Int -> [Int]
    makeLine x i = changeElement zeroes i ((-1)^(if f x then 1 else 0))
    
-- | Основная функция модуля, демонстрирующая алгоритм Дойча-Йожи.
jozsa :: Matrix (Complex Double) -> Int -> IO String
jozsa f n = initial |> gateHn n
                    |> f
                    |> gateHn n
                    >>> (measure . fromVector n)
  where
    initial = toVector $ foldl1 entangle $ replicate n qubitZero

-- | Функция для построения гистограммы результатов заданной квантовой схемы.
--   Запускает квантовую схему требуемое количество раз, собирает результаты и
--   строит из них ассоциативный список пар (частота, результат).
histogram :: (Monad m, Ord a) => m a -> Int -> m [(Int, a)]
histogram qs n = do l <- replicateM n qs
                    return $ map (length &&& head) $ group $ sort l

-- | Главная функция модуля, которая строит гистограмму результатов измерения
--   квантового регистра, запуская алгоритм Дойча-Йожи заданное количество раз.
--   В данном случае изучаются все 9 разработанных оракулов.
main :: Int -> IO [[(Int, String)]]
main n = mapM (\i -> histogram (jozsa (makeOracle (oracle i) 3) 3) n) [1..9]

-- | Симулятор оракула. Первый аргумент используется в качестве индекса того
--   оракула, который надо возвратить для исследований.
oracle :: Int -> [Bool] -> Bool
oracle 1 [_, _, _] = False
oracle 2 [x, y, z] = x && y && z
oracle 3 [x, y, _] = x && y
oracle 4 [x, y, z] = x && (z || y && not z)
oracle 5 [x, _, _] = x
oracle 6 [x, y, z] = y && z || x && (not y || y && not z)
oracle 7 [x, y, z] = y || oracle 6 [x, y, z]
oracle 8 [x, y, z] = x || y || z
oracle 9 [_, _, _] = True

{-------------------------------------------------------------------------------
Результаты работы:

8/0: [[(1000000, "000")],
7/1:  [(561592,  "000"), (62964,   "001"), (62666,  "010"), (62108, "011"), (63144,  "100"), (62304, "101"), (62799,  "110"), (62423, "111")],
6/2:  [(249916,  "000"),                   (249794, "010"),                 (250264, "100"),                 (250026, "110")],
5/3:  [(62405,   "000"), (62373,   "001"), (62694,  "010"), (62098, "011"), (562886, "100"), (62517, "101"), (62505,  "110"), (62522, "111")],
4/4:                    [(1000000, "001")],
3/5:  [(62504,   "000"), (62706,   "001"), (62515,  "010"), (62525, "011"), (562153, "100"), (62357, "101"), (62386,  "110"), (62854, "111")],
2/6:  [(250329,  "000"),                   (249748, "010"),                 (250030, "100"),                 (249893, "110")],
1/7:  [(562946,  "000"), (62096,   "001"), (62240,  "010"), (62560, "011"), (62595,  "100"), (62763, "101"), (62628,  "110"), (62172, "111")],
0/8:  [(1000000, "000")]]
-------------------------------------------------------------------------------}

{-[ КОНЕЦ МОДУЛЯ ]-------------------------------------------------------------}
