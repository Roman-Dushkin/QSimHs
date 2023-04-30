{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{------------------------------------------------------------------------------}
{- | Модуль с описанием функций для реализации квантового алгоритма Гровера.

   Организация: ООО «А-Я эксперт»
   Автор:       Душкин Р. В.
   Проект:      Квантовые вычисления и функциональное программирование
                                                                              -}
{------------------------------------------------------------------------------}

module Grover
(
  grover,
  main
)
where

{-[ СЕКЦИЯ ИМПОРТА ]-----------------------------------------------------------}

import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Data.Complex (Complex, realPart)
import Data.Function (on)
import Data.List (sort, group)

import Circuit
import Gate
import Qubit

{-[ ФУНКЦИИ ]------------------------------------------------------------------}

-- | Специально подготовленный оракул для демонстрации алгоритма Гровера на
--   трёх кубитах.
oracle :: Matrix (Complex Double)
oracle = matrixToComplex [[1, 0, 0, 0, 0, 0, 0,  0],
                          [0, 1, 0, 0, 0, 0, 0,  0],
                          [0, 0, 1, 0, 0, 0, 0,  0],
                          [0, 0, 0, 1, 0, 0, 0,  0],
                          [0, 0, 0, 0, 1, 0, 0,  0],
                          [0, 0, 0, 0, 0, 1, 0,  0],
                          [0, 0, 0, 0, 0, 0, 1,  0],
                          [0, 0, 0, 0, 0, 0, 0, -1]]

-- | Ещё один оракул, который соответствует функции, возвращающей 1 для
--   нескольких (трёх) значений.
oracle' :: Matrix (Complex Double)
oracle' = matrixToComplex [[1,  0, 0, 0,  0, 0, 0,  0],
                           [0, -1, 0, 0,  0, 0, 0,  0],
                           [0,  0, 1, 0,  0, 0, 0,  0],
                           [0,  0, 0, 1,  0, 0, 0,  0],
                           [0,  0, 0, 0, -1, 0, 0,  0],
                           [0,  0, 0, 0,  0, 1, 0,  0],
                           [0,  0, 0, 0,  0, 0, 1,  0],
                           [0,  0, 0, 0,  0, 0, 0, -1]]

-- | Функция, реализующая гейт диффузии.
diffusion :: Matrix (Complex Double)
diffusion = 2 <*:> (qubitPlus3 |><| qubitPlus3) <-> gateIn 3
  where
    qubitPlus3 = toVector $ foldl1 entangle $ replicate 3 qubitPlus

-- | Основная функция модуля, демонстрирующая алгоритм Гровера на трёх кубитах.
grover :: Matrix (Complex Double) -> IO String
grover f = initial |> gateHn 3
                   |> f |> diffusion
                   |> f |> diffusion
                   >>> (measure . fromVector 3)
  where
    initial = toVector $ foldr entangle qubitZero $ replicate 2 qubitZero

-- | Главная функция модуля, которая строит гистограмму результатов измерения
--   квантового регистра, запуская алгоритм Гровера заданное количество раз.
main f n = do l <- replicateM n $ grover f
              return $ map (length &&& head) $ group $ sort l

{-[ КОНЕЦ МОДУЛЯ ]-------------------------------------------------------------}
