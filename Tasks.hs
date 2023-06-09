{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{------------------------------------------------------------------------------}
{- | Модуль с описанием решения некоторых задач по модели квантовых вычислений.

   Организация: ООО «А-Я эксперт»
   Автор:       Душкин Р. В.
   Проект:      Квантовые вычисления и функциональное программирование
                                                                              -}
{------------------------------------------------------------------------------}

module Tasks
(
  notHZH,
  swapQubits,
  deutsch
)
where

{-[ СЕКЦИЯ ИМПОРТА ]-----------------------------------------------------------}

import Data.Complex

import Circuit

{-[ ФУНКЦИИ ]------------------------------------------------------------------}

-- | Функция, демонстрирующая реализацию гейта НЕ через гейты Адамара и смены
--   фазы.
notHZH :: Qubit Double -> IO ()
notHZH q = (toVector q) |> gateH
                        |> gateZ
                        |> gateH
                        >>> (measure . fromVector 1)
                        >>= putStrLn

-- | Сервисная функция для вывода на экран таблицы истинности для функции
--   `swapQubits`.
truthTableSQ :: IO ()
truthTableSQ = sequence_ [swapQubits q1 q2 >>= putStrLn | q1 <- basis,
                                                          q2 <- basis]
  where
    basis = [qubitZero, qubitOne]

-- | Функция, реализующая квантовую схему, которая обменивает квантовые
--   состояния у двух заданных кубитов.
swapQubits :: Qubit Double -> Qubit Double -> IO String
swapQubits q1 q2 = toVector (entangle q1 q2) |> gateCNOT
                                             |> gateCNOT'
                                             |> gateCNOT
                                             >>> (measure . fromVector 2)

-- | Определение гейта «Контролируемое-НЕ», в котором контролирующий кубит
--   является вторым в паре кубитов.
gateCNOT' :: Matrix (Complex Double)
gateCNOT' = [[1.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0],
             [0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 1.0 :+ 0.0],
             [0.0 :+ 0.0, 0.0 :+ 0.0, 1.0 :+ 0.0, 0.0 :+ 0.0],
             [0.0 :+ 0.0, 1.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0]]

-- | Сервисная функция для тестирования алгоритма Дойча.
testDeutsch :: IO ()
testDeutsch = mapM_ deutsch [unitaryF1, unitaryF2, unitaryF3, unitaryF4]

-- | Функция, реализующая квантовый алгоритм Дойча, который за один
--   вычислительный шаг осуществляет проверку, является ли заданная функция,
--   выраженная как унитарное преобразование, константной или сбалансированной.
--   Функция должна быть бинарной и от одного аргумента (всего может быть 4
--   вида таких функций).
deutsch :: Matrix (Complex Double) -> IO ()
deutsch f = do (result:_) <- circuit
               case result of
                 '0' -> putStrLn "Функция f константна."
                 '1' -> putStrLn "Функция f сбалансирована."
                 _   -> return ()
  where
    gateH2  = gateH <++> gateH
    circuit = toVector (entangle qubitZero qubitOne) |> gateH2
                                                     |> f
                                                     |> gateH2
                                                     >>> (measure . fromVector 2)

{-
deutsch' f = if f 0 == f 1
               then putStrLn "Функция f константна."
               else putStrLn "Функция f сбалансирована."
-}

-- | Унитарное преобразование для представления квантового оракула функции
--   \f x = 0\.
unitaryF1 :: Matrix (Complex Double)
unitaryF1 = [[1.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0],
             [0.0 :+ 0.0, 1.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0],
             [0.0 :+ 0.0, 0.0 :+ 0.0, 1.0 :+ 0.0, 0.0 :+ 0.0],
             [0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 1.0 :+ 0.0]]

-- | Унитарное преобразование для представления квантового оракула функции
--   \f x = 1\.
unitaryF2 :: Matrix (Complex Double)
unitaryF2 = [[0.0 :+ 0.0, 1.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0],
             [1.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0],
             [0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 1.0 :+ 0.0],
             [0.0 :+ 0.0, 0.0 :+ 0.0, 1.0 :+ 0.0, 0.0 :+ 0.0]]

-- | Унитарное преобразование для представления квантового оракула функции
--   \f x = x\.
unitaryF3 :: Matrix (Complex Double)
unitaryF3 = gateCNOT

-- | Унитарное преобразование для представления квантового оракула функции
--   \f x = not x\.
unitaryF4 :: Matrix (Complex Double)
unitaryF4 = [[0.0 :+ 0.0, 1.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0],
             [1.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0],
             [0.0 :+ 0.0, 0.0 :+ 0.0, 1.0 :+ 0.0, 0.0 :+ 0.0],
             [0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 1.0 :+ 0.0]]

{-[ КОНЕЦ МОДУЛЯ ]-------------------------------------------------------------}
