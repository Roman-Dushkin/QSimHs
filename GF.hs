{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{------------------------------------------------------------------------------}
{- | Модуль с описанием функций для реализации задачи факторизации (разложения
     заданного числа на простые множители) при помощи квантового алгоритма
     Гровера.

   Организация: ООО «А-Я эксперт»
   Автор:       Душкин Р. В.
   Проект:      Квантовые вычисления и функциональное программирование
                                                                              -}
{------------------------------------------------------------------------------}

module GF
(
  main
)
where

{-[ СЕКЦИЯ ИМПОРТА ]-----------------------------------------------------------}

import Control.Monad (replicateM, unless)
import Data.Char (isDigit, toLower)
import Data.Complex (Complex)
import Data.Function (on)
import Data.List ((!!), elemIndex, isPrefixOf)
import Data.Maybe (fromJust)
import Data.Numbers.Primes (primes, isPrime, primeFactors)

import Circuit
import Gate
import Qubit

{-[ ФУНКЦИИ ]------------------------------------------------------------------}

-- | Функция, возвращающая список пар простых чисел, из которых первое строго
--   меньше второго.
pairsOfPrimes :: Integral a => [(a, a)]
pairsOfPrimes = [(x, y) | y <- primesWO2, x <- takeWhile (< y) primesWO2]
  where
    primesWO2 = tail primes

-- | Список чисел, которые могут быть разложены на простые множители при помощи
--   алгоритма Шора (они являются произведением ровно двух различных простых
--   чисел, отличающихся от 2).
goodNumbers :: Integral a => [a]
goodNumbers = map (uncurry (*)) pairsOfPrimes

-- | Функция, которая строит оракул для поиска заданного числа в списке
--   правильных чисел `goodNumbers`. Первым аргументом принимает количество
--   кубитов, для которого необходимо построить оракул (2 в степени этого
--   количества должно быть больше, чем индекс числа для факторизации в списке
--   хороших чисел). Число для факторизации должно подаваться вторым аргументом,
--   и это должно быть хорошее число (присутствующее в списке), иначе функция
--   впадёт в бесконечный поиск несуществующего числа в бесконечном списке.
makeOracle :: Integral a => Int -> a -> Matrix (Complex Double)
makeOracle q m = matrixToComplex $ map makeLine [1..2^q]
  where
    zeroes = replicate (2^q) 0
    index  = 1 + fromJust (elemIndex m goodNumbers)
    
    makeLine :: Int -> [Int]
    makeLine i = changeElement zeroes i ((-1)^(if i == index then 1 else 0))

-- | Сервисная функция, которая возвращает значение \True\, если заданное целое
--   число является квадратом.
isSquare :: Integer -> Bool
isSquare i = round (sqrt $ fromInteger i) ^ 2 == i

-- | Сервисная функция для вычисления логарифма по основанию 2, при этом
--   возвращается только целове число (производится округление вверх), поскольку
--   при помощи этой функции получается количество потребных кубитов.
log2 :: Integer -> Int
log2 = ceiling . logBase 2 . fromInteger

-- | Сервисная функция для вычисления верхней оценки количества требуемых для
--   выполнения итераций Гровера.
nofIterations :: Integer -> Int
nofIterations i = ceiling (pi/4 * sqrt (fromInteger i))

-- | Сервисная функция для преобразования строки, представляющей двоичное число,
--   в число.
binToNumber :: String -> Int
binToNumber = sum . zipWith (*) (map (2^) [0..]) . map (read . (:[])) . reverse

-- | Функция, реализующая гейт диффузии.
diffusion :: Int -> Matrix (Complex Double)
diffusion q = 2 <*:> (qubitPlusQ |><| qubitPlusQ) <-> gateIn q
  where
    qubitPlusQ = toVector $ foldl1 entangle $ replicate q qubitPlus

-- | Функция, реализающая квантовую схему алгоритма Гровера. Первым параметром
--   получает оракул. Вторым параметром получает количество кубитов в схеме.
--   Третьим параметром получает количество итераций.
grover :: Matrix (Complex Double) -> Int -> Int -> IO String
grover f q n = initial |> gateHn q
                       |> iteration
                       >>> (measure . fromVector q)
  where
    initial   = toVector $ foldr1 entangle $ replicate q qubitZero
    iteration = foldr1 (<**>) $ replicate n (f <**> diffusion q)

-- | Служебная функция для осуществления одного вызова алгоритма Гровера для
--   факторизации заданного числа.
calculatePrimeFactors :: Integral a => a -> IO (a, a)
calculatePrimeFactors m = do let q'     = log2 $ toInteger (1 + length (takeWhile (/= m) goodNumbers))
                                 q      = if q' == 0 then 1 else q'
                                 oracle = makeOracle q m
                             result <- grover oracle q $ nofIterations (2^q)
                             return $ pairsOfPrimes !! binToNumber result

-- | Служебная функция для расчёта сложности выполнения алгоритма. Первым
--   элементом возвращаемой пары является сложность классического алгоритма.
--   Вторым, соответственно, является сложность квантового алгоритма.
calculateComplexity :: Integral a => a -> (Int, Int)
calculateComplexity m = (2^q `div` 2, nofIterations (2^q))
  where
    q' = log2 $ toInteger (1 + length (takeWhile (/= m) goodNumbers))
    q  = if q' == 0 then 1 else q'

-- | Функция, которая осуществляет монадическое действие до тех пор, пока не
--   будет выполнено условие (предикат). Возвращает результат действия,
--   удовлетворяющий предикат, а заодно и количество повторений действия.
repeatUntil :: Monad m => Int -> m a -> (a -> Bool) -> m (a, Int)
repeatUntil counter action condition = do let counter' = counter + 1
                                          result <- action
                                          if condition result
                                            then return (result, counter')
                                            else repeatUntil counter' action condition

-- | Сервисная функция, которая выводит на экран заключение об эффективности
--   квантового алгоритма по сравнению с классическим.
showComplexity :: (Int, Int) -> IO ()
showComplexity (c, q) | r < 1.0   = putStrLn ("Квантовый алгоритм отработал менее эффективно классического в " ++ show (ceiling $ recip r) ++ " раз.")
                      | r == 1.0  = putStrLn  "Эффективность квантового и классического алгоритмов одинакова."
                      | otherwise = putStrLn ("Превышение эффективности квантового алгоритма над классическим: в " ++ show (ceiling r) ++ " раз.")
  where
    r  = ((/) `on` (fromInteger . toEnum)) c q

-- | Главная функция модуля, в которой осуществляется взаимодействие с
--   пользователем, расчёт результата и вывод его на экран.
main :: IO ()
main = do putStr "Введите число для факторизации: "
          mStr <- getLine
          unless (map toLower mStr `isPrefixOf` "quit") $
            if not (all isDigit mStr) || null mStr
              then putStrLn "Надо вводить число, а не что-то иное.\n" >> main
              else do let m = read mStr
                      if m `mod` 2 == 0
                        then putStrLn "Введённое число чётно.\n" >> main
                        else if isPrime m
                               then putStrLn "Введённое число простое.\n" >> main
                               else if length (primeFactors m) > 2
                                      then putStrLn "У введённого числа больше двух простых делителей.\n" >> main
                                      else if isSquare m
                                             then putStrLn "Введённое число является квадратом.\n" >> main
                                             else do let (cc, cq) = calculateComplexity m
                                                     ((x, y), count) <- repeatUntil 0 (calculatePrimeFactors m) (\(x, y) -> x * y == m)
                                                     putStrLn ("Решение найдено: " ++ show m ++ " = " ++ show x ++ " * " ++ show y)
                                                     putStrLn ("Алгоритм Гровера был вызван " ++ show count ++ " раз.")
                                                     showComplexity (cc, cq * count)
                                                     putStrLn ""
                                                     main

{-[ КОНЕЦ МОДУЛЯ ]-------------------------------------------------------------}
