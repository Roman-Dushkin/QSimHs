{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{------------------------------------------------------------------------------}
{- | Модуль с описанием функций для реализации квантового алгоритма Шора
     факторизации целого числа.

   Организация: ООО «А-Я эксперт»
   Автор:       Душкин Р. В.
   Проект:      Квантовые вычисления и функциональное программирование
                                                                              -}
{------------------------------------------------------------------------------}

module Shor
(
  main
)
where

{-[ СЕКЦИЯ ИМПОРТА ]-----------------------------------------------------------}

import Control.Arrow ((&&&), (***))
import Control.Monad (liftM, replicateM)
import Data.Bits (xor)
import Data.Complex (Complex(..))
import Data.List (inits, group, sort, unfoldr)
import Data.Ratio ((%), denominator)
import GHC.Real (Ratio((:%)))

import Circuit
import Gate
import Qubit

{-[ ИЗОМОРФНЫЕ ТИПЫ ДАННЫХ ]---------------------------------------------------}

-- | Тип для представления цепной дроби, в которую переводится отношение
--   задействованного количества кубитов к найденному в результате выполнения
--   квантовой процедуры числу.
newtype Chain = Chain
                {
                  unChain :: [Integer]
                }
  deriving (Eq, Show)

{-[ ФУНКЦИИ ]------------------------------------------------------------------}

-- | Количество попыток запуска алгоритма Шора для факторизации. Чем больше это
--   число, тем больше вероятность найти разложение с первой попытки, однако тем
--   дольше работает функция `main`.
nofShorAttempts :: Int
nofShorAttempts = 10

-- | Количество попыток запуска квантовой подпрограммы в рамках алгоритма Шора.
--   Чем больше это число, тем больше вероятность найти нужный период в первом
--   же запуске полного алгоритма Шора, однако тем медленнее работает функция
--   сбора вероятных периодов.
nofQFCalling :: Int
nofQFCalling = 3

-- | Число, которое будем факторизовывать при помощи квантового алгоритма Шора.
numberToFactor :: Int
numberToFactor = 21

-- | Число, при помощи которого будет осуществляться поиск разложения заданного
--   числа.
simpleNumber :: Int
simpleNumber = 2

-- | Количество вспомогательных кубитов, которое требуется для факторизации
--   заданного числа.
nofAncillas :: Int
nofAncillas = 5
-- ceiling ((log . fromInteger . toInteger) numberToFactor / log 2)

-- | Количество рабочих кубитов, которое требуется для факторизации заданного
--   числа.
nofWorkingQubits :: Int
nofWorkingQubits = 4
-- 2 * nofAncillas + 1

-- | Общее число потребных кубитов, которые требуются для факторизации заданного
--   числа.
nofQubits :: Int
nofQubits = nofWorkingQubits + nofAncillas

-- | Специальная функция, используемая в алгоритме Шора, период которой
--   необходимо найти. Пока в виде обычной функции, а не в виде квантового
--   оракула.
periodicFunction :: Int -> Int
periodicFunction x = simpleNumber^x `mod` numberToFactor

-- | Предварительно вычисленная константа для построения оракула в виде матрицы.
--   Число обозначает регистр кубитов в векторном представлении (в десятичном
--   выражении; то есть в этом регистре 9 кубитов), в который преобразуется
--   квантовый регистр, векторное представление которого соответствует номеру
--   числа в этом списке, при этом нумерация начинается с 0. То есть кубит
--   \|000000000>\ преобразуется в \|000000001>\ и т. д.
oracleList :: [Int]
oracleList = [  1,   0,   3,   2,   5,   4,   7,   6,   9,   8,  11,  10,  13,
               12,  15,  14,  17,  16,  19,  18,  21,  20,  23,  22,  25,  24,
               27,  26,  29,  28,  31,  30,  34,  35,  32,  33,  38,  39,  36,
               37,  42,  43,  40,  41,  46,  47,  44,  45,  50,  51,  48,  49,
               54,  55,  52,  53,  58,  59,  56,  57,  62,  63,  60,  61,  68,
               69,  70,  71,  64,  65,  66,  67,  76,  77,  78,  79,  72,  73,
               74,  75,  84,  85,  86,  87,  80,  81,  82,  83,  92,  93,  94,
               95,  88,  89,  90,  91, 104, 105, 106, 107, 108, 109, 110, 111,
               96,  97,  98,  99, 100, 101, 102, 103, 120, 121, 122, 123, 124,
              125, 126, 127, 112, 113, 114, 115, 116, 117, 118, 119, 144, 145,
              146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158,
              159, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139,
              140, 141, 142, 143, 171, 170, 169, 168, 175, 174, 173, 172, 163,
              162, 161, 160, 167, 166, 165, 164, 187, 186, 185, 184, 191, 190,
              189, 188, 179, 178, 177, 176, 183, 182, 181, 180, 193, 192, 195,
              194, 197, 196, 199, 198, 201, 200, 203, 202, 205, 204, 207, 206,
              209, 208, 211, 210, 213, 212, 215, 214, 217, 216, 219, 218, 221,
              220, 223, 222, 226, 227, 224, 225, 230, 231, 228, 229, 234, 235,
              232, 233, 238, 239, 236, 237, 242, 243, 240, 241, 246, 247, 244,
              245, 250, 251, 248, 249, 254, 255, 252, 253, 260, 261, 262, 263,
              256, 257, 258, 259, 268, 269, 270, 271, 264, 265, 266, 267, 276,
              277, 278, 279, 272, 273, 274, 275, 284, 285, 286, 287, 280, 281,
              282, 283, 296, 297, 298, 299, 300, 301, 302, 303, 288, 289, 290,
              291, 292, 293, 294, 295, 312, 313, 314, 315, 316, 317, 318, 319,
              304, 305, 306, 307, 308, 309, 310, 311, 336, 337, 338, 339, 340,
              341, 342, 343, 344, 345, 346, 347, 348, 349, 350, 351, 320, 321,
              322, 323, 324, 325, 326, 327, 328, 329, 330, 331, 332, 333, 334,
              335, 363, 362, 361, 360, 367, 366, 365, 364, 355, 354, 353, 352,
              359, 358, 357, 356, 379, 378, 377, 376, 383, 382, 381, 380, 371,
              370, 369, 368, 375, 374, 373, 372, 385, 384, 387, 386, 389, 388,
              391, 390, 393, 392, 395, 394, 397, 396, 399, 398, 401, 400, 403,
              402, 405, 404, 407, 406, 409, 408, 411, 410, 413, 412, 415, 414,
              418, 419, 416, 417, 422, 423, 420, 421, 426, 427, 424, 425, 430,
              431, 428, 429, 434, 435, 432, 433, 438, 439, 436, 437, 442, 443,
              440, 441, 446, 447, 444, 445, 452, 453, 454, 455, 448, 449, 450,
              451, 460, 461, 462, 463, 456, 457, 458, 459, 468, 469, 470, 471,
              464, 465, 466, 467, 476, 477, 478, 479, 472, 473, 474, 475, 488,
              489, 490, 491, 492, 493, 494, 495, 480, 481, 482, 483, 484, 485,
              486, 487, 504, 505, 506, 507, 508, 509, 510, 511, 496, 497, 498,
              499, 500, 501, 502, 503]

-- | Сервисная функция для построения списка, на основе которого строится оракул
--   для заданной функции. Данный список необходимо использовать в функции
--   `oracle` вместо вызова константы `oracleList`. Если вызвать данную функцию
--   следующим образом:
--
--   \makeOracleList 4 5 periodicFunction\
--
--   то в результате получится список, в точности равный списку `oracleList`.
makeOracleList :: Int -> Int -> (Int -> Int) -> [Int]
makeOracleList m n f = [x * 2^n + (y `xor` f x) | x <- [0..2^m - 1],
                                                  y <- [0..2^n - 1]]

-- | Подготовленный оракул для функции `periodicFunction` в двоичном
--   представлении.
oracle :: Matrix (Complex Double)
oracle = matrixToComplex $
           map (qubitFromInt nofQubits) $
           makeOracleList nofWorkingQubits nofAncillas periodicFunction

-- | Сервисная функция, которая готовит одну строку для создания оракула.
qubitFromInt :: Integral a => a -> Int -> [a]
qubitFromInt b n = changeElement (replicate (2^b) 0) (n + 1) 1

-- | Функция для построения гейта, выполняющего квантовое преобразование Фурье.
qft :: Int -> Matrix (Complex Double)
qft q = [[euler m n | m <- [1..2^q]] | n <- [1..2^q]]
  where
    euler m n = let power = (-2) * pi * (m - 1) * (n - 1) / 2^q
                in  cos power :+ sin power

-- | Основная функция модуля, которая демонстрирует квантовый алгоритм Шора для
--   факторизации целых чисел.
quantumFactoring :: Matrix (Complex Double) -> IO String
quantumFactoring o = initial |> gateHn nofWorkingQubits <++> gateIn nofAncillas
                             |> o
                             |> qft nofWorkingQubits <++> gateIn nofAncillas
                             >>> (measure . fromVector nofQubits)
  where
    initial = toVector $ foldr1 entangle $ replicate nofQubits qubitZero

-- | Функция, которая возвращает период по измеренному значению входного
--   (первого) квантового регистра.
getPeriod :: Integer -> Integer
getPeriod s = denominator $ fromChainLimited nofWorkingQubits $ toChain $ s % 2^nofWorkingQubits

-- | Функция, которая переводит дробь, обратную заданной, в цепную.
--   Предполагается, что аргумент меньше единицы.
toChain :: Rational -> Chain
toChain = Chain . unfoldr step
  where
   step r@(a :% b) | a == 0    = Nothing
                   | b < a     = Nothing
                   | otherwise = Just (m, b % a - m % 1) where m = b `div` a

-- | Функция, которая переводит цепную дробь в обычную.
fromChain :: Chain -> Rational
fromChain = foldr ((\a b -> rev $ b + a) . (% 1)) 0 . unChain
  where rev (a :% b) = b :% a

-- | Функция, которая находит приближённое значение отношения заданного в виде
--   цепной дроби числа, полученного в результате измерения выхода квантовой
--   процедуры, к периоду. При этом для найденного значения отношения число бит
--   в числителе и знаменателе получается не более \n\.
fromChainLimited :: Int -> Chain -> Rational
fromChainLimited n = last .
                       takeWhile ((< 2^n) . denominator) .
                       map (fromChain . Chain) .
                       inits .
                       unChain

-- | Функция для получения случайного периода посредством запуска заданное
--   количество раз квантового алгоритма Шора.
findRandomPeriod :: Int -> IO Integer
findRandomPeriod m = do l <- replicateM m $ quantumFactoring oracle
                        return $
                          foldr (lcm . getPeriod) 1 $
                          filter (/= 0) $
                          map snd $
                          filter (\(i, _) -> i >= m `div` 10) $
                          map (length &&& (binToInt . head)) $
                          group $
                          sort $
                          map (take nofWorkingQubits) l
  where
    binToInt s = sum $ zipWith (*) (map (2^) [0..]) (map (\c -> if c == '0' then 0 else 1) $ reverse s)

-- | Функция для сбора большинства возможных периодов для числа, которое
--   факторизуется при помощи алгоритма Шора.
collectPeriods :: Int -> Int -> IO [Integer]
collectPeriods n m = do l <- replicateM n $ findRandomPeriod m
                        return $
                          filter (\r -> simpleNumber^(r `div` 2) `mod` numberToFactor /= numberToFactor - 1) $
                          filter even $
                          map head $
                          group $
                          sort l

-- | Служебная функция, которая получает на вход период, а возвращает пару
--   делителей числа `numberToFactor`.
getFactors r = (gcd numberToFactor $ simpleNumber^(r `div` 2) - 1,
                gcd numberToFactor $ simpleNumber^(r `div` 2) + 1)

-- | Главная функция модуля, которая используется для запуска квантового
--   алгоритма Шора для факторизации числа `numberToFactor` и вывода на экран
--   его простых множителей.
main :: IO ()
main = do putStrLn ("Факторизуем число: " ++ show numberToFactor)
          putStrLn ("Используем для этого число: " ++ show simpleNumber)
          putStrLn ("Запускаем квантовый алгоритм факторизации " ++ show nofShorAttempts ++ " раз")
          rs <- collectPeriods nofShorAttempts nofQFCalling
          let fs = filter (\(f1, f2) -> f1 /= 1 && f2 /= 1) $ map getFactors rs
          case fs of
            ((f1, f2):_) -> putStrLn ("Ура, разложение найдено: " ++ show numberToFactor ++ " = " ++ show f1 ++ " * " ++ show f2)
            []           -> putStrLn "Увы, квантовый алгоритм Шора потерпел фиаско. Попробуйте запустить его ещё раз."

-- | Функция для проведения исследования по поводу вероятности нахождения ответа
--   в зависимости от количества запусков квантовой подпрограммы и самого
--   квантового алгоритма.
investigate :: Int -> Int -> IO Int
investigate n m = liftM sum $ replicateM 100 try
  where
    try = do rs <- collectPeriods n m
             let fs = filter (\(f1, f2) -> f1 /= 1 && f2 /= 1) $ map getFactors rs
             case fs of
               ((f1, f2):_) -> return 1
               []           -> return 0

-- | Служебная функция для массированного запуска исследований зависимости
--   вероятности нахождения результата по алгоритму Шора `investigate`.
runInvestigation :: [Int] -> [Int] -> IO ()
runInvestigation ns ms = mapM_ showIt [((n, m), investigate n m) | n <- ns, m <- ms]
  where
    showIt ((x, y), n) = do n' <- n
                            putStrLn ("(" ++ show x ++ ", " ++ show y ++ "): " ++ show n')

-- | Функция, которая написана специально для того, чтобы показать реализацию
--   алгоритма Шора от первого до последнего шага без использования квантовой
--   подпрограммы. Поиск периода осуществляется при помощи нахождения в списке
--   степеней заданного числа первой единицы.
classicFactoring :: Int -> [(Int, Int)]
classicFactoring m = map ((gcd m *** gcd m) . makePrerequisites) $
                       filter testProperPeriods $
                       map findProperPeriods simpleNumbers
  where
    simpleNumbers :: [Int]
    simpleNumbers = [a | a <- [2..m - 1], gcd m a == 1]
    
    findProperPeriods :: Int -> (Int, Int)
    findProperPeriods a = (a, (+ 1) $ length $ takeWhile (/= 1) $ map (\x -> a^x `mod` m) [1..])
    
    testProperPeriods :: (Int, Int) -> Bool
    testProperPeriods (a, r) = even r && a^(r `div` 2) `mod` m /= m - 1
    
    makePrerequisites :: (Int, Int) -> (Int, Int)
    makePrerequisites (a, r) = (subtract 1 *** (+ 1)) $ double $ a^(r `div` 2)
    
    double :: a -> (a, a)
    double x = (x, x)

{-[ КОНЕЦ МОДУЛЯ ]-------------------------------------------------------------}
