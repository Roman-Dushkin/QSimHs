{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{------------------------------------------------------------------------------}
{- | Модуль с описанием программных сущностей для представления и базовой
     обработки гейтов.

   Организация: ООО «А-Я эксперт»
   Автор:       Душкин Р. В.
   Проект:      Квантовые вычисления и функциональное программирование
                                                                              -}
{------------------------------------------------------------------------------}

module Gate
(
  -- * Типы
  Vector,
  Matrix,
  
  -- * Функции
  -- ** Обработка
  apply,
  adjoint,
  
  -- ** Операторы
  (<|>),
  (|><|),
  (<*:>),
  (<**>),
  (<++>),
  (<+>),
  (<->),
  
  -- ** Функции для простого создания гейтов
  vectorToComplex,
  matrixToComplex,
  
  -- ** Функции для простого восприятия целых чисел
  vectorToInt,
  matrixToInt,

  -- ** Специальные константы: гейты
  gateI,
  gateX,
  gateY,
  gateZ,
  gateH,
  gateCNOT,
  gateN,
  gateHn,
  gateIn
)
where

{-[ СЕКЦИЯ ИМПОРТА ]-----------------------------------------------------------}

import Data.Complex (Complex(..), conjugate, realPart)
import Data.List (transpose)

import Qubit

{-[ СИНОНИМЫ ТИПОВ ]-----------------------------------------------------------}

-- | Вектор как наименование для списка.
type Vector a = [a]

-- | Матрица как наименование для списка списков.
type Matrix a = [Vector a]

{-[ ОПРЕДЕЛЕНИЕ ПРИОРИТЕТА ]---------------------------------------------------}

infix 9 <|>, |><|

infix 8 <*:>

infixl 7 <++>

infixl 4 <**>

infixl 3 <+>, <->

{-[ ФУНКЦИИ ]------------------------------------------------------------------}

-- | Функция, вычисляющая произведение матрицы на вектор. В итоге получается
--   вектор. Разработчик должен сам следить за корректностью размерностей
--   матрицы и вектора, подаваемых на вход этой фунции.
apply :: Num a => Matrix a -> Vector a -> Vector a
apply m v = map (sum . zipWith (*) v) m

-- | Функция для получения эрмитово-сопряжённой матрицы для заданной.
adjoint :: RealFloat a => Matrix (Complex a) -> Matrix (Complex a)
adjoint = map (map conjugate) . transpose

-- | Оператор для получения внутреннего произведения двух векторов.
(<|>) :: Num a => Vector a -> Vector a -> a
v1 <|> v2 = sum $ zipWith (*) v1 v2

-- | Оператор для получения внешнего произведения двух векторов.
(|><|) :: RealFloat a => Vector (Complex a) -> Vector (Complex a) -> Matrix (Complex a)
v1 |><| v2 = [map (* c) v2 | c <- map conjugate v1]

-- | Оператор для умножения матрицы на число. Первым аргументом получает число,
--   а вторым, соответственно, матрицу.
(<*:>) :: Num a => a -> Matrix a -> Matrix a
c <*:> m = map (map (c *)) m

-- | Оператор для перемножения (обычного) матриц. Как обычно, разработчик должен
--   сам следить за корректностью размерностей перемножаемых матриц.
(<**>) :: Num a => Matrix a -> Matrix a -> Matrix a
m1 <**> m2 = [apply m1 col | col <- transpose m2]

-- | Оператор для тензорного перемножения матриц.
(<++>) :: Num a => Matrix a -> Matrix a -> Matrix a
m1 <++> m2 = concatMap (map concat . transpose) $
               groups (length $ head m1) [c <*:> m2 | c <- concat m1]

-- | Оператор для сложения двух матриц. Разработчик самостоятельно должен
--   следить за корректностью размерностей входных матриц, чтобы получать
--   адекватные результаты.
(<+>) :: Num a => Matrix a -> Matrix a -> Matrix a
m1 <+> m2 = zipWith (zipWith (+)) m1 m2

-- | Оператор для вычитания матриц друг из друга. Разработчик самостоятельно
--   должен следить за корректностью размерностей входных матриц, чтобы получать
--   адекватные результаты.
(<->) :: Num a => Matrix a -> Matrix a -> Matrix a
m1 <-> m2 = zipWith (zipWith (-)) m1 m2

-- | Сервисная функция для перевода списка счётных чисел в список комплексных
--   чисел. Используется для преобразования кубитов в каноническом представлении
--   с целыми коэффициентами при квантовых состояниях.
vectorToComplex :: Integral a => Vector a -> Vector (Complex Double)
vectorToComplex = map (\i -> fromIntegral i :+ 0.0)

-- | Ещё одна сервисная функция для перевода матрицы счётных чисел в матрицу
--   комплексных чисел. Используется для преобразования унитарных матриц
--   (квантовых операторов) в матричном представлении с целями коэффициентами.
matrixToComplex :: Integral a => Matrix a -> Matrix (Complex Double)
matrixToComplex = map vectorToComplex

-- | Сервисная функция для получения простого для восприятия вектора, в котором
--   одни целые числа. Неадекватна к применению для произвольных векторов.
vectorToInt :: Vector (Complex Double) -> Vector Int
vectorToInt = map (round . realPart)

-- | Ещё одна сервисная функция для получения простой для восприятия матрицы,
--   в которой одни целые числа. Неадекватна к применению для произвольных
--   матриц.
matrixToInt :: Matrix (Complex Double) -> Matrix Int
matrixToInt = map vectorToInt

-- | Константная функция, возвращающая матричное представление квантового гейта
--   I (тождественное преобразование).
gateI :: Matrix (Complex Double)
gateI = [[1.0 :+ 0.0, 0.0 :+ 0.0],
         [0.0 :+ 0.0, 1.0 :+ 0.0]]

-- | Константная функция, возвращающая матричное представление квантового гейта
--   X (отрицание).
gateX :: Matrix (Complex Double)
gateX = [[0.0 :+ 0.0, 1.0 :+ 0.0],
         [1.0 :+ 0.0, 0.0 :+ 0.0]]

-- | Константная функция, возвращающая матричное представление квантового гейта
--   Y (изменение фазы).
gateY :: Matrix (Complex Double)
gateY = [[0.0 :+ 0.0, 0.0 :+ (-1.0)],
         [0.0 :+ 1.0, 0.0 :+ 0.0]]

-- | Константная функция, возвращающая матричное представление квантового гейта
--   Z (сдвиг фазы).
gateZ :: Matrix (Complex Double)
gateZ = [[1.0 :+ 0.0, 0.0 :+ 0.0],
         [0.0 :+ 0.0, (-1.0) :+ 0.0]]

-- | Константная функция, возвращающая матричное представление квантового гейта
--   H (преобразование Адамара).
gateH :: Matrix (Complex Double)
gateH = [[(1/sqrt 2) :+ 0.0, (1/sqrt 2) :+ 0.0],
         [(1/sqrt 2) :+ 0.0, ((-1)/sqrt 2) :+ 0.0]]

-- | Константная функция, возвращающая матричное представление квантового гейта
--   CNOT (контролируемое-НЕ).
gateCNOT :: Matrix (Complex Double)
gateCNOT = [[1.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0],
            [0.0 :+ 0.0, 1.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0],
            [0.0 :+ 0.0, 0.0 :+ 0.0, 0.0 :+ 0.0, 1.0 :+ 0.0],
            [0.0 :+ 0.0, 0.0 :+ 0.0, 1.0 :+ 0.0, 0.0 :+ 0.0]]

-- | Функция, реализующая заданный гейт для заданного количества кубитов.
gateN :: Matrix (Complex Double) -> Int -> Matrix (Complex Double)
gateN g n = foldl1 (<++>) $ replicate n g

-- | Функция, реализующая гейт Адамара для заданного количества кубитов.
gateHn :: Int -> Matrix (Complex Double)
gateHn = gateN gateH

-- | Функция, реализующая тождественный гейт для заданного количества кубитов.
gateIn :: Int -> Matrix (Complex Double)
gateIn = gateN gateI

{-[ КОНЕЦ МОДУЛЯ ]-------------------------------------------------------------}
