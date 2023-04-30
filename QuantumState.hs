{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{------------------------------------------------------------------------------}
{- | Модуль с описанием программных сущностей для представления и базовой
     обработки квантовых состояний.

   Организация: ООО «А-Я эксперт»
   Автор:       Душкин Р. В.
   Проект:      Квантовые вычисления и функциональное программирование
                                                                              -}
{------------------------------------------------------------------------------}

module QuantumState
(
  -- * Типы
  QuantumState(..),
  
  -- * Функции
  -- ** Создание
  toPair,
  fromPair,
  
  -- ** Применение функций
  applyQS,
  predicateQS,
  conjugateQS
)
where

{-[ СЕКЦИЯ ИМПОРТА ]-----------------------------------------------------------}

import Data.Complex

{-[ АЛГЕБРАИЧЕСКИЕ ТИПЫ ДАННЫХ ]-----------------------------------------------}

-- | Тип, представляющий одно квантовое состояние.
data QuantumState a = QS
                      {
                        amplitude :: Complex a,  -- ^ Амплитуда квантового состояния.
                        label     :: String      -- ^ Метка квантового состояния.
                      }

{-[ ЭКЗЕМПЛЯРЫ КЛАССОВ ]-------------------------------------------------------}

instance (Eq a, Ord a, Num a, RealFloat a, Show a) => Show (QuantumState a) where
  show (QS a l) = brackets ++ "|" ++ l ++ ">"
    where
      brackets = if realPart a /= 0 && imagPart a /= 0
                   then "(" ++ prettyShowComplex a ++ ")"
                   else prettyShowComplex a

{-[ ФУНКЦИИ ]------------------------------------------------------------------}

-- | Служебная функция для преобразования комплексного числа в «приятную»
--   строку.
prettyShowComplex :: (Eq a, Ord a, Num a, RealFrac a, Show a) => Complex a -> String
prettyShowComplex (r :+ 0) = prettyShowNumber r
prettyShowComplex (0 :+ i) | i == 1    = "i"
                           | i == -1   = "-i"
                           | otherwise = prettyShowNumber i ++ "i"
prettyShowComplex (r :+ i) | i == 1    = prettyShowNumber r ++ " + i"
                           | i == -1   = prettyShowNumber r ++ " - i"
                           | i >= 0    = prettyShowNumber r ++ " + " ++ prettyShowNumber i ++ "i"
                           | otherwise = prettyShowNumber r ++ " - " ++ prettyShowNumber (abs i) ++ "i"

-- | Служебная функция, которая преобразует в строку числа (целые и
--   действительные) так, что если у числа нет дробной части, то в строку не
--   выносится никакой дробной части: 2.5 -> "2.5", 2.0 -> "2".
prettyShowNumber :: (RealFrac a, Show a) => a -> String
prettyShowNumber f = if d == 0.0
                       then show n
                       else show f
  where
    (n, d) = properFraction f

-- | Функция для преобразования квантового состояния в пару величин.
toPair :: QuantumState a -> (Complex a, String)
toPair (QS a l) = (a, l)

-- | Функция для создания квантового состояния из пары вида (Амплитуда, Метка).
fromPair :: (Complex a, String) -> QuantumState a
fromPair (a, l) = QS a l

-- | Функция высшего порядка для применения заданной функции к амплитуде
--   заданного квантового состояния.
applyQS :: (Complex a -> Complex a) -> QuantumState a -> QuantumState a
applyQS f (QS a l) = QS (f a) l

-- | Ещё одна функция высшего порядка для применения заданного предиката к
--   амплитуде заданного квантового состояния.
predicateQS :: (Complex a -> Bool) -> QuantumState a -> Bool
predicateQS p (QS a _) = p a

-- | Специализированная функция, которая возвращает комплексно-сопряжённое
--   квантовое состояние для заданного. Является обёрткой над функцией
--   \conjugate\ из модуля \Data.Complex\ и предназначена для упрощения работы с
--   квантовыми состояниями.
conjugateQS :: RealFloat a => QuantumState a -> QuantumState a
conjugateQS = applyQS conjugate

{-[ КОНЕЦ МОДУЛЯ ]-------------------------------------------------------------}
