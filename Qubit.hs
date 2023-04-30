{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{------------------------------------------------------------------------------}
{- | Модуль с описанием программных сущностей для представления и базовой
     обработки кубитов.

   Организация: ООО «А-Я эксперт»
   Автор:       Душкин Р. В.
   Проект:      Квантовые вычисления и функциональное программирование
                                                                              -}
{------------------------------------------------------------------------------}

module Qubit
(
  -- * Типы
  Qubit(..),
  
  -- * Функции
  -- ** Общие сервисные функции
  groups,
  changeElement,
  
  -- ** Создание
  toList,
  fromLists,
  toPairList,
  fromPairList,
  toVector,
  fromVector,
  
  -- ** Обработка
  liftQubit,
  entangle,
  conjugateQubit,
  scalarProduct,
  norm,
  normalize,
  measure,
  measureP,
  
  -- ** Специальные константы: кубиты
  qubitZero,
  qubitZero',
  qubitOne,
  qubitOne',
  qubitPlus,
  qubitPlus',
  qubitMinus,
  qubitMinus',
  qubitPhiPlus,
  qubitPhiPlus',
  qubitPhiMinus,
  qubitPhiMinus',
  qubitPsiPlus,
  qubitPsiPlus',
  qubitPsiMinus,
  qubitPsiMinus'
)
where

{-[ СЕКЦИЯ ИМПОРТА ]-----------------------------------------------------------}

import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Data.Complex
import Data.Function (on)
import Data.List (inits, sortBy, transpose)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Tuple (swap)
import System.Random (Random, randomRIO)

import QuantumState

{-[ ИЗОМОРФНЫЕ ТИПЫ ]----------------------------------------------------------}

-- | Тип, представляющий кубит. Это просто список квантовых состояний.
newtype Qubit a = Qubit [QuantumState a]

{-[ ЭКЗЕМПЛЯРЫ КЛАССОВ ]-------------------------------------------------------}

instance (RealFloat a, Show a) => Show (Qubit a) where
  show (Qubit       []) = ""
  show (Qubit     [qs]) = show qs
  show (Qubit (qs:qss)) = show qs ++ " + " ++ show (Qubit qss)

{-[ ФУНКЦИИ ]------------------------------------------------------------------}

-- | Сервисная функция для разбиения заданного списка на подсписки заданной
--   длины.
groups :: Int -> [a] -> [[a]]
groups i s | null s = []
           | otherwise  = let (h, t) = splitAt i s
                          in   h : groups i t

-- | Служебная функция для замены элемента в заданном списке на заданной позиции
--   (счёт начинается с 1) на заданное значение.
changeElement :: [a] -> Int -> a -> [a]
changeElement xs i x = take (i - 1) xs ++
                       [x] ++
                       drop i xs

-- | Служебная функция для получения из кубита списка его квантовых состояний
--   (разворачиватель). Хотя, конечно, это можно было сделать при помощи
--   именованного поля.
quantumStates :: Qubit a -> [QuantumState a]
quantumStates (Qubit qs) = qs

-- | Функция для преобразования кубита в вектор (список). На выходе получается
--   просто список амплитуд.
toList :: Qubit a -> [Complex a]
toList = map amplitude . quantumStates

-- | Функция для преобразования кубита в список меток его квантовых состояний.
toLabelList :: Qubit a -> [String]
toLabelList = map label . quantumStates

-- | Функция для создания кубита из двух списков — списка комплексных амплитуд и
--   списка меток. Разработчик сам должен следить за тем, что семантика кубита
--   должна выполняться (например, при полном задействовании всех базисных
--   состояний их количество должно составлять степень двойки).
fromLists :: [Complex a] -> [String] -> Qubit a
fromLists a l = Qubit $ zipWith QS a l

-- | Функция для преобразования кубит в список пар.
toPairList :: Qubit a -> [(Complex a, String)]
toPairList = map toPair . quantumStates

-- | Функция для создания кубита из списка пар — пар комплексных амплитуд и
--   меток. Разработчик сам должен следить за тем, что семантика кубита должна
--   выполняться (например, при полном задействовании всех базисных состояний их
--   количество должно составлять степень двойки).
fromPairList :: [(Complex a, String)] -> Qubit a
fromPairList = Qubit . map fromPair

-- | Функция для преобразования кубита к векторному представлению в стандартном
--   базисе.
toVector :: Num a => Qubit a -> [Complex a]
toVector q = if all (`elem` "01") labels
               then map (fromMaybe (0 :+ 0) . flip lookup qsPairs) basis
               else error "Некорректные метки кубита."
  where
    n       = length $ label $ head $ quantumStates q
    labels  = concatMap label $ quantumStates q
    qsPairs = map (swap . toPair) $ sortBy (comparing label) $ quantumStates q
    basis   = replicateM n "01"

-- | Функция для создания кубита из векторного представления в стандартном
--   базисе.
fromVector :: Int -> [Complex a] -> Qubit a
fromVector n q = fromLists q $ replicateM n "01"

-- | Сервисная функция для «втягивания» заданной функции в кубит и применения её
--   к списку квантовых состояний.
liftQubit :: ([QuantumState a] -> [QuantumState b]) -> Qubit a -> Qubit b
liftQubit f (Qubit qs) = Qubit $ f qs

-- | Функция для связывания двух кубитов в одну систему из нескольких кубитов.
--   По сути, производит тензорное умножение кубитов друг на друга.
entangle :: RealFloat a => Qubit a -> Qubit a -> Qubit a
entangle (Qubit qss1) (Qubit qss2)
  = Qubit $ filter (predicateQS (/= 0)) [QS (((*)  `on` amplitude) qs1 qs2)
                                            (((++) `on` label) qs1 qs2) | qs1 <- qss1,
                                                                          qs2 <- qss2]

-- | Функция для получения комплексно-сопряжённого кубита для заданного.
conjugateQubit :: RealFloat a => Qubit a -> Qubit a
conjugateQubit = liftQubit (map conjugateQS)

-- | Функция для вычисления скалярного (внутреннего) произведения двух заданных
--   кубитов.
scalarProduct :: RealFloat a => Qubit a -> Qubit a -> a
scalarProduct q1 q2 = realPart $
                        sum $
                        ((zipWith (*)) `on` toVector) q1 q2

-- | Функция для получения нормы вектора, представляющего собой кубит (то есть
--   его длину).
norm :: RealFloat a => Qubit a -> a
norm q = sqrt $ scalarProduct q (conjugateQubit q)

-- | Функция для нормализации заданного кубита, то есть для получения из кубита
--   нового, норма (длина) которого равна в точности 1. Это значит, что у
--   результирующего кубита сумма квадратов модулей амплитуд равна 1, и
--   выполняется условие нормированности.
normalize :: RealFloat a => Qubit a -> Qubit a
normalize q = liftQubit (map (applyQS (/ (norm q :+ 0)))) q

-- | Функция для осуществления процесса измерения заданного кубита. В
--   зависимости от распределения амплитуд вероятности выбирает одно из
--   квантовых состояний кубита и возвращает его метку.
measure :: (RealFloat a, Random a) => Qubit a -> IO String
measure q = getRandomElementWithProbabilities $
              sortBy (compare `on` snd) $
              map ((swap . \(a, l) -> (realPart (a * conjugate a), l)) . toPair) $
              quantumStates q

-- | Функция для осуществления процесса частичного измерения заданного кубита. В
--   зависимости от распределения амплитуд вероятности выбирает одно из
--   частичных квантовых состояний, определяемых набором индексов (второй
--   аргумент). Возвращает пару (измеренная метка, оставшиеся квантовые
--   состояния)
measureP :: (RealFloat a, Random a) => Qubit a -> [Int] -> IO (String, Qubit a)
measureP q xs = undefined

-- | Служебная функция для получения случайного элемента из заданного списка с
--   учётом распределения вероятностей. Список должен содержать пары, первым
--   элементом которых являются возвращаемые элементы, а вторым — вероятность.
--   Значения вероятности не обязательно должны быть нормированы.
getRandomElementWithProbabilities :: (Ord b, Num b, Random b) => [(a, b)] -> IO a
getRandomElementWithProbabilities l = (head . goodList) `fmap` randomRIO (0, sumProbs l)
  where
    goodList p = map fst $
                   dropWhile (\(_, p') -> p' < p) $
                   map ((fst . last) &&& sumProbs) $
                   tail $
                   inits l
    sumProbs = sum . map snd

-- | Константная функция для представления кубита |0> в стандартном
--   вычислительном базисе.
qubitZero :: Qubit Double
qubitZero = Qubit [QS (1.0 :+ 0.0) "0", QS (0.0 :+ 0.0) "1"]

-- | Константная функция для представления кубита |0> в стандартном
--   вычислительном базисе в виде вектора.
qubitZero' :: [Complex Double]
qubitZero' = toVector qubitZero

-- | Константная функция для представления кубита |1> в стандартном
--   вычислительном базисе.
qubitOne :: Qubit Double
qubitOne = Qubit [QS (0.0 :+ 0.0) "0", QS (1.0 :+ 0.0) "1"]

-- | Константная функция для представления кубита |1> в стандартном
--   вычислительном базисе в виде вектора.
qubitOne' :: [Complex Double]
qubitOne' = toVector qubitOne

-- | Константная функция для представления кубита |+> в стандартном
--   вычислительном базисе.
qubitPlus :: Qubit Double
qubitPlus = Qubit [QS ((1/sqrt 2) :+ 0.0) "0", QS ((1/sqrt 2) :+ 0.0) "1"]

-- | Константная функция для представления кубита |+> в стандартном
--   вычислительном базисе в виде вектора.
qubitPlus' :: [Complex Double]
qubitPlus' = toVector qubitPlus

-- | Константная функция для представления кубита |-> в стандартном
--   вычислительном базисе.
qubitMinus :: Qubit Double
qubitMinus = Qubit [QS (1/sqrt 2 :+ 0.0) "0", QS ((-1)/sqrt 2 :+ 0.0) "1"]

-- | Константная функция для представления кубита |-> в стандартном
--   вычислительном базисе в виде вектора.
qubitMinus' :: [Complex Double]
qubitMinus' = toVector qubitMinus

-- | Константная функция для представления кубита Φ+ (первое состояние Белла) в
--   стандартном вычислительном базисе.
qubitPhiPlus :: Qubit Double
qubitPhiPlus = Qubit [QS (1/sqrt 2 :+ 0.0) "00", QS (1/sqrt 2 :+ 0.0) "11"]

-- | Константная функция для представления кубита Φ+ (первое состояние Белла) в
--   стандартном вычислительном базисе в виде вектора.
qubitPhiPlus' :: [Complex Double]
qubitPhiPlus' = toVector qubitPhiPlus

-- | Константная функция для представления кубита Φ- (второе состояние Белла) в
--   стандартном вычислительном базисе.
qubitPhiMinus :: Qubit Double
qubitPhiMinus = Qubit [QS (1/sqrt 2 :+ 0.0) "00", QS ((-1)/sqrt 2 :+ 0.0) "11"]

-- | Константная функция для представления кубита Φ- (второе состояние Белла) в
--   стандартном вычислительном базисе в виде вектора.
qubitPhiMinus' :: [Complex Double]
qubitPhiMinus' = toVector qubitPhiMinus

-- | Константная функция для представления кубита Ψ+ (третье состояние Белла) в
--   стандартном вычислительном базисе.
qubitPsiPlus :: Qubit Double
qubitPsiPlus = Qubit [QS (1/sqrt 2 :+ 0.0) "01", QS (1/sqrt 2 :+ 0.0) "10"]

-- | Константная функция для представления кубита Ψ+ (третье состояние Белла) в
--   стандартном вычислительном базисе в виде вектора.
qubitPsiPlus' :: [Complex Double]
qubitPsiPlus' = toVector qubitPsiPlus

-- | Константная функция для представления кубита Ψ- (четвёртое состояние Белла)
--   в стандартном вычислительном базисе.
qubitPsiMinus :: Qubit Double
qubitPsiMinus = Qubit [QS (1/sqrt 2 :+ 0.0) "01", QS ((-1)/sqrt 2 :+ 0.0) "10"]

-- | Константная функция для представления кубита Ψ- (четвёртое состояние Белла)
--   в стандартном вычислительном базисе в виде вектора.
qubitPsiMinus' :: [Complex Double]
qubitPsiMinus' = toVector qubitPsiMinus

{-[ КОНЕЦ МОДУЛЯ ]-------------------------------------------------------------}
