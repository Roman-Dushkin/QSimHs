{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{------------------------------------------------------------------------------}
{- | Более сложные функции для генерации квантовых схем на языке Quipper.

   Организация: ООО «А-Я эксперт»
   Автор:       Душкин Р. В.
   Проект:      Квантовые вычисления и функциональное программирование
                                                                              -}
{------------------------------------------------------------------------------}

module QuipperHard
(
  main
)
where

{-[ СЕКЦИЯ ИМПОРТА ]-----------------------------------------------------------}

import Quipper

{-[ ФУНКЦИИ ]------------------------------------------------------------------}

-- | Функция, строящая квантовую схему для перевода заданного кубита из
--   вычислительного базиса в базис {|+>, |->}.
plusMinus :: Bool -> Circ Qubit
plusMinus b = do q <- qinit b
                 r <- hadamard q
                 return r

-- | Функция для получения пары связанных кубитов в квантовом состоянии
--   a|00> + b|11>.
entangle :: Qubit -> Circ (Qubit, Qubit)
entangle a = do b <- qinit False
                b <- qnot b `controlled` a
                return (a, b)

-- | Функция для получения состояния Бела Phi+.
bellPhiPlus :: Circ (Qubit, Qubit)
bellPhiPlus = plusMinus False >>= entangle

-- | Служебная функция для осуществления контролируемого вращения списка кубитов
--   в рамках Квантового преобразования Фурье.
rotations :: Qubit -> [Qubit] -> Int -> Circ [Qubit]
rotations _ []     _ = return []
rotations c (q:qs) n = do qs' <- rotations c qs n
                          let m = ((n + 1) - length qs)
                          q' <- rGate m q `controlled` c
                          return (q':qs')

-- | Основная функция для осуществления Квантового преобразования Фурье.
qft' :: [Qubit] -> Circ [Qubit]
qft' []     = return []
qft' [x]    = do hadamard x
                 return [x]
qft' (x:xs) = do xs'' <- qft' xs
                 xs' <- rotations x xs'' (length xs'')
                 x' <- hadamard x
                 return (x':xs')

-- | Обёртка над функцией `qft'` для правильного порядка кубитов.
qft :: [Qubit] -> Circ [Qubit]
qft qs = do qs <- qft' (reverse qs)
            return qs

-- | Главная функция модуля. Должна быть хоть как-то определена, чтобы
--   интерпретатор quipperi работал.
main = undefined

{-[ КОНЕЦ МОДУЛЯ ]-------------------------------------------------------------}
