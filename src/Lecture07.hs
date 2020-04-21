{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-imports #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Lecture07 where

import Lecture07.Money
import Data.Semigroup
import Data.Foldable

{-
  07: Классы типов

  - ad-hoc polymorphism (overloading)
  - классы типов
    - Синтаксис
    - Eq, Show
    - default implementation
    - minimal implementation (Ord)
- Примеры
  - :i Num, Floating, Integral
  - :i Enum, Bounded
  - superclass
  - subclass
    - instance ClassA where
      f :: a -> a -- type signatures
      f = undefined
  - Dictionary passing (как работают тайпклассы)
    - https://mpickering.github.io/posts/2018-03-20-recordsvstypeclasses.html
    - http://www.haskellforall.com/2012/05/scrap-your-type-classes.html
    - https://homepages.inf.ed.ac.uk/wadler/papers/class/class.ps
  - Расширения
    - https://limperg.de/ghc-extensions/#basic-classes
      - FlexibleContexts
      - FlexibleInstances
      - TypeSynonymInstances
        - instance Class [Char] where
        - instance Class String where
      - MultiParamTypeClasses
        - class Class a b where ...
        - https://qfpl.io/posts/orphans-and-fundeps/
      - UndecidableInstances
      - OverlappingInstances
      - IncoherentInstances
      - ConstrainedClassMethods 
  - deriving (Eq, Show)
    - https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/generic-deriving
-}

-- <Задачи для самостоятельного решения>

{-
  Реализуйте инстанс Show для Expr:

    - Number 0 ~> 0
    - Plus (Number 0) (Number 3) ~> 0 + 3
    - Abs (Plus (Number 0) (Number 3)) ~> |0 + 3|
    - UnaryMinus (Plus (Mult (Number 2) (Number 2)) (Number 2)) ~> -((2 * 2) + 2)
-}
data Expr
  = Number Integer
  | Plus Expr Expr
  | Minus Expr Expr
  | Mult Expr Expr
  | UnaryMinus Expr
  | Abs Expr
  deriving Eq

instance Show Expr where
    show e = case e of
        Number x      -> show x
        Plus e1 e2    -> show e1 ++ " + " ++ show e2
        Minus e1 e2   -> show e1 ++ " - " ++ show e2
        Mult e1 e2    -> "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
        UnaryMinus e1 -> "-(" ++ show e1 ++ ")"
        Abs e1        -> "|" ++ show e1 ++ "|"

{-
  Реализуйте instance Semigroup для вектора:
-}
newtype Vec a = Vec { unVec :: [a] } deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Vec a) where
    x1 <> x2 = let
        zipped = zip (unVec x1) (unVec x2)

        f :: (Semigroup a) => (a, a) -> a
        f (x, y) = x <> y

        in Vec{ unVec=map f zipped }


instance Semigroup Integer where
    x <> y = x + y

{-
  Реализуйте instance Semigroup для типа для логгирования:
-}
newtype LogEntry = LogEntry { unLogEntry :: String } deriving (Eq, Show)

instance Semigroup LogEntry where
    x <> y = LogEntry { unLogEntry= unLogEntry x ++ unLogEntry y}

{-
  В `src/Lecture07/Money.hs` определены:
    - тип `Money a` для денег
    - типы `USD` и `RUB` для представления валют
    - конструкторы `mkDollars` и `mkRubbles`

  Реализуйте инстансы Semigroup для Money a.
-}

instance Semigroup (Money USD) where
    x <> y = mkDollars $ getMoney x + getMoney y

instance Semigroup (Money RUB) where
    x <> y = mkRubbles $ getMoney x + getMoney y

{-
  Реализуйте инстанс Functor для ExactlyOne
-}
data ExactlyOne a = ExactlyOne a deriving (Eq, Show)

instance Functor ExactlyOne where
    fmap f (ExactlyOne x) = ExactlyOne (f x)

{-
  Реализуйте инстанс Functor для `Maybe a`
-}
data Maybe' a = Just' a | Nothing' deriving (Eq, Show)

instance Functor Maybe' where
    fmap _ (Nothing') = Nothing'
    fmap f (Just' x) = Just' (f x)

{-
  Реализуйте инстанс Functor для `List a`
-}
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

{-
  `FileTree a` — тип для представления дерева файловой системы.

  Параметр `a` позволяет населять файлы произвольным значением.
-}
data FileTree a
  = Empty
  | File String a
  | Dir String [FileTree a]
  deriving (Eq, Show)

{-
  `FileInfo` — тип с информацией о файле: содержит его размер и дату последнего изменения. 
-}
data FileInfo = FileInfo
  { size :: Integer
  , modified :: String
  } deriving (Eq, Show)

{-
  Тогда мы можем строить деревья типа `FileTree FileInfo` и работать с ними.

  Например ниже определены функции для вычисления суммы всех размеров файлов в дереве
  и нахождения последней даты обновления файла в дереве.
-}

-- Пример использования Foldable для суммирования размера файла
fileSizeOfTree :: FileTree FileInfo -> Integer
fileSizeOfTree = getSum . foldMap (\FileInfo{..} -> Sum size)
-- С помощью расширения RecordWildCards     ^
-- мы раскрываем record и получаем доступ к полю size   ^^^^

-- Нужно для `latestModified`. В обычном коде так делать не надо.
instance Bounded [Char] where
  minBound = ""
  maxBound = "99999"

-- А здесь используется для нахождения даты последнего изменения
latestModified :: FileTree FileInfo -> String
latestModified = getMax . foldMap (\FileInfo{..} -> Max modified)

{-
  Чтобы функции выше работали, необходимо
  реализовать instance Foldable для FileTree:
-}

instance Foldable FileTree where
  -- foldMap :: Monoid m => (FileInfo -> m) -> FileTree FileInfo -> m
  foldMap f x = case x of
    Empty       -> mempty
    File _ info -> f info
    Dir _ files -> foldl (\a x -> a <> foldMap f x) mempty files

{-
  В этом задании вам необходимо придумать и написать иерархию исключений
  с помощью классов типов на основе набора требований:

  1. Базовый класс Exception с возможностью получения сообщения ошибки
  2. Класс для API ошибок. Должен уметь возвращать
    - ошибку в формате JSON
    - уровень severity (debug, info, error, warn)
  3. Класс для ошибок при работе с базой данных.
    - дополнительно возвращает сообщение об ошибке от базы данных
  4. Класс для ошибок доменной логики
    - дополнительно возвращает контекс с данными

  Реализовывать инстансы не нужно.
-}

class Exception a where
    message :: a -> String

data Severity
  = Debug
  | Info
  | Error
  | Warn
  deriving (Eq, Show)

data Json 
    = JNull
    | JString String
    | JNum Int
    | JBool Bool
    | JArray [Json]
    | JObject [(String, Json)]
    deriving Eq

class (Exception a) => ExceptionAPI a where
    severity :: a -> Severity
    json :: a -> Json

class (Exception a) => DataBaseException a where
    db_exception :: a -> String

class (Exception a) => DomanLogicException a where
    context :: a -> [(String, String)]

-- </Задачи для самостоятельного решения>
