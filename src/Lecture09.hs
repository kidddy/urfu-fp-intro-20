{-# LANGUAGE DuplicateRecordFields #-}

module Lecture09 where

import System.Random
import System.Directory
import System.FilePath
import Data.UUID
import System.IO
import Data.List

{-
  09: Монады IO и Random

  - overview and motivation
  - Pure computations a -> b
    - Lazy: very hard to do i/o => pure
    - effects and side-effects
      - java checked exceptions
        - throwable Exception
  - not a toy => I/O (before monads, haskell 1.0)
    - streams
    - continuations
  - Effects
    - a -> IO b -- i/o
    - a -> (b -> r) -> r -- continuations
    - a -> b + Ex -- exceptions
    - a -> [b]
    - a -> Maybe b
    - T = PR _, IO _, (_ -> r) -> r, _ + Ex, [_], Maybe _
      - T-effectfull computation a -> b is a pure computation a -> T b
      - why we want (show examples, continuations figure 4, arguments passing)
        - 1. want to embed pure data into effectful world
          a -> T a
        - 2. composition of effectful computation
          a -> T b
          b -> T c
          ---------
          a -> T c

          bind :: T b -> (b -> T c) -> T c
    - class T m where
        (>>=)  :: m a -> (  a -> m b) -> m b
        return ::   a                 -> m a
  - Monad
    - set of types: Int, Float, Maybe a, IO a, ...
    - laws
      Left identity: 	return a >>= f ≡ f a
      Right identity: m >>= return ≡ m
      Associativity: 	(m >>= f) >>= g	≡ m >>= (\x -> f x >>= g)
  - Higher-kinded polymorphism + type classes + monads
  - monadic I/O (haskell 1.3 1996)
    - standard functions
  - random
    - standard functions
  - algebraic effects
  - Functor => Applicative => Monad
    - semantics (behaviour)

  Подробнее:
  - Chapter 7
    https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf
  - https://www.seas.upenn.edu/~cis194/fall16/lectures/06-io-and-monads.html
-}

-- <Задачи для самостоятельного решения>

{-
  TODO list

  Напишите программу для работы со списком задач.
  Хранить задачи нужно в виде файлов в определённой папке.
  Вы можете сами выбрать формат имени и содержимого файлов.

  Вам понадобятся
  - System.Directory
    https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html
  - Работа с файлами в IO
    http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html
-}

newtype TodoList = TodoList FilePath deriving (Eq, Show)

newtype Id = Id String deriving (Eq, Show, Read)

newtype Title = Title String deriving (Eq, Show, Read)

newtype Deadline = Deadline String deriving (Eq, Show, Read, Ord)

newtype Content = Content String deriving (Eq, Show, Read)

-- Тип для чтения Todo
data Todo = Todo
  { todoId :: Id
  , title :: Title
  , content :: Content
  , deadline :: Deadline
  , isDone :: Bool
  } deriving (Eq, Show, Read)

instance Ord Todo where
  compare (Todo _ _ _ deadline _) (Todo _ _ _ deadline' _) = compare deadline deadline'

-- Тип для редактирования Todo
data TodoEdit = TodoEdit
  { title :: Title
  , content :: Content
  , deadline :: Deadline
  } deriving (Eq, Show)

uniqueId :: IO Id
uniqueId = do
  g <- newStdGen
  let (u, _) = random g
  return $ Id $ toString u

createTodoList :: FilePath -> IO TodoList
createTodoList rootFolder = do
  listPath <- makeRelativeToCurrentDirectory rootFolder
  createDirectoryIfMissing False listPath
  return (TodoList listPath)
  
writeAndClose :: FilePath -> String -> IO ()
writeAndClose filePath content = do
  h <- openFile filePath WriteMode
  hPutStrLn h content
  hClose h
  
addTodo :: TodoList -> Title -> Content -> Deadline -> IO Id
addTodo todoList title text deadline = do
  id <- uniqueId
  let filePath = todoPath todoList id 
  let x = Todo id title text deadline False
  writeAndClose filePath $ show x
  return $ todoId x

todoPath :: TodoList -> Id -> FilePath
todoPath (TodoList path ) (Id fileId) = path </> fileId

readTodo :: TodoList -> Id -> IO Todo
readTodo todoList id = readFile (todoPath todoList id) >>= return . read

showTodo :: TodoList -> Id -> IO ()
showTodo todoList id = readTodo todoList id >>= putStrLn . show 

removeTodo :: TodoList -> Id -> IO ()
removeTodo todoList id = removeFile (todoPath todoList id)

replaceTodo :: TodoList -> Id -> Todo -> IO ()
replaceTodo todoList id todo = do
  removeTodo todoList id
  let path = todoPath todoList id
  writeAndClose path $ show todo

editTodo :: TodoList -> Id -> TodoEdit -> IO ()
editTodo todoList id (TodoEdit title content deadline) = do
  todo <- readTodo todoList id
  replaceTodo todoList id (Todo id title content deadline (isDone todo))
 
setTodoAsDone :: TodoList -> Id -> IO ()
setTodoAsDone todoList id = do
  todo <- readTodo todoList id
  replaceTodo todoList id todo { isDone = True }

-- Todo должны быть упорядочены по возрастанию deadline'а
readAllTodo :: TodoList -> IO [Todo]
readAllTodo todoList@(TodoList path) = do
  listDirectory path >>= mapM (readTodo todoList . Id) >>= return . sort

readUnfinishedTodo :: TodoList -> IO [Todo]
readUnfinishedTodo todoList =
  readAllTodo todoList >>= return . filter (not . isDone)

showTodos :: [Todo] -> IO ()
showTodos = mapM_ $ putStrLn . show

showAllTodo :: TodoList -> IO ()
showAllTodo todoList = readAllTodo todoList >>= showTodos

showUnfinishedTodo :: TodoList -> IO ()
showUnfinishedTodo todoList = readUnfinishedTodo todoList >>= showTodos

{-
  Напишите игру для угадывания случайного числа.

  При старте, необходимо случайным образом выбрать число в
  отрезке [0..100] и предоставить пользователю возможность его угадать.

  Пример игровой сессии:

  > playGuessGame
  Your number: 50
  > Too big
  Your number: 25
  > Too small
  Your number: 37  
  > Yep, that's the number!
-}

playGuessGame :: IO ()
playGuessGame = do
  num <- getStdRandom (randomR (0, 100))
  readAndCheck num
  where 
    readAndCheck num = do
        putStr "Your number: "
        guess <- getLine
        check (read guess :: Int) num

    check guess num =
      case compare guess num of
        EQ -> do putStrLn "Yep, that's the number"
        LT -> do
          putStrLn "Too small"
          readAndCheck num
        GT -> do
          putStrLn "Too big"
          readAndCheck num

-- </Задачи для самостоятельного решения>