{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lecture11.PersonsT where

import Data.List
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Prelude hiding (id)
import Lecture10.Reader ( Person (..)
                        , PersonId
                        , persons
                        , processSingle
                        , processPair
                        )


-- <Задачи для самостоятельного решения>

{-
  В этом задании нужно адаптировать код из 10 лекции к новым требованиям
  при помощи трансформеров.

  1. Необходимо собирать статистику найденных людей:
    - количество одиноких персон
    - количество замужних персон
  2. В функцию `findById` добавить логгирование с помощью монады Write.
    Нужно сообщать была ли найдена персона по данному id или нет.

  Вы можете переиспользовать функции, которые остались без изменения из Lecture10.Reader
-}

data PersonSearchStats = PersonSearchStats
  { marriedPersonsCount :: Integer
  , singlePersonsCount  :: Integer
  } deriving (Show)

emptyStats :: PersonSearchStats
emptyStats = PersonSearchStats 0 0

plus_married :: PersonSearchStats -> PersonSearchStats
plus_married (PersonSearchStats married single) =
    PersonSearchStats (married+1) single

plus_single :: PersonSearchStats -> PersonSearchStats
plus_single (PersonSearchStats married single) =
    PersonSearchStats married (single+1)

newtype PersonsT a = PersonsT
  { runPersonsT :: ReaderT [Person] (StateT PersonSearchStats (Writer [String])) a}

  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader [Person]
    , MonadWriter [String]
    , MonadState PersonSearchStats
    )

runPersons :: PersonsT a -> ((a, PersonSearchStats), [String])
runPersons = runWriter . state_t . reader_t . runPersonsT
    where
        state_t  x = runStateT  x emptyStats
        reader_t x = runReaderT x persons


findById :: PersonId -> PersonsT (Maybe Person)
-- findById pId = error ""
findById pId = do
    persons <- ask 
    let m_person = find (\p -> id p == pId) persons
    case m_person of
        Just p -> tell ["ID:" ++ show pId ++ " found"]
        _      -> tell ["ID:" ++ show pId ++ " not found"]
    return m_person

processPerson :: PersonId -> PersonsT (Maybe String)
processPerson pId = do
    stats <- get
    m_person <- findById pId
    case m_person of
        Nothing -> return Nothing
        Just p  -> case marriedBy p of
            Nothing       -> do
                put $ plus_single stats
                return . Just $ processSingle p
            Just parterId -> do
                put $ plus_married stats
                m_partner <- findById parterId
                return . Just $ case m_partner of
                    Nothing      -> processSingle p
                    Just partner -> processPair p partner
    

{-
  Функция должна выводить на экран:
  - общую поисковую статистику по всем найденым персонам.
  - результат каждого поиска

  Записывать в "persons.log" общий лог всех поисков.
-}
processPersons :: [PersonId] -> IO ()
processPersons personIds = let
    action :: PersonId -> PersonsT (PersonId, Maybe String)
    action p = (\r -> (p, r)) <$> processPerson p

    print_result :: (PersonId, Maybe String) -> IO ()
    print_result (pId, result) = do
        putStr $ "ID:" ++ show pId ++ " "
        case result of
            Just _ -> putStrLn "found"
            _      -> putStrLn "not found" 

    print_stats :: PersonSearchStats -> IO ()
    print_stats (PersonSearchStats married single) = do
        putStrLn $ "Stats:"
        putStrLn $ "\tTotal:   " ++ show (married + single)
        putStrLn $ "\tMarried: " ++ show married
        putStrLn $ "\tSingle:  " ++ show single

    ((results, stats), logs) = runPersons $ mapM action personIds
    in do
        mapM_ print_result results
        putStrLn ""
        print_stats stats
        writeFile "persons.log" $ intercalate "\n" logs ++ "\n"

-- </Задачи для самостоятельного решения>
