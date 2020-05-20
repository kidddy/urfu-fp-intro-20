module Lecture10.Reader where

import Data.List
import Data.Maybe
import Control.Monad.Reader
import Prelude hiding (id)

-- <Задачи для самостоятельного решения>

{-
  Задача: по имеющейся базе данных сфомировать набор рекламных писем.
  Для неженатого(-ой) персоны письмо должно иметь вид:

  Для мужчины:

"""
  Уважаемый Имя Отчество!
  Разрешите предложить Вам наши услуги.
"""

  Для женщины:

"""
  Уважаемая Имя Отчество!
  Разрешите предложить Вам наши услуги.
"""

  Семейным парам шлется одно письмо вида

"""
  Уважаемые Имя_мужа Отчество_мужа и Имя_жены Отчество_жены!
  Разрешите предложить вам наши услуги.
"""

-}

data Sex = Male | Female deriving (Show, Eq, Ord)

type PersonId = Int


data Person = Person
  { id        :: Int
  , family    :: String
  , name      :: String
  , surname   :: String
  , sex       :: Sex
  , marriedBy :: Maybe Int
  } deriving (Show, Eq, Ord)

persons :: [Person]
persons =
  [ Person 1 "Иванов"    "Иван"      "Иванович"     Male   $ Nothing
  , Person 2 "Петров"    "Петр"      "Петрович"     Male   $ Just 7
  , Person 3 "Соловьева" "Алия"      "Фаридовна"    Female $ Nothing
  , Person 4 "Кузнецова" "Мария"     "Ивановна"     Female $ Just 8
  , Person 5 "Гринько"   "Юлия"      "Владимировна" Female $ Nothing
  , Person 6 "Кабанов"   "Александр" "Романович"    Male   $ Nothing
  , Person 7 "Петрова"   "Екатерина" "Алексеевна"   Female $ Just 2
  , Person 8 "Кузнецов"  "Евгений"   "Семёнович"    Male   $ Just 4
  , Person 9 "Антонов"   "Юрий"      "Васильевич"   Male   $ Nothing
  ]

-- Поиск персоны по номеру
findById :: PersonId -> Reader [Person] (Maybe Person)
findById pId = do
    persons <- ask
    return $ find (\p -> id p == pId) persons

processSingle :: Person -> String
processSingle p = let
    greeting :: String
    greeting = case sex p of
        Male   -> intercalate " " ["Уважаемый", name p, surname p]
        Female -> intercalate " " ["Уважаемая", name p, surname p]
    in intercalate "\n" [greeting ++ "!", "Разрешите предложить Вам наши услуги."]

processPair :: Person -> Person -> String
processPair husband wife = let
    greeting :: String
    greeting = intercalate " " [ "Уважаемые"
                               , name    husband
                               , surname husband
                               , "и"
                               , name    wife
                               , surname wife
                               ]
    in intercalate "\n" [greeting ++ "!", "Разрешите предложить вам наши услуги."]

processPerson :: PersonId -> Reader [Person] (Maybe String)
processPerson pId = do
    m_person <- findById pId
    case m_person of
        Nothing -> return Nothing
        Just p  -> case marriedBy p of
            Nothing       -> return . Just $ processSingle p
            Just parterId -> do
                m_partner <- findById parterId
                return . Just $ case m_partner of
                    Nothing      -> processSingle p
                    Just partner -> processPair p partner
    

processPersons :: [PersonId] -> [Maybe String]
processPersons pIds = do
    pId <- pIds
    return $ runReader (processPerson pId) persons

-- </Задачи для самостоятельного решения>
