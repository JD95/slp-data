{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import           Data.Csv     ((.!))
import qualified Data.Csv     as CSV
import qualified Data.Text    as T
import qualified Data.Vector  as V
import           GHC.Generics
import           Prelude      ()
import           Protolude

data BaseSample
  = BaseSample
  { date        :: Text
  , age         :: Int
  , gender      :: Text
  , stutter     :: Text
  , examiner    :: Text
  , participant :: Int
  , passage     :: Text
  , time        :: Double
  , question1   :: Int
  , question2   :: Int
  , question3   :: Int
  , question4   :: Int
  } deriving (Show, Generic, NFData)

instance CSV.FromRecord BaseSample where
  parseRecord v = BaseSample
              <$> v .! 0
              <*> v .! 1
              <*> v .! 2
              <*> v .! 3
              <*> v .! 4
              <*> v .! 5
              <*> v .! 6
              <*> v .! 7
              <*> v .! 8
              <*> v .! 9
              <*> v .! 10
              <*> v .! 11

instance CSV.ToRecord BaseSample

data TimedSample
  = TimedSample
  { tDate        :: Text
  , tAge         :: Int
  , tGender      :: Text
  , tStutter     :: Text
  , tExaminer    :: Text
  , tParticipant :: Int
  , tPassage     :: Text
  , tTime        :: Double
  , tPercentage  :: Double
  , tQuestion1   :: Int
  , tQuestion2   :: Int
  , tQuestion3   :: Int
  , tQuestion4   :: Int
  } deriving (Show, Generic, NFData)

instance CSV.FromRecord TimedSample where
  parseRecord v = TimedSample
              <$> v .! 0
              <*> v .! 1
              <*> v .! 2
              <*> v .! 3
              <*> v .! 4
              <*> v .! 5
              <*> v .! 6
              <*> v .! 7
              <*> v .! 8
              <*> v .! 9
              <*> v .! 10
              <*> v .! 11
              <*> v .! 12

showBaseSample :: BaseSample -> Text
showBaseSample (BaseSample d a g s e part p t q1 q2 q3 q4)
 = foldr (<>) "" $ intersperse ", "
  [ d
  , show a
  , g
  , s
  , e
  , show part
  , p
  , show t
  , show q1
  , show q2
  , show q3
  , show q4
  ]

baseSampleData :: BaseSample -> Text
baseSampleData (BaseSample _ _ _ _ _ _ p t q1 q2 q3 q4)
 = foldr (<>) "" $ intersperse ", "
  [ p
  , show t
  , show q1
  , show q2
  , show q3
  , show q4
  ]

showTimedSample :: TimedSample -> Text
showTimedSample (TimedSample _ _ _ _ _ _ _ t pt q1 q2 q3 q4)
 = foldr (<>) "" $ intersperse ", "
  [ show t
  , show pt
  , show q1
  , show q2
  , show q3
  , show q4
  ]

sortTimed = sortOn tPercentage

showData :: BaseSample -> [TimedSample] -> Text
showData b ts = showBaseSample b <> ", , "
             <> (T.concat . intersperse ", , " $ fmap showTimedSample $ sortTimed ts)
             <> "\n"


sameParticipant a b
  = and [ date a == tDate b
        , gender a == tGender b
        , stutter a == tStutter b
        , examiner a == tExaminer b
        , participant a == tParticipant b
        , passage a == tPassage b
        ]

data Participant = Participant Text Text Text Text Int Text deriving (Show, Eq, Ord)

participantData :: Participant -> Text
participantData (Participant d a g s e part)
 = foldr (<>) "" $ intersperse ", "
  [ d
  , show a
  , g
  , s
  , show e
  , part
  ]

class Sample a where
  getParticipant :: a -> Participant

instance Sample BaseSample where
  getParticipant b = Participant (date b) (gender b) (stutter b) (examiner b) (participant b) (passage b)

instance Sample TimedSample where
  getParticipant t = Participant (tDate t) (tGender t) (tStutter t) (tExaminer t) (tParticipant t) (tPassage t)

class EmptyColumns a where
  emptyColumns :: a -> Text

instance EmptyColumns Participant where
  emptyColumns = const (T.pack $ replicate 6 ',')

instance EmptyColumns BaseSample where
  emptyColumns = const (T.pack $ replicate 9 ',')

instance EmptyColumns TimedSample where
  emptyColumns = const (T.pack $ replicate 9 ',')
