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
  } deriving (Show, Eq, Generic, NFData)

instance Ord BaseSample where
  a <= b = passage a <= passage b

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

baseSampleQuality :: BaseSample -> Text
baseSampleQuality (BaseSample _ _ _ _ _ _ p t q1 q2 q3 q4)
 = foldr (<>) "" $ intersperse ", "
  [ p
  , show t
  , ""
  , ""
  , ""
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

timedSampleQuality :: TimedSample -> Text
timedSampleQuality (TimedSample _ _ _ _ _ _ p t pt q1 q2 q3 q4)
 = foldr (<>) "" $ intersperse ", "
  [ show p
  , ""
  , ""
  , ""
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

samePassage :: BaseSample -> TimedSample -> Bool
samePassage b t = participant b == tParticipant t && passage b == tPassage t

data Participant = Participant Text Int Text Text Text Int deriving (Show, Eq, Ord)

participantData :: Participant -> Text
participantData (Participant d a g s e part)
 = foldr (<>) "" $ intersperse ", "
  [ d
  , show a
  , g
  , s
  , e
  ]

class Sample a where
  getParticipant :: a -> Participant

instance Sample BaseSample where
  getParticipant b = Participant (date b) (age b) (gender b) (stutter b) (examiner b) (participant b)

instance Sample TimedSample where
  getParticipant t = Participant (tDate t) (tAge t) (tGender t) (tStutter t) (tExaminer t) (tParticipant t)

class EmptyColumns a where
  emptyColumns :: a -> Text

instance EmptyColumns Participant where
  emptyColumns = const (T.pack $ replicate 4 ',')

instance EmptyColumns BaseSample where
  emptyColumns = const (T.pack $ replicate 9 ',')

instance EmptyColumns TimedSample where
  emptyColumns = const (T.pack $ replicate 9 ',')

twisters :: [Text]
twisters =
  [ " piper"
  , " seashells"
  , " woodchuck"
  , " tutor"
  , " oyster"
  , " perkins"
  , " perkins"
  , " moses"
  , " blackbear"
  , " chester"
  , " betty"
  ]

isTwister = (`elem` twisters)
