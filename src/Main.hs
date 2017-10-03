{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main where

import           Control.DeepSeq
import           Control.Parallel
import           Control.Parallel.Strategies
import           Criterion.Main
import           Data.Csv ((.!))
import qualified Data.Csv as CSV
import qualified Data.Map as M
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V
import           Lens.Micro.Platform
import           Options.Applicative
import           Prelude (String, getContents)
import           Protolude
import           System.Directory

data Options = Options { folder :: Text }

opts :: Parser Options
opts = Options <$> option auto
     ( long "folder"
    <> short 'm'
    <> metavar "FOLDER"
    <> help "The folder to print"
    <> showDefault
    <> value "data"
      )

optsInfo :: ParserInfo Options
optsInfo = info (opts <**> helper)
  ( fullDesc
  <> progDesc "Print a simple folder"
  <> header "slp-data - a minimal application"
  )

data BaseSample
  = BaseSample
  { date :: Text
  , age :: Int
  , gender :: Text 
  , stutter :: Text
  , examiner :: Text
  , participant :: Int
  , passage :: Text
  , time :: Double
  , question1 :: Int
  , question2 :: Int
  , question3 :: Int
  , question4 :: Int
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
  { tDate :: Text
  , tAge :: Int
  , tGender :: Text 
  , tStutter :: Text
  , tExaminer :: Text 
  , tParticipant :: Int
  , tPassage :: Text
  , tTime :: Double
  , tPercentage :: Double
  , tQuestion1 :: Int
  , tQuestion2 :: Int
  , tQuestion3 :: Int
  , tQuestion4 :: Int
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

printData = appendFile "data.csv"

sameParticipant a b
  = and [ date a == tDate b
        , gender a == tGender b
        , stutter a == tStutter b
        , examiner a == tExaminer b
        , participant a == tParticipant b
        , passage a == tPassage b
        ]

data Participant = Participant Text Text Text Text Int Text deriving (Show, Eq, Ord)

class Sample a where
  getParticipant :: a -> Participant

instance Sample BaseSample where
  getParticipant b = Participant (date b) (gender b) (stutter b) (examiner b) (participant b) (passage b)

instance Sample TimedSample where
  getParticipant t = Participant (tDate t) (tGender t) (tStutter t) (tExaminer t) (tParticipant t) (tPassage t)

base = T.isSuffixOf "base.csv" . toS
timed = T.isSuffixOf "timed.csv" . toS

readData :: CSV.FromRecord f => Text -> IO (Either String (V.Vector f))
readData path = do
  lines <- readFile . toS $ path
  pure . CSV.decode CSV.NoHeader . toS $ lines

extractSamples :: CSV.FromRecord f => (Text -> Bool) -> [Text] -> IO (Either String [f])
extractSamples f = fmap (fmap (concat. fmap V.toList) . sequence)
                 . sequence
                 . parMap rseq readData
                 . filter f 

gatherSamples :: Text -> IO (Either String [BaseSample], Either String [TimedSample])
gatherSamples path = do
  contents <- (fmap . fmap) (T.append (path <> "/") . toS) . listDirectory . toS $ path
  baseSamples <- extractSamples @BaseSample base contents
  timedSamples <- extractSamples @TimedSample timed contents
  pure (baseSamples, timedSamples)

processSamples f (Right b, Right t) = do
  mapM_ (printData) . parShowData . M.elems $ f t b 
processSamples _ _ = print "Failed to read values"

parShowData = parMap rdeepseq (uncurry showData) 

groupSamples :: [TimedSample] -> [BaseSample] -> M.Map Participant (BaseSample, [TimedSample])
groupSamples ts bs = foldr (\t m -> M.update (u t) (getParticipant t) m) (M.fromList (fmap f bs)) ts 
  where f b = (getParticipant b, (b, []))
        u t = Just . second ((:) t)
        
-- For Criterion
setupEnv = do
  options <- execParser optsInfo
  let path = folder options
  gatherSamples "data" 
 
main :: IO ()
main = setupEnv >>= processSamples groupSamples 


