{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import Prelude (String, getContents)
import Protolude

import Data.Semigroup ((<>))
import Options.Applicative
import Lens.Micro.Platform
import qualified Data.Csv as CSV
import Data.Csv ((.!))
import System.Directory
import qualified Data.Vector as V
import qualified Data.Text as T

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
  } deriving (Show, Generic) 

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
  , tPercetnage :: Double
  , tQuestion1 :: Int
  , tQuestion2 :: Int
  , tQuestion3 :: Int
  , tQuestion4 :: Int
  } deriving (Show, Generic)

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
              
instance CSV.ToRecord TimedSample

base = T.isSuffixOf "base.csv" . toS
timed = T.isSuffixOf "timed.csv" . toS

readData :: CSV.FromRecord f => Text -> IO (Either String (V.Vector f))
readData path = do
  lines <- readFile . toS $ path
  pure . CSV.decode CSV.NoHeader . toS $ lines

extractSamples :: CSV.FromRecord f => (Text -> Bool) -> [Text] -> IO [Either String (V.Vector f)]
extractSamples f =  sequence . fmap readData . filter f 

main :: IO ()
main = do
  options <- execParser optsInfo
  let path = folder options
  contents <- (fmap . fmap) (T.append (path <> "/") . toS) . listDirectory . toS $ path
  baseSamples <- extractSamples @BaseSample base contents
  timedSamples <- extractSamples @TimedSample timed contents
  mapM_ print baseSamples
  pure ()


