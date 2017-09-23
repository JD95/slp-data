{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import Prelude ()
import Protolude

import Data.Semigroup ((<>))
import Options.Applicative
import Lens.Micro.Platform
import qualified Data.Csv as CSV

data Options = Options { folder :: Text }

opts :: Parser Options
opts = Options <$> option auto
     ( long "folder"
    <> short 'm'
    <> metavar "FOLDER"
    <> help "The folder to print"
    <> showDefault
    <> value "Hello, World!"
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
  , gender :: Char
  , stutter :: Text
  , examiner :: Char
  , participant :: Int
  , passage :: Text
  , time :: Double
  , question1 :: Int
  , question2 :: Int
  , question3 :: Int
  , question4 :: Int
  } deriving (Show, Generic) 

instance CSV.FromRecord BaseSample
instance CSV.ToRecord BaseSample

data TimedSample
  = TimedSample
  { tDate :: Text
  , tAge :: Int
  , tGender :: Char
  , tStutter :: Text
  , tExaminer :: Char
  , tParticipant :: Int
  , tPassage :: Text
  , tTime :: Double
  , tPercetnage :: Double
  , tQuestion1 :: Int
  , tQuestion2 :: Int
  , tQuestion3 :: Int
  , tQuestion4 :: Int
  } deriving (Show, Generic)

instance CSV.FromRecord TimedSample
instance CSV.ToRecord TimedSample

main :: IO ()
main = do
  options <- execParser optsInfo
  putStrLn (folder options)

