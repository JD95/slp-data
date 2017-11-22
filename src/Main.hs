{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Control.DeepSeq
import           Control.Parallel
import           Control.Parallel.Strategies
import           Criterion.Main
import           Data.Csv                    ((.!))
import qualified Data.Csv                    as CSV
import           Data.List
import qualified Data.Map                    as M
import           Data.Semigroup              ((<>))
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
import           Lens.Micro.Platform
import           Options.Applicative
import           Prelude                     (String, getContents)
import           Protolude
import           System.Directory

import           Types

data Options = Options
  { folder :: Text
  }

opts :: Parser Options
opts =
  Options <$>
  option
    auto
    (long "folder" <> short 'm' <> metavar "FOLDER" <>
     help "The folder to print" <>
     showDefault <>
     value "data")

optsInfo :: ParserInfo Options
optsInfo =
  info
    (opts <**> helper)
    (fullDesc <> progDesc "Print a simple folder" <>
     header "slp-data - a minimal application")

printData = appendFile "data.csv"

printQualityScores = appendFile "quality-data.csv"

base = T.isSuffixOf "base.csv" . toS

timed = T.isSuffixOf "timed.csv" . toS

readData :: CSV.FromRecord f => Text -> IO (Either String (V.Vector f))
readData path = do
  lines <- readFile . toS $ path
  pure . CSV.decode CSV.NoHeader . toS $ lines

extractSamples ::
     CSV.FromRecord f => (Text -> Bool) -> [Text] -> IO (Either String [f])
extractSamples f =
  fmap (fmap (concat . fmap V.toList) . sequence) .
  sequence . parMap rseq readData . filter f

gatherSamples ::
     Text -> IO (Either String [BaseSample], Either String [TimedSample])
gatherSamples path = do
  contents <-
    (fmap . fmap) (T.append (path <> "/") . toS) . listDirectory . toS $ path
  baseSamples <- extractSamples @BaseSample base contents
  timedSamples <- extractSamples @TimedSample timed contents
  pure (baseSamples, timedSamples)

processSamples f p (Right b, Right t) = do
  mapM_ (p) . parShowData . M.elems $ f t b
processSamples _ _ _ = print "Failed to read values"

parShowData = parMap rdeepseq (uncurry showData)

groupSamples ::
     [TimedSample]
  -> [BaseSample]
  -> M.Map Participant (BaseSample, [TimedSample])
groupSamples ts bs =
  foldr
    (\t m -> M.update (u t) (getParticipant t) m)
    (M.fromList (fmap f bs))
    ts
  where
    f b = (getParticipant b, (b, []))
    u t = Just . second ((:) t)

showQualityScores :: M.Map Participant (BaseSample, [TimedSample]) -> Text
showQualityScores = foldr (<>) "" . M.foldMapWithKey qualityScore

addNewLine = (<>) "\n"

qualityScore :: Participant -> (BaseSample, [TimedSample]) -> [Text]
qualityScore p (b, ts) =
  fmap addNewLine $
  zipWith3
    (\a b c -> a <> b <> c)
    participantColumns
    baseSampleColumns
    timedSampleColumns
  where
    participantColumns = participantData p : replicate 9 (emptyColumns p)
    baseSampleColumns = baseSampleData b : replicate 9 (emptyColumns b)
    timedSampleColumns =
      foldr1 (zipWith (<>)) $
      fmap (\t -> replicate 9 (emptyColumns t) ++ [showTimedSample t]) ts

processQualityScores (Right b, Right t) =
  printQualityScores . showQualityScores $ groupSamples t b

-- For Criterion
setupEnv = do
  options <- execParser optsInfo
  let path = folder options
  gatherSamples "data"

main :: IO ()
main = do
  e <- setupEnv
  processSamples groupSamples printData e
  processQualityScores e
