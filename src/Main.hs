{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Control.Arrow
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
import           Protolude                   hiding (second)
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

printData s = appendFile (s <> "-data.csv")

printQualityScores s = appendFile (s <> "-quality-data.csv")

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

collectKeys :: Ord a => [(a, b)] -> Map a [b]
collectKeys = foldr (\(k, v) -> M.alter (f v) k) (M.fromList [])
  where
    f t (Just ts) = Just (t : ts)
    f t Nothing   = Just [t]

groupByParticipant :: [BaseSample] -> M.Map Participant [BaseSample]
groupByParticipant bs = collectKeys $ fmap (getParticipant &&& identity) bs

groupBySample :: [TimedSample] -> [BaseSample] -> M.Map BaseSample [TimedSample]
groupBySample ts bs = M.fromList $ fmap (f ts) bs
  where
    f ts b = (b, filter (samePassage b) ts)

groupSamples ::
     [TimedSample]
  -> [BaseSample]
  -> M.Map Participant (M.Map BaseSample [TimedSample])
groupSamples ts bs = fmap (groupBySample ts) $ groupByParticipant bs

addNewLine = flip (<>) "\n"

participantColumns p = participantData p : replicate 19 (emptyColumns p)

emptySpace = replicate 10 (T.pack $ replicate 9 ',')

baseSampleColumns b = b ++ emptySpace

timedSampleColumns t = emptySpace ++ t

qualityScore :: BaseSample -> [TimedSample] -> Text
qualityScore b ts =
  T.concat $ intersperse (T.pack $ replicate  11 ',') $ baseSampleQuality b : fmap timedSampleQuality (sortTimedSamples ts)

onBoth f (a, b) = (f a, f b)

sortTimedSamples = sortBy (\a b -> compare (tPercentage a) (tPercentage b))

partitionTwisters :: [(BaseSample, [TimedSample])] -> ([Text], [Text])
partitionTwisters =
  (baseSampleColumns *** timedSampleColumns) .
  onBoth (fmap $ uncurry qualityScore) . partition (isTwister . passage . fst)

concatWithComma x y = x <> ", " <> y

showQualityScores :: M.Map Participant (M.Map BaseSample [TimedSample]) -> Text
showQualityScores = foldr (<>) "" . M.foldMapWithKey f
  where
    f k v =
     fmap addNewLine $ do
     zipWith concatWithComma
      (participantColumns k)
       (uncurry (zipWith concatWithComma) .  partitionTwisters . M.toList $ v)

qualityTest = do
  (b, t) <- setupEnv
  pure $ (groupSamples <$> t <*> b)

-- For Criterion
setupEnv = do
  options <- execParser optsInfo
  let path = folder options
  gatherSamples "data"

main :: IO ()
main = do
  e <- setupEnv
  case e of
    (Right b, Right t) -> do
      let samples = groupSamples t b
      let (pws, pns) = M.partitionWithKey isPWS samples
      let record s ss = do
            printQualityScores s . showQualityScores $ ss
            mapM_ (printData s) . join . fmap (fmap (uncurry showData) . M.toList) . M.elems $ ss
      record "result/both" samples
      record "result/pws" pws
      record "result/pns" pns
    (Left e, Left e') -> do
        print e
        print e'
    (Left e, _) -> print e
    (_, Left e) -> print e
