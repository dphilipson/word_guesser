{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import System.IO
import System.Random

commonFilePath :: FilePath
commonFilePath = "common.txt"

scrabbleFilePath :: FilePath
scrabbleFilePath = "scrabble.txt"

commonLowerBound :: Int
commonLowerBound = 1000

giveUpPhrase :: Text
giveUpPhrase = "give up"

main :: IO ()
main = do
    commonWords <- linesVector commonFilePath
    scrabbleWords <- linesSet scrabbleFilePath
    playGame commonWords scrabbleWords
    return ()

playGame :: Vector Text -> Set Text -> IO ()
playGame commonWords scrabbleWords = do
    secret <- selectRandom commonWords
    TIO.putStrLn "I'm thinking of a word."
    playRounds secret scrabbleWords 0

playRounds :: Text -> Set Text -> Int -> IO ()
playRounds secret scrabbleWords roundNum = do
    mGuess <- getGuess scrabbleWords
    case mGuess of
        Just guess -> case compare secret guess of
            LT -> do
                TIO.putStrLn $ T.concat
                    ["My word comes before \"" , guess, "\" in the dictionary."]
                playRounds secret scrabbleWords $ roundNum + 1
            GT -> do
                TIO.putStrLn $ T.concat
                    ["My word comes after \"", guess, "\" in the dictionary."]
                playRounds secret scrabbleWords $ roundNum + 1
            EQ -> do
                TIO.putStrLn $ T.concat
                    ["You got it! The answer was \"", guess, "\""]
                TIO.putStrLn $ T.concat
                    ["You used ", T.pack $ show $ roundNum + 1, " guesses."]
        Nothing -> TIO.putStrLn $ T.concat
                        ["The answer was \"", secret, "\""]

getGuess :: Set Text -> IO (Maybe Text)
getGuess scrabbleWords = do
    input <- T.toLower <$> prompt
        "Enter a word to guess (or \"give up\" to quit): "
    case () of
        _ | input == giveUpPhrase -> return Nothing
          | S.member input scrabbleWords -> return $ Just input
          | otherwise -> do
                TIO.putStrLn $ T.concat
                    [ "\""
                    , input
                    , "\" is not a word I know. Try something else."]
                getGuess scrabbleWords

linesVector :: FilePath -> IO (Vector Text)
linesVector path = V.fromList <$> canonicalLines path

linesSet :: FilePath -> IO (Set Text)
linesSet path = S.fromList <$> canonicalLines path

canonicalLines :: FilePath -> IO [Text]
canonicalLines path = (map T.strip . T.lines . T.toLower) <$> TIO.readFile path

selectRandom :: Vector Text -> IO Text
selectRandom v = (!) v <$> randomRIO (commonLowerBound, V.length v)

prompt :: Text -> IO Text
prompt p = do
    TIO.putStr p
    hFlush stdout
    TIO.getLine
