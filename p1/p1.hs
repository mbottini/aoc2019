import System.IO
import Text.Read
import Data.Maybe
import System.Environment

fuelRequired :: Int -> Int
fuelRequired mass = mass `div` 3 - 2

inIO :: Monad m => (a -> b) -> a -> m b
inIO f = return . f

getMasses :: FilePath -> IO [Int]
getMasses path = readFile path >>=
                 inIO lines >>=
                 inIO (map readMaybe) >>=
                 inIO (concatMap maybeToList)

fuelSpaceship :: FilePath -> IO Int
fuelSpaceship path = getMasses path >>=
                     inIO (map fuelRequired) >>=
                     inIO sum

main = do
    path <- getArgs >>= inIO listToMaybe    
    case path of
        Nothing -> putStrLn "Usage: ./p1 <filepath>"
        Just p -> fuelSpaceship p >>=
                  inIO show >>=
                  putStrLn
