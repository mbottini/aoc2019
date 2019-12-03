import System.IO
import Text.Read
import Data.Maybe
import System.Environment

iterateFuel :: Int -> [Int]
iterateFuel = tail . iterate (\x -> x `div` 3 - 2)

getFuelCost :: Int -> Int
getFuelCost mass = sum . 
                   takeWhile (\x -> x >= 0) $ 
                   iterateFuel mass

inIO :: Monad m => (a -> b) -> a -> m b
inIO f = return . f

getMasses :: FilePath -> IO [Int]
getMasses path = readFile path >>=
                 inIO lines >>=
                 inIO (map readMaybe) >>=
                 inIO (concatMap maybeToList)

fuelSpaceship :: FilePath -> IO Int
fuelSpaceship path = getMasses path >>=
                     inIO (map getFuelCost) >>=
                     inIO sum

main = do
    path <- getArgs >>= inIO listToMaybe    
    case path of
        Nothing -> putStrLn "Usage: ./p1 <filepath>"
        Just p -> fuelSpaceship p >>=
                  inIO show >>=
                  putStrLn
