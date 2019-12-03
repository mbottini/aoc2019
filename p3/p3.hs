import System.IO
import Text.Read
import Data.List.Split
import Data.Maybe
import System.Environment

inIO :: Monad m => (a -> b) -> a -> m b
inIO = (.) return

getNums :: FilePath -> IO [Int]
getNums path = readFile path >>=
               inIO (splitOn ",") >>=
               inIO (map readMaybe) >>=
               inIO (concatMap maybeToList)
               

getChunk :: Int -> Int -> [a] -> [a]
getChunk start size = take size . drop start

modifyLst :: Int -> a -> [a] -> [a]
modifyLst 0 x (y:ys) = x:ys
modifyLst n x (y:ys) = y : modifyLst (n-1) x ys

deref :: Int -> [Int] -> Int
deref ptr = (!! ptr)

data ProgState = Finished | Running deriving(Show)
data Program = Prog ProgState Int [Int] deriving(Show)

runProgram :: Program -> Program
runProgram p@(Prog Finished _ _) = p
runProgram (Prog _ index mem)
    | mem !! index == 99 = Prog Finished index mem
    | otherwise = runProgram (Prog Running (index+4) newMem)
        where newMem = runOpcode (getChunk index 4 mem) mem

runOpcode :: [Int] -> [Int] -> [Int]
runOpcode (op:p1:p2:p3:[]) lst
    | op == 1 = modifyLst p3 (a1+a2) lst
    | op == 2 = modifyLst p3 (a1*a2) lst
    | otherwise = error "Unknown operand!"
        where a1 = deref p1 lst
              a2 = deref p2 lst 

getFinalState :: Program -> [Int]
getFinalState (Prog Finished _ lst) = lst
getFinalState p = getFinalState (runProgram p)

createProg :: [Int] -> Program
createProg = Prog Running 0

main = do
    path <- getArgs >>= inIO listToMaybe    
    case path of
        Nothing -> putStrLn "Usage: ./p3 <filepath>"
        Just p -> getNums p >>=
                  inIO (modifyLst 1 12) >>=
                  inIO (modifyLst 2 2) >>=
                  inIO createProg >>=
                  inIO getFinalState >>=
                  inIO (show . head) >>=
                  putStrLn
