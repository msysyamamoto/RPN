import Control.Monad
import Data.Char

data Item a = Number Double | Operator (a -> a -> a)| Quit

main :: IO ()
main = do
    runCalc $ Just []
    return ()

runCalc :: Maybe [Double] -> IO ()
runCalc xs = do
    print xs
    line <- getLine
    case parseInput line of
        Nothing   -> runCalc xs
        Just Quit -> return () 
        Just y    -> runCalc $ do
            stack <- xs
            foldingFunction' stack y

isQuit :: String -> Bool
isQuit = ("quit" ==) . toLowers

parseInput :: String -> Maybe (Item Double)
parseInput ['*']  = return $ Operator (*)
parseInput "quit" = return Quit
parseInput str    = case readMaybe str of
                        Just x  -> return $ Number (x ::Double)
                        Nothing -> Nothing

toLowers :: String -> String
toLowers = map toLower

solveRPN :: String -> Maybe Double
solveRPN st = do
    [result] <- foldM foldingFunction [] $ words st
    return result

foldingFunction' :: [Double] -> (Item Double) -> Maybe [Double]
foldingFunction' (x:y:ys) (Operator op) = return $ (op y x) : ys 
foldingFunction' xs (Number x) = return (x:xs)

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*"    = return $ (y * x) :ys
foldingFunction (x:y:ys) "+"    = return $ (y + x) :ys
foldingFunction (x:y:ys) "-"    = return $ (y - x) :ys
foldingFunction (0:_:_)  "/"    = Nothing
foldingFunction (x:y:ys) "/"    = return $ (y / x) :ys
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x, "")] -> Just x
                                _         -> Nothing
