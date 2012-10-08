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
        Nothing   -> (putStrLn "bad input" >> runCalc xs)
        Just Quit -> return ()
        Just y    -> runCalc $ do
            stack <- xs
            calcRPN stack y

parseInput :: String -> Maybe (Item Double)
parseInput "*"  = return $ Operator (*)
parseInput "+"  = return $ Operator (+)
parseInput "-"  = return $ Operator (-)
parseInput "/"  = return $ Operator (/)
parseInput "quit" = return Quit
parseInput str    = case readMaybe str of
                        Just x  -> return $ Number x
                        Nothing -> Nothing

toLowers :: String -> String
toLowers = map toLower

calcRPN :: [Double] -> (Item Double) -> Maybe [Double]
calcRPN (x:y:ys) (Operator op) = return $ (op y x) : ys
calcRPN xs (Operator _)        = return xs
calcRPN xs (Number x)          = return (x:xs)
calcRPN _  _                   = undefined

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x, "")] -> Just x
                                _         -> Nothing
