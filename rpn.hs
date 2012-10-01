import Control.Monad
import Data.Char

main :: IO ()
main = do
    runCalc $ Just []
    return ()

runCalc :: Maybe [Double] -> IO ()
runCalc xs = do
    print xs
    line <- getLine
    if toLowers line == "quit"
        then return ()
        else runCalc $ do
            stack <- xs
            foldingFunction stack line

toLowers :: String -> String
toLowers = map toLower

solveRPN :: String -> Maybe Double
solveRPN st = do
    [result] <- foldM foldingFunction [] $ words st
    return result

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
