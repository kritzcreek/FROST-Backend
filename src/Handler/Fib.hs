module Handler.Fib where

import Import

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

getFibR :: Int -> Handler Value
getFibR i = return $ object ["value" .= (fibs !! abs i)]