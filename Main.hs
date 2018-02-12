module Main where

import Tokeniser(alexScanTokens)
import Parser(parse)

main :: IO()
main = do
    s <- getContents
    let 
      tokens = alexScanTokens s
      monotoneProblem = parse tokens
    print tokens
    print monotoneProblem