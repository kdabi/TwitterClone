module Main where

import API.Server (newHTTPHandler)

main :: IO ()
main = newHTTPHandler 8080
