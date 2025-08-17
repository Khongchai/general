module Main where

import Html (Html, append_, h1_, html_, p_, render)

main :: IO ()
main = putStrLn (render myHtml)

myHtml :: Html
myHtml =
  html_ "Hey there!" $
    append_ (h1_ "This is my title!") $
      append_ (p_ "This is the first line of the body") (p_ "This is the second line of the body")