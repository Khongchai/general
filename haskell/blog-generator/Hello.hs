module Main where

import Data.Binary (Word8)
import Html (Html, h1_, html_, ol_, p_, render, ul_)

main :: IO ()
main = putStrLn (render myHtml)

myHtml :: Html
myHtml =
  html_ "Hey there!" $
    h1_ "This is my title!"
      <> p_ "This is the first line of the body"
      <> p_ "This is the second line of the body"
      <> ol_ [p_ "First line", p_ "Second line..."]