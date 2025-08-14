el_ :: String -> String -> String
el_ tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

body_ :: String -> String
body_ = el_ "body"

html_ :: String -> String
html_ = el_ "html"

head_ :: String -> String
head_ = el_ "head"

title_ :: String -> String
title_ = el_ "title"

p_ :: String -> String
p_ = el_ "p"

h1_ :: String -> String
h1_ = el_ "h1"

makeHtml :: String -> String -> String
makeHtml title body = html_ (head_ (title_ title) <> body_ body)

myHtml :: String
myHtml =
  makeHtml
    "Hey there!"
    (h1_ "Do you want to see" <> p_ "Deez nuts?!")

main :: IO ()
main =
  putStrLn myHtml