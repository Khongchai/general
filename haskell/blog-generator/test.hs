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

makeHtml :: String -> String -> String
makeHtml title body = html_ (head_ (title_ title) <> body_ body)

myHtml :: String
myHtml = makeHtml "My page title" "My page content"

main :: IO ()
main =
  putStrLn myHtml