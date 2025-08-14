tag_ :: String -> String -> String
tag_ tag content = "<" <> tag <> ">" <> content <> "<" <> tag <> ">"

body_ :: String -> String
body_ = tag_ "body"

html_ :: String -> String
html_ content = "<html>" <> content <> "</html>"

head_ :: String -> String
head_ content = "<head>" <> content <> "</head>"

title_ :: String -> String
title_ content = "<title>" <> content <> "</title>"

makeHtml :: String -> String -> String
makeHtml title body = html_ (head_ (title_ title) <> body_ body)

myHtml :: String
myHtml = makeHtml "My page title" "My page content"

main :: IO ()
main =
  putStrLn myHtml