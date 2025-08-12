body_ content = "<body>" <> content <> "</body>"

html_ content = "<html>" <> content <> "</html>"

head_ content = "<head>" <> content <> "</head>"

title_ content = "<title>" <> content <> "</title>"

makeHtml title body = html_ (head_ (title_ title) <> body_ body)

myHtml = makeHtml "My page title" "My page content"

main =
  putStrLn myHtml