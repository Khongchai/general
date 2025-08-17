newtype Html = Html String

-- myFunc :: (b -> c) -> (a -> b) -> (a -> c)
-- myFunc f g x = f (g x)

newtype Structure = Structure String

type Title = String

getStructureString :: Structure -> String
getStructureString struct =
  case struct of
    Structure str -> str

el_ :: String -> String -> String
el_ tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

h1_ :: String -> Structure
h1_ = Structure . el_ "h1"

p_ :: String -> Structure
p_ = Structure . el_ "p"

_html :: Title -> Structure -> String
_html title (Structure body) = el_ "html" (el_ "head" (el_ "title" title) <> el_ "body" body)

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) = Structure (a <> b)

render :: Html -> String
render html =
  case html of
    Html str -> str

myHtml :: String
myHtml =
  _html "Hey there!" $
    append_ (h1_ "This is my title!") $
      append_ (p_ "This is the first line of the body") (p_ "This is the second line of the body")

main :: IO ()
main =
  putStrLn myHtml