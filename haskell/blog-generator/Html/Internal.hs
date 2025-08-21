module Html.Internal where

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
h1_ = Structure . el_ "h1" . escape

p_ :: String -> Structure
p_ = Structure . el_ "p" . escape

html_ :: Title -> Structure -> Html
html_ title (Structure body) = Html $ el_ "html" (el_ "head" (el_ "title" title) <> el_ "body" body)

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) = Structure (a <> b)

appendMany_ :: [Structure] -> Structure
appendMany_ = foldr append_ (Structure "")

-- same as:
-- appendMany_ [] = Structure ""
-- appendMany_ (x : xs) = x `append_` (appendMany_ xs)

render :: Html -> String
render html =
  case html of
    Html str -> str

ul_ :: [Structure] -> Structure
ul_ structures = Structure $ el_ "ul" $ concatMap (el_ "li" . escape . getStructureString) structures

ol_ :: [Structure] -> Structure
ol_ structures = Structure $ el_ "ol" $ concatMap (el_ "li" . escape . getStructureString) structures

code_ :: String -> Structure
code_ = Structure . el_ "pre" . escape

escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
   in concatMap escapeChar
