module HsBlog.Convert where

import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading n txt -> Html.h_ n txt
    Markup.Paragraph p -> Html.p_ p
    Markup.UnorderedList list -> Html.ul_ $ map Html.p_ list
    Markup.OrderedList list -> Html.ol_ $ map Html.p_ list
    Markup.CodeBlock code -> Html.code_ (unlines code)

concatStructure :: [Html.Structure] -> Html.Structure
concatStructure list =
  case list of
    x : rest -> x <> concatStructure rest
    [] -> Html.empty_

convert :: Html.Title -> Markup.Document -> Html.Html
convert title =
  Html.html_ title . foldMap convertStructure

-- addWithInput :: Int -> Int
-- addWithInput n = readIntFromStdin + n

-- tmain =
--   let result1 = addWithInput 1
--       result2 = addWithInput 2
--    in print (result2 - result1)

process :: Html.Title -> String -> String
process title =
  Html.render . convert title . Markup.parse