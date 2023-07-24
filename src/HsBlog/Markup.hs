module HsBlog.Markup where

-- Word8 is an 8-bit unsigned integer type
import Data.Maybe
import Data.Word (Word8)
import Numeric.Natural

type Document =
  [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Show)

getStructureString :: Structure -> String
getStructureString = show

hello :: Document
hello = [Paragraph "Hello, world!"]

hello1 :: Document
hello1 = [Heading 1 "* Welcome", Paragraph "To this tutorial about Haskell."]

h2 :: [Structure]
h2 =
  [ Paragraph
      "Remember that multiple lines with no separation are grouped together into a single paragraph but list items remain separate.",
    OrderedList
      ["# Item 1 of a list", "# Item 2 of the same list"]
  ]

example4 :: Document
example4 =
  [ Heading 1 "Compiling programs with ghc",
    Paragraph "Running ghc invokes the Glasgow Haskell Compiler (GHC), and can be used to compile Haskell modules and programs into native executables and libraries.",
    Paragraph "Create a new Haskell source file named hello.hs, and write the following code in it:",
    CodeBlock
      [ "main = putStrLn \"Hello, Haskell!\""
      ],
    Paragraph "Now, we can compile the program by invoking ghc with the file name:",
    CodeBlock
      [ "➜ ghc hello.hs",
        "[1 of 1] Compiling Main             ( hello.hs, hello.o )",
        "Linking hello ..."
      ],
    Paragraph "GHC created the following files:",
    UnorderedList
      [ "hello.hi - Haskell interface file",
        "hello.o - Object file, the output of the compiler before linking",
        "hello (or hello.exe on Microsoft Windows) - A native runnable executable."
      ],
    Paragraph "GHC will produce an executable when the source file satisfies both conditions:",
    OrderedList
      [ "Defines the main function in the source file",
        "Defines the module name to be Main or does not have a module declaration"
      ],
    Paragraph "Otherwise, it will only produce the .o and .hi files."
  ]

replicate1 :: Int -> a -> [a]
replicate1 i x =
  case i of
    0 -> []
    _ -> x : replicate1 (i - 1) x

even1 :: Int -> Bool
even1 v =
  case v of
    0 -> True
    _ -> odd1 (v - 1)

odd1 :: Int -> Bool
odd1 v =
  case v of
    0 -> False
    _ -> even1 (v - 1)

-- | A data type representing colors
data Color
  = RGB Word8 Word8 Word8

getBluePart :: Color -> Word8
getBluePart color =
  case color of
    RGB _ _ blue -> blue

data Brightness
  = Dark
  | Bright

data EightColor
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

data AnsiColor
  = AnsiColor Brightness EightColor

ansiColorToVGA :: AnsiColor -> Color
ansiColorToVGA ansicolor =
  case ansicolor of
    AnsiColor Dark Black ->
      RGB 0 0 0
    AnsiColor Bright Black ->
      RGB 85 85 85
    AnsiColor Dark Red ->
      RGB 170 0 0
    AnsiColor Bright Red ->
      RGB 255 85 85
    _ ->
      RGB 0 0 0

-- and so on

isBright :: AnsiColor -> Bool
isBright x =
  case x of
    AnsiColor Bright _ -> True
    _ -> False

ansiToUbuntu :: AnsiColor -> Color
ansiToUbuntu ansicolor =
  case ansicolor of
    AnsiColor Dark Black ->
      RGB 1 1 1
    AnsiColor Dark Red ->
      RGB 222 56 43
    AnsiColor Dark Green ->
      RGB 57 181 74
    AnsiColor Dark Yellow ->
      RGB 255 199 6
    AnsiColor Dark Blue ->
      RGB 0 111 184
    AnsiColor Dark Magenta ->
      RGB 118 38 113
    AnsiColor Dark Cyan ->
      RGB 44 181 233
    AnsiColor Dark White ->
      RGB 204 204 204
    AnsiColor Bright Black ->
      RGB 128 128 128
    AnsiColor Bright Red ->
      RGB 255 0 0
    AnsiColor Bright Green ->
      RGB 0 255 0
    AnsiColor Bright Yellow ->
      RGB 255 255 0
    AnsiColor Bright Blue ->
      RGB 0 0 255
    AnsiColor Bright Magenta ->
      RGB 255 0 255
    AnsiColor Bright Cyan ->
      RGB 0 255 255
    AnsiColor Bright White ->
      RGB 255 255 255

isEmpty1 :: [a] -> Bool
isEmpty1 x =
  case listToMaybe x of
    Nothing -> True
    Just _ -> False

isEmpty2 :: [a] -> Bool
isEmpty2 x =
  case x of
    [] -> True
    _ : _ -> False

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    -- done case
    [] -> maybeToList context
    -- Heading 1 case
    ('*' : ' ' : line) : rest ->
      maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)
    -- Unordered list case
    ('-' : ' ' : line) : rest ->
      case context of
        Just (UnorderedList list) ->
          parseLines (Just (UnorderedList (list <> [trim line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)
    -- Ordered list case
    ('#' : ' ' : line) : rest ->
      case context of
        Just (OrderedList list) ->
          parseLines (Just (OrderedList (list <> [trim line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (OrderedList [trim line])) rest)
    -- CodeBlock case
    ('>' : ' ' : line) : rest ->
      case context of
        Just (CodeBlock code) ->
          parseLines (Just (CodeBlock (code <> [line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (CodeBlock [line])) rest)
    -- Paragraph case
    currentLine : rest ->
      let line = trim currentLine
       in if line == ""
            then maybe id (:) context (parseLines Nothing rest)
            else case context of
              Just (Paragraph paragraph) ->
                parseLines (Just (Paragraph (unwords [paragraph, line]))) rest
              _ ->
                maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words
