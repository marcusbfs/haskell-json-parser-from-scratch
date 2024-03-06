module JsonParser where

import Control.Applicative (Alternative (..))
import Data.Char (isDigit, isSpace)

-- Main parser
data Error i e
  = EndOfInput -- Expected more input, but there is nothing
  | Unexpected i -- We didn't expect to find this element
  | CustomError e -- Extra errors the user may want to create
  | Empty -- Used in `Alternative` implementation of `empty`
  deriving (Eq, Show)

--  i: The input stream.
--  e: The type of custom error messages. If we don' have those, we may use Void instead.
--  a: The result of our parsing function. It represents the structure parsed from our consumed input
newtype Parser i e a = Parser
  { runParser :: i -> Either (Error i e) (i, a)
  }

-- Instances of the parser

instance Functor (Parser i e) where
  fmap f (Parser ra) = Parser $ \input -> do
    (input', a) <- ra input
    return (input', f a)

instance Applicative (Parser i e) where
  pure a = Parser $ \input -> Right (input, a)
  (Parser rf) <*> (Parser ra) = Parser $ \input -> do
    (input', f) <- rf input
    (input'', a) <- ra input'
    return (input'', f a)

instance Monad (Parser i e) where
  (Parser ra) >>= f = Parser $ \input -> do
    (input', a) <- ra input
    (input'', b) <- runParser (f a) input'
    return (input'', b)

instance Alternative (Parser i e) where
  empty = Parser $ \_ -> Left Empty
  (Parser ra) <|> (Parser rb) = Parser $ \input -> case ra input of
    Left _ -> rb input
    good -> good

-- Concrete types and functions

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonInteger Integer -- only integer numbers
  | JsonString String -- only strings with no escpaed characters
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

data CustomJsonError = NotImplemented deriving (Show, Eq)

type JError = Error String CustomJsonError

type JParser = Parser String JError

charP :: Char -> JParser Char
charP c = Parser $ \input -> case input of
  "" -> Left $ Unexpected $ "Cannot match char '" ++ [c] ++ "' with an empty string."
  (hc : cs) -> if hc == c then Right (cs, hc) else Left $ Unexpected $ "charP: Expecting '" ++ [c] ++ "' but found '" ++ [hc] ++ "' in input \"" ++ input ++ "\"."

stringP :: String -> JParser String
stringP = mapM charP

takeWhileP :: (Char -> Bool) -> JParser String
takeWhileP predicate = Parser $ \input ->
  let (valid, rest) = span predicate input
   in if valid == ""
        then Left $ Unexpected $ "takeWhileP: cannot apply predicate to input \"" ++ input ++ "\"."
        else Right (rest, valid)

sepByP :: JParser Char -> JParser a -> JParser [a]
sepByP sep element =
  many
    ( (many space *> element <* many space <* sep)
        <|> many space
        *> element
        <* many space
    )

space :: JParser Char
space = Parser $ \input -> case input of
  -- "" -> Right (input, ' ')
  "" -> Left $ Unexpected $ "space: empty string."
  (hc : cs) -> if isSpace hc then Right (cs, hc) else Left $ Unexpected $ "space: Expecting empty space but found '" ++ [hc] ++ "' in input \"" ++ input ++ "\"."

quotedString :: JParser String
quotedString = charP '"' *> takeWhileP (/= '"') <* charP '"'

-- Specific JSON parsers

nullP :: JParser JsonValue
nullP = JsonNull <$ stringP "null"

boolP :: JParser JsonValue
boolP = (\s -> JsonBool (s == "true")) <$> (stringP "true" <|> stringP "false")

integerP :: JParser JsonValue
integerP = JsonInteger . read <$> takeWhileP isDigit

jStringP :: JParser JsonValue
jStringP = JsonString <$> (charP '"' *> takeWhileP (/= '"') <* charP '"')

arrayP :: JParser JsonValue
arrayP = JsonArray <$> (charP '[' *> elements <* charP ']')
  where
    elements = many space *> sepByP (charP ',') jsonP <* many space

objectP :: JParser JsonValue
objectP = JsonObject <$> (charP '{' *> many space *> elements <* many space <* charP '}')
  where
    element =
      (\str _ val -> (str, val))
        <$> quotedString
        <*> (many space *> charP ':' *> many space)
        <*> jsonP
    elements = many space *> sepByP (charP ',') element <* many space

jsonP :: JParser JsonValue
jsonP = many space *> (nullP <|> boolP <|> integerP <|> jStringP <|> arrayP <|> objectP) <* many space
