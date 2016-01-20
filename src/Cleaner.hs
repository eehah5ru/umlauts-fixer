module Cleaner (
               extractDefinition
               , CleanerError (..) ) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Utils (parseString)

testString :: String
testString = " \n\
\ a) ungenannt, ohne Namensnennung: \n\
\ein anonymer Verfasser, Brief; \n\
\eine anonyme Anzeige; \n\
\anonyme Waren (No-Name-Produkte); \n\
\ein[en] Traktat anonym herausgeben; \n\
\\n\
\© DUDEN - Das gro§e  der deutschen Sprache,|4. Aufl. Mannheim 2012 [CD-ROM]"

newtype CleanerError = CleanerError [Char] deriving (Show, Eq)

parseTestDefinition :: String
parseTestDefinition = parseString definition testString

extractDefinition :: String -> Either CleanerError String
extractDefinition s = case parse definition "clipboard" s of
                           Left e -> Left . CleanerError . show $ e
                           Right ss -> Right ss

copyrightSign :: Char
copyrightSign = '©'

whiteSpace :: Parser ()
whiteSpace = skipMany1 space

-- definitionText :: Parser (String)
-- definitionText = defAndSpaces <|> definitionText' <?> "definitionText"
--   where definitionText' = many1 $ noneOf [copyrightSign]
--         defAndSpaces = (try definitionText') <* (try $ skipMany1 space)

definitionText :: Parser (String)
-- definitionText = undefined
definitionText = manyTill anyChar (try $ p)
  where p = (optional (many1 space)) >> (optional newline) >> (optional copyright) >> eof

copyright :: Parser ()
copyright = (optional newline) >> char copyrightSign >> many1 anyChar >> return ()

definition :: Parser (String)
definition = do
                (optional whiteSpace)
                t <- definitionText
                return t
