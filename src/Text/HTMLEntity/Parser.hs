{-# LANGUAGE OverloadedStrings #-}

module Text.HTMLEntity.Parser where

import Control.Applicative
import Data.Attoparsec.Text as A
import Data.Char
import Data.Functor
import qualified Data.HashMap.Strict as H
import Data.Ix
import Data.Monoid
import qualified Data.Text as T
import qualified Text.HTMLEntity.Table as Table

decodeParser :: Parser T.Text
decodeParser = go
  where
    go =
        eof <|>
        liftA2 (<>) ((A.takeWhile1 (/= '&') <?> "plain text") <|> (ent <?> "entity")) go
    eof = "" <$ endOfInput
    ent = do
        void $ char '&'
        inner <-
            eitherP
                (do void $ char '#'
                    (char 'x' *> hexadecimal) <|> decimal)
                (takeWhile1 (/= ';') <?> "entity name")
        void $ char ';'
        case inner of
            Right ename ->
                case H.lookup ename Table.names of
                    Just u -> pure u
                    Nothing -> fail $ "Unknown entity name " ++ T.unpack ename
            Left n
                | n > ord maxBound -> fail $ show n ++ " is out of Char range"
                | otherwise -> pure $ T.singleton $ chr n

encodeParser :: Parser T.Text
encodeParser = mmany $ multichar <|> singlechar <|> fallback
  where
    mmany p = mmany_p
      where
        mmany_p = ssome_p <|> pure mempty
        ssome_p = liftA2 (<>) p mmany_p
    multichar =
        foldr1 (<|>) $
        map (\(a, b) -> entity b <$ string a) $ H.toList Table.entitiesMulti
    singlechar = do
        c <- specialsClass
        return $
            -- there are named entities for most symbols in the ASCII
            -- range, but there's no need to encode them except for &
            if inRange ('!', '}') c && c /= '&' && c /= '<' && c /= '>'
                then T.singleton c
                else entity $ H.lookupDefault undefined c Table.entitiesSingle
    fallback = do
        c <- anyChar
        return $
            if c < ' ' || c > '~'
                then decEntity c
                else T.singleton c
    specialsClass = satisfy (inClass $ H.keys Table.entitiesSingle)
    entity s = "&" <> s <> ";"
    decEntity c = "&#" <> T.pack (show (ord c)) <> ";"
