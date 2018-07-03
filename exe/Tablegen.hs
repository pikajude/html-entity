{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aeson hiding (pairs)
import qualified Data.ByteString as B
import Data.Function
import qualified Data.HashMap.Strict as H
import Data.List
import Data.Monoid
import Data.Ord
import qualified Data.Text as T
import Data.Tuple
import Language.Haskell.Exts.Build
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax hiding (String)

entities :: IO Value
entities = either error id . eitherDecodeStrict @Value <$> B.readFile "data/entities.json"

main :: IO ()
main = do
    e <- entities
    writeFile "src/Text/HTMLEntity/Table.hs" $ prettyPrint $ mkmod e

mkmod :: Value -> Module ()
mkmod (Object es) =
    Module
        ()
        (Just (ModuleHead () (ModuleName () "Text.HTMLEntity.Table") Nothing Nothing))
        [OptionsPragma () (Just GHC) "-fno-warn-missing-signatures"]
        [ ImportDecl
              ()
              (ModuleName () "Data.HashMap.Strict")
              False
              False
              False
              Nothing
              Nothing
              (Just (ImportSpecList () False [IVar () (name "fromList")]))
        , ImportDecl
              ()
              (ModuleName () "Data.Text")
              False
              False
              False
              Nothing
              Nothing
              (Just (ImportSpecList () False [IVar () (name "pack")]))
        ]
        [ nameBind (name "names") $
          app (var $ name "fromList") $
          listE $ map asMapPair $ nubBy ((==) `on` fst) $ sortBy (comparing fst) pairs
        , noinline "names"
        , nameBind (name "entitiesMulti") $
          app (var $ name "fromList") $
          listE $
          map (asMapPair . swap) $
          nubBy ((==) `on` snd) $
          sortBy (comparing snd <> comparing (T.length . fst)) $
          filter (\(_, b) -> T.length b > 1) pairs
        , noinline "entitiesMulti"
        , nameBind (name "entitiesSingle") $
          app (var $ name "fromList") $
          listE $
          map (asMapCharText . swap) $
          nubBy ((==) `on` snd) $
          sortBy (comparing snd <> comparing (T.length . fst)) $
          filter (\(_, b) -> T.length b == 1) pairs
        , noinline "entitiesSingle"
        ]
  where
    asMapCharText (key, val) =
        tuple [charE $ T.head key, app (var $ name "pack") (textE val)]
    asMapPair (key, val) =
        tuple [app (var $ name "pack") (textE key), app (var $ name "pack") (textE val)]
    noinline x = InlineSig () False Nothing (UnQual () (name x))
    pairs :: [(T.Text, T.Text)]
    pairs =
        map
            (\(x, Object y) ->
                 ( clean x
                 , let String y' = y H.! "characters"
                    in y')) $
        H.toList es
    clean = T.takeWhile (/= ';') . T.dropWhile (== '&')
    textE = strE . T.unpack
mkmod _ = undefined

fromSrc :: String -> Exp ()
fromSrc s =
    let ParseOk x = parseExp s
     in () <$ x
