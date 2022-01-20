{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures#-}
module Text.HTMLEntity.Table where
import Data.HashMap.Strict (fromList, HashMap)
import Data.Text (Text)
import Text.HTMLEntity.TH (litFile)
names :: HashMap Text Text
names
  = fromList (read [litFile|data/Names.hs|] :: [(Text, Text)])

{-# NOINLINE names #-}

entitiesMulti :: HashMap Text Text
entitiesMulti
  = fromList (read [litFile|data/EntitiesMulti.hs|] :: [(Text, Text)])

{-# NOINLINE entitiesMulti #-}

entitiesSingle :: HashMap Char Text
entitiesSingle
  = fromList (read [litFile|data/EntitiesSingle.hs|] :: [(Char, Text)])

{-# NOINLINE entitiesSingle #-}
