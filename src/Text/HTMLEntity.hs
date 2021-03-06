-- | Efficient decoding and encoding of HTML entities in text.
module Text.HTMLEntity
    ( -- * Usage
      decode
    , decode'
    , encode
    -- ** Partial decoding/encoding
    -- $partial
    , decodePartial
    , encodePartial
    ) where

import Data.Attoparsec.Text
import Data.Text (Text)
import Text.HTMLEntity.Parser

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text.IO as T

-- *** Decoding/encoding
-- | Decode HTML entities contained in the given text. Returns
-- @Left decodeError@ on failure. The parser will do its best to explain
-- the problem.
--
-- >>> mapM_ T.putStrLn $ decode "H&eacute;llo w&oast;rld!"
-- Héllo w⊛rld!
--
-- >>> decode "&NonExistentEntity;"
-- Left "entity: Failed reading: Unknown entity name NonExistentEntity"
--
-- >>> decode "&#100000000;"
-- Left "entity: Failed reading: 100000000 is out of Char range"
--
-- >>> decode "&#xffffffff;"
-- Left "entity: Failed reading: 4294967295 is out of Char range"
decode :: Text -> Either String Text
decode = parseOnly decodeParser
{-# INLINE decode #-}

-- | Like 'decode', except that if a decode error occurs, the original
-- output is returned unmodified. Use if you're certain that your input is
-- well-formed.
--
-- >>> T.putStrLn $ decode' "W&esdot;ll-formed inpu&DoubleRightTee;"
-- W≐ll-formed inpu⊨
--
-- >>> T.putStrLn $ decode' "Utter n&#2322548;ns&CurlyE;nse"
-- Utter n&#2322548;ns&CurlyE;nse
decode' :: Text -> Text
decode' n = either (const n) id $ decode n
{-# INLINE decode' #-}

-- | Encodes the input for use as text in an HTML document.
--
-- 'encode' will use named entities where possible, except for most symbols
-- in the ASCII block, where it was deemed this would result in
-- unnecessarily bloated output.
--
-- >>> T.putStrLn $ encode "Héllo wörld!"
-- H&eacute;llo w&ouml;rld!
--
-- >>> T.putStrLn $ encode "x ≂̸ y"
-- x &nesim; y
--
-- >>> T.putStrLn $ encode "\2534\6188"
-- &#2534;&#6188;
encode :: Text -> Text
encode = either (error "html-entity internal encoding error") id . parseOnly encodeParser
{-# INLINE encode #-}

{- $partial
These functions are provided for convenience if you're using attoparsec
in a streaming style, and all return 'Result' values. Use them as you
would normally.
-}
-- | Partial 'decode'.
decodePartial :: Text -> Result Text
decodePartial = parse decodeParser
{-# INLINE decodePartial #-}

-- | Partial 'encode'.
encodePartial :: Text -> Result Text
encodePartial = parse encodeParser
{-# INLINE encodePartial #-}
