{-# LANGUAGE TemplateHaskell #-}

-- | The default stylesheet, embedded at compile time.
module Mirror.Css (defaultStylesheet) where

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.FileEmbed (embedFile)

-- | Compile-time-embedded so the shipped binary never depends on a
-- runtime-resolvable data-file path.
--
-- 'decodeUtf8' is partial on invalid UTF-8 — normally excluded by this
-- project's ban on partial functions — but its only input here is the
-- static, plain-ASCII asset at @src/Mirror/Css/Default.css@, checked
-- into this repository and authored by Mirror itself, never user
-- data. This is the same kind of narrow, explicit, justified exception
-- already granted to the DOCX zip-archive boundary (§0, rule 7) —
-- declared, not silent — and because 'defaultStylesheet' is forced
-- eagerly the first time any conversion runs without a @--css@
-- override, a corrupted asset would fail every such run immediately,
-- not lurk unnoticed.
defaultStylesheet :: Text
defaultStylesheet = decodeUtf8 $(embedFile "src/Mirror/Css/Default.css")
