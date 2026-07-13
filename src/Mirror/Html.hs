{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

-- | A minimal HTML combinator library whose type index makes escaping
-- a static, checked property instead of a convention to remember.
module Mirror.Html
  ( Safety (..)
  , Html
  , escape
  , raw
  , tag
  , selfClosingTag
  , attr
  , renderHtml
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

data Safety = Escaped | Trusted

-- | A value of type @Html 'Escaped@ carries a static guarantee, checked
-- by GHC, that every piece of document-derived text reachable inside
-- it — element content *and* attribute values alike — passed through
-- 'escape' before being combined. 'Escaped' and 'Trusted' trees cannot
-- be mixed: 'tag'\/'selfClosingTag' combine only same-safety children.
-- 'renderHtml' accepts only 'Escaped'.
data Html (s :: Safety) where
  HtmlEscaped :: Text -> Html 'Escaped
  HtmlTrusted :: Text -> Html 'Trusted

-- | The one shared escaping table, used for both element content and
-- (quoted) attribute values — one function to audit, not two that must
-- be kept in sync.
escapeChar :: Char -> Text
escapeChar c = case c of
  '&'  -> "&amp;"
  '<'  -> "&lt;"
  '>'  -> "&gt;"
  '"'  -> "&quot;"
  '\'' -> "&#39;"
  _    -> Text.singleton c

escape :: Text -> Html 'Escaped
escape = HtmlEscaped . Text.concatMap escapeChar

-- | Escape hatch reserved for markup Mirror itself authors as a
-- compile-time literal. Never applied to parsed document content.
raw :: Text -> Html 'Trusted
raw = HtmlTrusted

class ToHtmlText (s :: Safety) where
  htmlText :: Html s -> Text
instance ToHtmlText 'Escaped where htmlText (HtmlEscaped t) = t
instance ToHtmlText 'Trusted where htmlText (HtmlTrusted t) = t

class Combine (s :: Safety) where
  wrap :: Text -> Html s
instance Combine 'Escaped where wrap = HtmlEscaped
instance Combine 'Trusted where wrap = HtmlTrusted

-- | Attribute values are escaped through the same table as content.
tag :: (ToHtmlText s, Combine s) => Text -> [(Text, Text)] -> [Html s] -> Html s
tag name attrs children = wrap $
  "<" <> name <> attrsText <> ">"
    <> mconcat (map htmlText children)
    <> "</" <> name <> ">"
  where
    attrsText = mconcat
      [ " " <> k <> "=\"" <> Text.concatMap escapeChar v <> "\"" | (k, v) <- attrs ]

selfClosingTag :: Combine s => Text -> [(Text, Text)] -> Html s
selfClosingTag name attrs = wrap $ "<" <> name <> attrsText <> " />"
  where
    attrsText = mconcat
      [ " " <> k <> "=\"" <> Text.concatMap escapeChar v <> "\"" | (k, v) <- attrs ]

attr :: Text -> Text -> (Text, Text)
attr = (,)

renderHtml :: Html 'Escaped -> Text
renderHtml = htmlText
