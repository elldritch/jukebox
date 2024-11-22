module Jukebox.HTML (
  frontMatter,
  classNames,
  buttonStylesPrimary,
  buttonStyles,
) where

import Relude

import GitHash (GitInfo, giHash, tGitInfoCwd)
import Text.Blaze.Html5 (Attribute, AttributeValue, Html, a, body, docTypeHtml, link, meta, p, toHtml, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes (charset, class_, content, href, lang, name, rel, target)

version :: GitInfo
version = $$tGitInfoCwd

frontMatter :: Html -> Html
frontMatter children = docTypeHtml ! lang "en" $ do
  H.head $ do
    meta ! charset "utf-8"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    H.title "Jukebox"
    meta ! name "description" ! content "Listen to music with your friends"
    link ! rel "stylesheet" ! href "/static/styles.css"
  body ! class_ "min-h-screen flex flex-col" $ do
    H.div ! class_ "grow" $ children
    H.div ! class_ " pt-4 pb-8 text-center text-sm text-gray-400" $ do
      p $ do
        "Open source on "
        a
          ! href "https://github.com/elldritch/jukebox"
          ! class_ "text-blue-400"
          ! target "_blank"
          $ "GitHub"
      p $ "Jukebox v." <> toHtml (take 7 $ giHash version)

classNames :: [AttributeValue] -> Attribute
classNames = class_ . foldl' (\x y -> x <> " " <> y) ""

baseButtonStyles :: [AttributeValue]
baseButtonStyles =
  [ "rounded-md"
  , "px-3"
  , "py-2"
  , "text-sm"
  , "font-semibold"
  , "shadow-sm"
  , "focus-visible:outline"
  , "focus-visible:outline-2"
  , "focus-visible:outline-offset-2"
  ]

buttonStylesPrimary :: [AttributeValue]
buttonStylesPrimary =
  baseButtonStyles
    <> [ "bg-indigo-600"
       , "text-white"
       , "hover:bg-indigo-500"
       , "focus-visible:outline-indigo-600"
       ]

buttonStyles :: [AttributeValue]
buttonStyles =
  baseButtonStyles
    <> [ "bg-white"
       , "text-gray-900"
       , "hover:bg-gray-50"
       , "ring-1"
       , "ring-inset"
       , "ring-gray-300"
       ]
