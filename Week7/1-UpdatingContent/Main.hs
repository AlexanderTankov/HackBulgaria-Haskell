{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Text.StringLike
import Data.Aeson
import Text.HTML.TagSoup
import Network.HTTP.Conduit

haskellOrgsContent :: IO BL.ByteString
haskellOrgsContent = simpleHttp "https://leanpub.com/gameinhaskell"

checkIsInProgress :: [Tag BL.ByteString] -> Bool
checkIsInProgress (x:y:xs) = x ~== "<em>" && y ~== "Note: this book is in progress" || checkIsInProgress (y:xs)
checkIsInProgress _ = False

getThemеs :: [Tag BL.ByteString] -> [Tag BL.ByteString]
getThemеs (x:xs) =  if x ~== "<ul class=\"toc no-parts\">" then getThemes' xs [] else getThemеs xs where
    getThemes' (x:xs) acc = if x ~== "</ul>" then acc else acc ++ getThemes' xs acc
    getThemes' _ acc = acc
getThemеs _ = []

convertToTags :: BL.ByteString -> [Tag BL.ByteString]
convertToTags bs = parseTags bs

getTagsInTag :: (Tag t) => t -> t -> [Tag BL.ByteString]
getTagsInTag openTag closeTag = takeWhile (~/= closeTag) . dropWhile (~/= openTag)

-- getTextInTag :: (Tag t) => t -> t -> Tag BL.ByteString -> Value
-- getTextInTag openTag closeTag = innerText (getTagsInTag openTag closeTag)
-- 
-- instance ToJSON (Tag BL.ByteString) where
--     toJSON = getTextInTag

main = do
    x <- haskellOrgsContent
    let tags = convertToTags x
    print $ checkIsInProgress $ tags
    BL.writeFile "test.json" $ encode $ getThemеs tags
