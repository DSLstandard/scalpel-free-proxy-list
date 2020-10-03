{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.HTML.Scalpel.FreeProxyList
( Anonymity(..)
, Entry(..)
, freeProxyListUrl
, freeProxyListScraper
)
where

import Text.HTML.Scalpel.Core
import Text.Parsec hiding ((<|>))
import Text.StringLike
import qualified Data.ByteString as BS

data Anonymity = Anonymous | EliteProxy | Transparent | Level3
	deriving (Show, Eq)

data Entry
	= Entry
		{ entryIp :: BS.ByteString
		, entryPort :: Int
		, entryCode :: BS.ByteString
		, entryCountry :: BS.ByteString
		, entryAnonymity :: Anonymity
		, entryGoogle :: Bool
		, entryHttps :: Bool
		, entryLastCheckedSeconds :: Int
		}
	deriving (Show, Eq)

freeProxyListUrl :: String
freeProxyListUrl = "https://free-proxy-list.net"

freeProxyListScraper :: (StringLike str, Monad m) => ScraperT str m [Entry]
freeProxyListScraper = do
	chroots ("table" @: ["id" @= "proxylisttable"] // "tbody" // "tr") $ inSerial $ do
		Entry
			<$> (nextData)
			<*> (read <$> nextData)
			<*> (nextData)
			<*> (nextData)
			<*> (nextAnonymity)
			<*> (nextBool)
			<*> (nextBool)
			<*> (nextLastChecked)
	where
	nextData :: (Monad m, StringLike a, StringLike b) => SerialScraperT a m b
	nextData = castString <$> seekNext (text "td")

	nextBool :: (Monad m, StringLike a) => SerialScraperT a m Bool
	nextBool = nextData >>= \(x :: String) -> case x of
		"yes" -> pure True
		"no" -> pure False
		_ -> error "unknown boolean"

	nextLastChecked :: (Monad m, StringLike a) => SerialScraperT a m Int
	nextLastChecked = do
		x :: String <- nextData
		case (parse lastCheckedParser "" x) of
			Left y -> error $ show y
			Right z -> pure z

	nextAnonymity :: (Monad m, StringLike a) => SerialScraperT a m Anonymity
	nextAnonymity = nextData >>= \(x :: String) -> case x of
		"level3"      -> pure Level3
		"anonymous"   -> pure Anonymous
		"elite proxy" -> pure EliteProxy
		"transparent" -> pure Transparent
		_ -> error "unknown anonymity"

lastCheckedParser :: Stream s m Char => ParsecT s u m Int
lastCheckedParser = do
	h <- go "hour"
	m <- go "minute"
	s <- go "second"
	pure $ h * 3600 + m * 60 + s
	where
		go str = option 0 $ try $ read <$> many1 digit <* space <* string str <* optional (char 's') <* space
