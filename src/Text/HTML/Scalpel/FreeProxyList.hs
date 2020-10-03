{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | a very short example:
--
-- >>> import Text.HTML.Scalpel (scrapeURL)
-- >>> scrapeURL freeProxyListUrl freeProxyListScraper
-- [Entry {entryIp = ...}, ...]
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

-- | according to https://free-proxy-list.net
--
-- There are 3 levels of proxies according to their anonymity.
--
--   * Level 1 - Elite Proxy / Highly Anonymous Proxy: The websites can't detect you are using a proxy.
--
--   * Level 2 - Anonymous Proxy: The websites know you are using a proxy but can't know your real IP.
--
--   * Level 3 - Transparent Proxy: The websites know you are using a proxy as well as your real IP.
--
-- for some reason `Level3` and `Transparent` are distinct from each other? i've no idea why so i just made seperate constructors for them
data Anonymity = Anonymous | EliteProxy | Transparent | Level3
	deriving (Show, Eq)

-- | an actual proxy entry from free-proxy-list's table of proxies
data Entry
	= Entry
		{ entryIp :: BS.ByteString
		, entryPort :: Int
		, entryCode :: BS.ByteString -- ^the country code of the proxy (e.g - US, UK)
		, entryCountry :: BS.ByteString -- ^the full name of the country
		, entryAnonymity :: Anonymity
		, entryGoogle :: Bool -- ^can it connect to google? (usually it can't)
		, entryHttps :: Bool -- ^can it do https?
		, entryLastCheckedSeconds :: Int -- ^how many seconds has it been since the last check, this field is very rough tho
		}
	deriving (Show, Eq)

-- | @freeProxyListUrl == "https://free-proxy-list.net"@
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
