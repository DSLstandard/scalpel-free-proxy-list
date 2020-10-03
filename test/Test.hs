{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Test.HUnit
import Text.HTML.Scalpel
import Text.HTML.Scalpel.FreeProxyList

tests :: Test
tests = TestList
	[ TestLabel "scrap https://free-proxy-list.net" testScrap
	]

testScrap :: Test
testScrap = TestCase $ do
	Just proxies <- scrapeURL @String freeProxyListUrl freeProxyListScraper
	print proxies

main :: IO ()
main = do
	void $ runTestTT tests
