{-# LANGUAGE TypeApplications #-}

import Test.Tasty
import Test.Tasty.HUnit
import Text.HTML.Scalpel
import Text.HTML.Scalpel.FreeProxyList

test :: TestTree
test = testCase ("scrap " <> freeProxyListUrl) $ do
	result <- scrapeURL @String freeProxyListUrl freeProxyListScraper
	case result of
		Just proxies -> print proxies
		Nothing -> error $ "unable to download from " <> freeProxyListUrl

main :: IO ()
main = defaultMain test
