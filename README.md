# scalpel-free-proxy-list

scraping proxy lists from https://free-proxy-list.net using scalpel

## Example

copied/modified from test/Test.hs:

```haskell
{-# LANGUAGE TypeApplications #-}

import Text.HTML.Scalpel (scrapeURL)
import Text.HTML.Scalpel.FreeProxyList (freeProxyListUrl, freeProxyListScraper)

main = do
	result <- scrapeURL @String -- to prevent ambiguous type
		freeProxyListUrl freeProxyListScraper
	case result of
		Just proxies -> print proxies
		Nothing -> error $ "unable to download from " <> freeProxyListUrl
```
