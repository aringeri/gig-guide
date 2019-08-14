# gig-guide
A visual gig guide for events in Melbourne. Check it out [here](http://104.196.248.201/)!

Written with Haskell and Leaflet.

Current event data sources:
* beat.com.au

### Build with cabal
```
cabal v2-build
```

### Build with stack
```
stack build
```
---
The build will produce the following webserver executable and scraping utilities.

`webserver` : server app that responds to web requests (default port 80).

`scrape-venues` : utility to gather and store venue data from online sources.

`geocode-venues` : utility to [geocode](https://en.wikipedia.org/wiki/Geocoding) venue locations based on text addresses.

`scrape-events` : utility to gather and store event data from online sources.
