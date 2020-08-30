# TimezoneDetect

![build](https://github.com/lfborjas/timezone-detect/workflows/Haskell%20CI/badge.svg)


Haskell bindings to the excellent [ZoneDetect](https://github.com/BertoldVdb/ZoneDetect) library, plus additional
UNIX-aware facilities to determine the UTC time of a given local time in a latitude and longitude.

## Usage

You'll need timezone database files to work with this library, see instructions [in the original repository](https://github.com/BertoldVdb/ZoneDetect/tree/master/database).

Once you have those files in hand, you'll be able to get a timezone from a given latitude and longitude:

```haskell
> lookupTimezone "./test/tz_db/timezone21.bin" 40.7831 (-73.9712) :: Maybe TimeZoneName
Just "America/New_York"
```

Additionally, we now depend on the [timezone-series](https://hackage.haskell.org/package/timezone-series) and [timezone-olson](https://hackage.haskell.org/package/timezone-olson)

With that, you can look up the UTC time at a point in time and space:

```haskell
>>> localWinter <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" "2019-12-25 00:30:00"
>>> utcTime <- timeAtPointToUTC "./test/tz_db/timezone21.bin" 40.7831 (-73.9712) localWinter
2019-12-25 05:30:00 UTC

>>> localSummer <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" "2019-07-25 00:30:00"
>>> utcTime <- timeAtPointToUTC "./test/tz_db/timezone21.bin" 40.7831 (-73.9712) localWinter
2019-12-25 04:30:00 UTC
```
