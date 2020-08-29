# TimezoneDetect

Haskell bindings to the excellent [ZoneDetect](https://github.com/BertoldVdb/ZoneDetect) library.

## Usage

You'll need timezone database files to work with this library, see instructions [in the original repository](https://github.com/BertoldVdb/ZoneDetect/tree/master/database).

Once you have those files in hand, you'll be able to get a timezone from a given latitude and longitude:

```haskell
> lookupTimeZone "./test/tz_db/timezone21.bin" 40.7831 (-73.9712)
Right "America/New_York"
```
