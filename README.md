# TimezoneDetect

![build](https://github.com/lfborjas/timezone-detect/workflows/Haskell%20CI/badge.svg)


Haskell bindings to the excellent [ZoneDetect](https://github.com/BertoldVdb/ZoneDetect) library, plus additional
UNIX-aware facilities to determine the UTC time of a given local time in a latitude and longitude.

## Usage

You'll need timezone database files to work with this library, see instructions [in the original repository](https://github.com/BertoldVdb/ZoneDetect/tree/master/database).
A copy is provided in the `test` directory of this repository, but it's intentionally not bundled in the package. We make no guarantees of its correctness,
we recommend you use the original authors' files!


### Timezone Name Lookup

Once you have those files in hand, you'll be able to get a timezone from a given latitude and longitude:

```haskell
>>> db <- openTimeZoneDatabase "./test/tz_db/timezone21.bin" 
>>> let tz = lookupTimeZoneName db 40.7831 (-73.9712) :: Maybe TimeZoneName
Just "America/New_York"
>>> closeTimeZoneDatabase db
```

You can use `withTimeZoneDatabase` to "bracket" access to the file (take care of opening and closing,)
but if all you want to do is do a one-off lookup, a convenience function that opens and closes the file when done
is also provided, specialized to `IO`:

```haskell
>>> tz <- lookupTimeZoneNameFromFile "./test/tz_db/timezone21.bin" 40.7831 (-73.9712)
"America/New_York"
```

### LocalTime to UTCTime conversion

Additionally, we depend on the [timezone-series](https://hackage.haskell.org/package/timezone-series) and [timezone-olson](https://hackage.haskell.org/package/timezone-olson) packages to add awareness of `tz database` information.

With that, you can look up the UTC time at a point in time and space:

```haskell
>>> import Data.Time
>> db <- openTimeZoneDatabase "./test/tz_db/timezone21.bin"
>>> localWinter <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" "2019-12-25 00:30:00"
>>> utcTime <- timeAtPointToUTC db 40.7831 (-73.9712) localWinter
2019-12-25 05:30:00 UTC

>>> localSummer <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" "2019-07-25 00:30:00"
>>> utcTime <- timeAtPointToUTC db 40.7831 (-73.9712) localWinter
2019-07-25 04:30:00 UTC
>>> closeTimeZoneDatabase db
```

You can also opt to obtain the timezone name separately (if you wanted to isolate that as a failure scenario,)
and, once in possession of it, use `timeInTimeZoneToUTC`:

```haskell
>>> localSummer <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d %T" "2019-07-25 00:30:00"
>>> utcTime <- timeInTimeZoneToUTC "America/New_York" localSummer
2019-07-25 04:30:00 UTC
```

## Copyright

This library is released under the GPL v2; but the license for the underlying C library bears the following copyright:

> Copyright (c) 2018, Bertold Van den Bergh (vandenbergh@bertold.org) 
> All rights reserved.
> Redistribution and use in source and binary forms, with or without
> modification, are permitted provided that the following conditions are met:
>    * Redistributions of source code must retain the above copyright
>       notice, this list of conditions and the following disclaimer.
>    * Redistributions in binary form must reproduce the above copyright
>       notice, this list of conditions and the following disclaimer in the
>       documentation and/or other materials provided with the distribution.
>    * Neither the name of the author nor the
>       names of its contributors may be used to endorse or promote products
>       derived from this software without specific prior written permission.
>
> THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
> ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
> WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
> DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR DISTRIBUTOR BE LIABLE FOR ANY
> DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
> (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
> LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
> ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
> (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
> SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
