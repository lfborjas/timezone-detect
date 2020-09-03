# Changelog for timezone-detect

## v0.3.0.1rc 

** In development, not released to hackage yet!**

* Adds property tests

## v0.3.0.0 (2020-09-02)

**Breaking Changes!**

* Introduce `openTimeZoneDatabase` and `closeTimeZoneDatabase` to hew closer to
  the underlying library's intended usage. And `withTimeZoneDatabase` to manage the
  opening and closing of the TZ file around an IO computation with it.
* Changes the signature of `lookupTimeZoneName` to take a timezone database, not
  a file, same with `timeAtPointToUTC`. Introduces `*FromFile` variants that
  work with the path to the DB file and manage the opening/closing.


## v0.2.2.0 (2020-08-30)

* Explicitly import `MonadFail` and `fail`; hide the `fail` from `Prelude`.
* Introduces `timeInTimeZoneToUTC`, for when the timezone name is already available.
* Minor updates to help build with older Haskell versions.
* Update github actions to build on said older haskells!
* `TimeZoneName` is now an alias for `FilePath`.

## v0.2.1.0 (2020-08-30)

* Depend on `base >= 4.9` to ensure `MonadFail` and `liftIO` are included.

## v0.2.0.0 (2020-08-30)

* Introduces dependencies on `time`, `timezone-series` and `timezone-olson`.
* __Breaking change__: this library is now aware of `Data.Time`, `TimezoneName` has been changed
  to `TimeZoneName` for consistency, and the `Detect` module is now a submodule of `Data.Time.LocalTime.TimeZone`.
* The function to find a timezone name is now more general (instead of `Either`) expects an instance of `MonadFail`,
  like `parseTimeM` in `Data.Time` does, and is now named `lookupTimeZoneName` for clarity.
* Introduces `timeAtPointToUTC` to determine the UTC instant represented by a local time in a latitude
  and longitude: uses the timezone-series and timezone-olson packages to reflect any daylight savings
  or other historical circumstances that may affect the timezone offset for the timezone in effect
  around the given geographic point.


## v0.1.0.0 (2020-08-29)

* Bundles the C code for [ZoneDetect](https://github.com/BertoldVdb/ZoneDetect)
* Exposes the `lookupTimezone` function to obtain the standardized name of a timezone, given
  a database file, latitude and longitude.
