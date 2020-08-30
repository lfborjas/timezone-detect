# Changelog for timezone-detect

## v0.2.1.0 (2020-08-30)

* Depend on `base >= 4.9` to ensure `MonadFail` and `lifIO` are included.

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
