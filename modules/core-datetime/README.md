# Core Datetime
GnuCOBOL 3.x core library with datetime functions

## Function Summary

| Name | Description |
| ----------- | ----------- | 
| [datetime-format](#datetime-format) | Format the given or current timestamp, replacing the tokens, such as YY Year 18 YYYY Year 2018 M Month of the year (1-12) 7 MM Month of the year (01-12) 07 MMM Month of the year textual Jul D Day of the month (1-31) 9 DD Day of the month (01-31) 09 DDD Day of the year (01-366) 07 WW Week of the year (01-53) 05 U Weekday (1-7) 2 EEE Weekday textual Tue h Hour of the day (0-23) 5 hh Hour of the day (00-23) 05 m Minute of the hour (0-59) 9 mm Minute of the hour (00-59) 09 s Second of the minute (0-59) 4 ss Second of the minute (00-59) 04 z Timezone GMT-08:00 x Timezone ISO 8601 -08:00 | 

## Function Details

### datetime-format

*datetime-format(l-format, l-timestamp)*

Format the given or current timestamp, replacing the tokens, such as YY Year 18 YYYY Year 2018 M Month of the year (1-12) 7 MM Month of the year (01-12) 07 MMM Month of the year textual Jul D Day of the month (1-31) 9 DD Day of the month (01-31) 09 DDD Day of the year (01-366) 07 WW Week of the year (01-53) 05 U Weekday (1-7) 2 EEE Weekday textual Tue h Hour of the day (0-23) 5 hh Hour of the day (00-23) 05 m Minute of the hour (0-59) 9 mm Minute of the hour (00-59) 09 s Second of the minute (0-59) 4 ss Second of the minute (00-59) 04 z Timezone GMT-08:00 x Timezone ISO 8601 -08:00

#### Parameters

> **l-format** 32-char long string 
> **l-timestamp** 21-char long current-date or ZERO 

#### Returns

> Formatted timestamp trailing by spaces, 32-char long

# Usage
Install and initialize [COBOL Package Manager](https://cobolget.com):
```
$ npm install -g cobolget
$ cobolget init
```
Add the package to the `Manifest`:
```
$ cobolget add core-datetime
$ cobolget update
```
Install the package and its dependencies:
```
$ cobolget install
....
Modules modules.cpy and modules.cbl updated.
```
Directory `modules` contains complete COBOL source-code and `modules.cpy` Copybook ready for inclusion into your project.
