## 0.4.7.2

* Fix question marks in string literals causing substitution errors (https://github.com/paul-rouse/mysql-simple/pull/54)

## 0.4.7.1

* Use parseTimeM from Data.Time.Format instead of parseTime (https://github.com/paul-rouse/mysql-simple/pull/53)

## 0.4.7

*  Allow JSON fields to be read in as ByteStrings (https://github.com/paul-rouse/mysql-simple/pull/52)

## 0.4.6

* Support GHC-9 via blaze-textual shim (https://github.com/paul-rouse/mysql-simple/pull/51).

## 0.4.5

* Add Semigroup instance for Query
* Add a `(Param a) => Param (In (Set a))` instance.
* Drop testing under GHC 7.8 / lts-2

## 0.4.4

* Report table name and database in the UnexpectedNull case of ResultError, using the previously empty errMessage field

## 0.4.3

* Use Data.Time.Format to parse TimeOfDay results properly (missing in https://github.com/paul-rouse/mysql-simple/pull/37).

## 0.4.2.0

* Handle fractional seconds in parameter substitution (without affecting cases where only whole seconds are used).  https://github.com/paul-rouse/mysql-simple/pull/40

## 0.4.1.0

* Add missing tuple instances of QueryResults (https://github.com/paul-rouse/mysql-simple/issues/9)
* Fix `error "foo"` left in code (https://github.com/paul-rouse/mysql-simple/issues/39)
* Allow fractional seconds in to be parsed in date/time results https://github.com/paul-rouse/mysql-simple/pull/37

## 0.4.0.1

* Fix https://github.com/paul-rouse/mysql-simple/issues/35

## 0.4.0.0

* Add type VaArgs for non-parenthesized variable-length list of parameters.
* Include fieldName in other constructors of ResultError.

## 0.3.0.0

* New maintainer and GitHub location - with many thanks to Bryan O'Sullivan for all of the past work on this module, and for facilitating the transfer of maintenance responsibility.
* Extra instances of QueryParams for larger tuples.
* Include fieldName in ResultError in the case of the Incompatible constructor.

The changes to ResultError and QueryParams were previously unreleased,
although present in the GitHub repository.  They are the reason for the
major version bump.
