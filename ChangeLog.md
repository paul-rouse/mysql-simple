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
