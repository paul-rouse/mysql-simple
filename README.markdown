# mysql-simple: mid-level bindings to the mysqlclient library

This library is a mid-level Haskell binding to the MySQL `mysqlclient`
client library.  It is aimed at speed and ease of use.

# Licensing

This library is BSD-licensed under the terms of the
[MySQL FOSS License Exception](http://www.mysql.com/about/legal/licensing/foss-exception/).

Since this library links against the GPL-licensed `mysqlclient`
library, a non-open-source application that uses it *may* be subject
to the terms of the GPL.

# To do

* Add support for prepared statements. This API is huge and of dubious
  performance worth, so it's not currently a priority for me. Patches
  welcome!

# Get involved!

We are happy to receive bug reports, fixes, documentation enhancements,
and other improvements.

Please report bugs via the
[github issue tracker](http://github.com/paul-rouse/mysql-simple/issues).

Master [git repository](http://github.com/paul-rouse/mysql-simple):

* `git clone git://github.com/paul-rouse/mysql-simple.git`

# Authors

This library was written by Bryan O'Sullivan, <bos@serpentine.com>, and
is now being maintained by Paul Rouse <pyr@doynton.org>.
