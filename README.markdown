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
[github issue tracker](http://github.com/mailrank/mysql-simple/issues).

Master [git repository](http://github.com/mailrank/mysql-simple):

* `git clone git://github.com/mailrank/mysql-simple.git`

There's also a [Mercurial mirror](http://bitbucket.org/bos/mysql-simple):

* `hg clone http://bitbucket.org/bos/mysql-simple`

(You can create and contribute changes using either git or Mercurial.)

# Authors

This library is written and maintained by Bryan O'Sullivan,
<bos@mailrank.com>.
