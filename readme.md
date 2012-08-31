Overview
========

Provides [Racket](http://www.racket-lang.org) support for many [Amazon
Web Services](http://aws.amazon.com/documentation/):

* [S3 storage](http://docs.amazonwebservices.com/AmazonS3/latest/dev/Welcome.html).

* [SimpleDB database](http://docs.amazonwebservices.com/AmazonSimpleDB/latest/DeveloperGuide/Welcome.html).

* [SES email](http://docs.amazonwebservices.com/ses/latest/DeveloperGuide/Welcome.html).

* [SNS notification](http://docs.amazonwebservices.com/sns/latest/api/Welcome.html?r=9480).

* [SQS queues](http://docs.amazonwebservices.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/Welcome.html).

* [CloudWatch monitoring](http://docs.amazonwebservices.com/AmazonCloudWatch/latest/DeveloperGuide/Welcome.html).

* [Glacier
  archiving](http://docs.amazonwebservices.com/amazonglacier/latest/dev/introduction.html). _New in v1.3, 2012-08-30_

* [Authorization signature version 4](http://docs.amazonwebservices.com/general/latest/gr/signature-version-4.html).  _New in v1.3, 2012-08-3_


Documentation
=============

Documentation is written using Racket's Scribble. The raw source is in
[manual.scrbl](https://github.com/greghendershott/aws/blob/master/manual.scrbl).

The HTML output that you'd actually want to read can be found on
[PLaneT](http://planet.plt-scheme.org/package-source/gh/aws.plt/1/2/planet-docs/manual/index.html).

You can also generate it using `./make-doc.sh`, then point your browser to
[file:///tmp/aws-doc/manual.html](file:///tmp/aws-doc/manual.html).


Requirements
============

* The access keys for an Amazon Web Services account. (If you want to experiment
  but are concerned about cost, keep in mind that AWS has a "free tier" for
  certain usage.)

* My `http` library available on
  [Github](https://github.com/greghendershott/http) or
  [PLaneT](http://planet.plt-scheme.org/display.ss?package=http.plt&owner=gh). 
  You don't have to install this mannually as Racket's `(require (planet ...))`
  should automatically install it.

* My `sha` library available on
  [Github](https://github.com/greghendershott/sha) or
  [PLaneT](http://planet.plt-scheme.org/display.ss?package=sha.plt&owner=gh). 
  You don't have to install this mannually as Racket's `(require (planet ...))`
  should automatically install it.

Unit tests
==========

* The tests require you to specify certain personal information. See
  `tests/example-dot-aws-tests-data` for more information.

* The `rackunit` tests use the submodule feature added in Racket 5.3. Tests are
  in `(module+ test ...)`.

  * You can run the tests for a `foo.rkt` file with `raco test foo.rkt`.

  * You can run tests for all files using `raco test -x ./`.  (The `-x` flag is
    important to avoid evaluating rkt files that have no `test` module
    whatsoever.)
