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

* [Glacier archiving](http://docs.amazonwebservices.com/amazonglacier/latest/dev/introduction.html).

* [Authorization signature version 4](http://docs.amazonwebservices.com/general/latest/gr/signature-version-4.html).

* [Route 53](http://docs.amazonwebservices.com/Route53/latest/APIReference/Welcome.html).

* [Dynamo](http://docs.amazonwebservices.com/amazondynamodb/latest/developerguide/Introduction.html).


Requirements
============

* Racket 6.0.1 or newer.

* The access keys for an Amazon Web Services account. (If you want to
  experiment but are concerned about cost, keep in mind that AWS has a
  free usage tier.)

* My [http] and [sha] packages. These are listed as dependencies and
  will be installed uatomatically when you `raco pkg install aws`.

[http]: https://github.com/greghendershott/http
[sha]: https://github.com/greghendershott/sha


Documentation
=============

* [Documentation](http://pkg-build.racket-lang.org/doc/rackjure@rackjure/index.html).


Unit tests
==========

To simply use the library you don't need to run the unit tests. But if you
want to run them:

* The tests require you to specify certain personal information in a dot
  file. See
  [example-dot-aws-tests-data](https://github.com/greghendershott/aws/blob/master/tests/example-dot-aws-tests-data)
  for more information.

* The `rackunit` tests use the submodule feature added in Racket 5.3. Tests are
  inside `(module+ test ...)` forms.

  * You can run the tests for one `foo.rkt` file with `raco test foo.rkt`.

  * You can run tests for all files using `raco test -x .`.  (The `-x` flag is
    important to avoid evaluating rkt files that have no `test` module
    whatsoever.)

* Be aware that the tests are extensive and will do significant data
  transfer with Amazon AWS. Although this shouldn't cost a _lot_ of
  money, it will take some time for them to complete.
