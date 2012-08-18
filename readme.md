Documentation
=============

Documentation is written using Racket's Scribble. The raw source is in
manual.scrbl.

The HTML output that you'd actually want to read can be found on
[PLaneT](http://planet.plt-scheme.org/display.ss?package=aws.plt&owner=gh).

You can also generate it using `./make-doc.sh`, then point your browser to
[file:///tmp/aws-doc/manual.html](file:///tmp/aws-doc/manual.html).


Requirements
============

* The access keys for an Amazon Web Services account.

* This library requires my `http` library available on
  [Github](https://github.com/greghendershott/http) or
  [PLaneT](http://planet.plt-scheme.org/display.ss?package=http.plt&owner=gh). You
  should not need to install this manually. Racket's `(require (planet ...))`
  should automatically install this the first time.

Unit tests
==========

* The tests require you to specify certain personal information. See
  `tests/example-dot-aws-tests-data` for more information.

* The `rackunit` tests use the submodule feature new in Racket 5.3. Tests are
  in `(module+ test ..)`. You can run the tests for a `foo.rkt` file with
  `raco test foo.rkt`. You can run tests for all files using `raco test ./`.
