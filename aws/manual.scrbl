#lang scribble/manual

@(require (for-label racket)
          (for-label net/head)
          (for-label json)
          (for-label "cw.rkt")
          (for-label "exn.rkt")
          (for-label "keys.rkt")
          (for-label "s3.rkt")
          (for-label (rename-in "sdb.rkt" [delete-item sdb-delete-item]))
          (for-label "ses.rkt")
          (for-label "sns.rkt")
          (for-label "sqs.rkt")
          (for-label "r53.rkt")
          (for-label "dynamo.rkt")
          (for-label "util.rkt"))

@title{Amazon Web Services}

@; ----------------------------------------------------------------------------
@section{Introduction}

This libary provides support for many of the
@hyperlink["http://aws.amazon.com/documentation/" "Amazon Web Services"].

@subsection{Which services?}

The services supported are those most likely to be used both

@itemize[
@item{In an application.}
@item{Via an AWS interface.}
]

@italic{Not} supported are services like EC2 that are mainly about managing
the ``infrastructure'' for your application, such as creating servers on which
to run. The assumption is that you can use Amazon's command line tools or web
app console to do that. (If your application is @italic{about} managing
infrastructure, sorry.)

Also not supported is the ElastiCache service. Its application use interface
is the usual @tt{memcached} protocol. Amazon provides another interface for
managing the infrastructure of ElastiCache, not for using it.

Likewise RDS: Although Amazon lets you programatically create and manage
database servers, your application uses them in the usual way, for
example via Racket's @racket[db] library.

@subsection{Scope}

The goal is to provide enough ``wrapper'' around a service's HTTP interface to
make it convenient to use from Racket, but not obscure the service's
semantics.

The single most error-prone and time-consuming thing about working with AWS is
calculating an authentication signature. If you don't do it @italic{exactly}
right, the request will be rejected and the specific reason why is often not
apparent. Furthermore, the method can vary subtly among some services. You can
burn many hours on this, or you can use this library.

In addition there is help for the tedium of marshalling values into and out of
the HTTP requests and responses.

This library uses my @racket[http] library to make HTTP requests, instead of
@racket[net/url]. Why? To use HTTP 1.1 capabilities such as the @tt{Expect:
100-continue} request header (to fail @tt{PUT} requests quickly) and the
@tt{Range} request header (a sort of @racket[subbytes] for @tt{GET}s).

@; ----------------------------------------------------------------------------
@section{Names}

The names of procedures and structs do @italic{not} have special prefixes to
``group'' them.  Instead, use the @racket[prefix-in] option for
@racket[require] if you prefer a prefix (or need one to avoid a name
collision).

For example if you want the @racket[aws/sns] procedures to have an @racket[sns-]
prefix, so that @racket[create-topic] is renamed to @racket[sns-create-topic]:

@racketblock[
(require (prefix-in sns- aws/sns))
(sns-create-topic "foobar")
]

@; ----------------------------------------------------------------------------
@section{AWS Keys}

@defmodule[aws/keys]

@defparam[public-key key string?]{

Your AWS public key, a.k.a. ``Access ID.''}

@defparam[private-key key string?]{

Your AWS private key, a.k.a. ``Secret Key.''}


@defproc[(read-keys
[file path? (build-path(find-system-path 'home-dir) ".aws-keys")]
) void?]{

Set the parameters @racket[public-key] and @racket[private-key] by
reading their values from a plain text file. The file should consist of two
lines:

@verbatim{
AWSAccessKeyId=<key>
AWSSecretKey=<key>
}

By default this file is @tt{~/.aws-keys}. You probably want to @tt{chmod} the
permissions of this file carefully. }


@defproc[(ensure-have-keys) void?]{

If either @racket[public-key] or @racket[private-key] is @racket[""],
calls @racket[read-keys]. If either is @italic{still} blank, calls
@racket[error] with a hopefully helpful reminder about how to set the
parameters. }

@; ----------------------------------------------------------------------------
@section{Exception handling}

Most of the functions do not return a failure value. Instead they raise
@racket[exn:fail:aws], which you need to ``catch'' using
@racket[with-handlers].

@defmodule[aws/exn]

@defstruct[exn:fail:aws (
[http-code exact-positive-integer?]
[http-message string?]
[aws-code string?]
[aws-message string?])]{

Used by @racket[header&response->exn:fail:aws] and @racket[check-response].

}


@defproc[(header&response->exn:fail:aws
[headers string?]
[entity (or/c bytes? xexpr?)]
[ccm continuation-mark-set?])
exn:fail:aws?]{

Given an HTTP response's @racket[headers] and @racket[entity], return a
@racket[exn:fail:aws] constructed with information from the response.

}


@defproc[(check-response
[in input-port?]
[headers string?]
) (or/c string? (raise/c exn:fail:aws?))]{

Check @racket[headers]. If the status code is one of 200, 201, 202, 204, 206,
301, 302, or 307, simply return @racket[headers] (without reading any response
entity from @racket[in]).

Otherwise, read the XML response body from @racket[in] and use the information
to construct and raise @racket[exn:fail:aws].

Note: This does @italic{not} close the input port @racket[in] before raising an
exception. It assumes you are using @racket[call/requests],
@racket[call/input-request], or @racket[call/output-request] from the
@racket[http/request] library (or using @racket[dynamic-wind] or other
exception handling, or a custodian---or whatever) to make sure the port is
closed!

}

@; ----------------------------------------------------------------------------
@section{S3 (Storage)}

@defmodule[aws/s3]

@hyperlink["http://docs.amazonwebservices.com/AmazonS3/latest/dev/Welcome.html"
"S3"] provides a fairly simple and REST-ful interface. Uploading an object to
S3 is an HTTP @tt{PUT} request. Download an object is a @tt{GET} request. And
so on. As a result, you may feel you don't need a lot of ``wrapper'' around
this.

Where you definitely @italic{will} want help is constructing the
@tt{Authorization} header S3 uses to authenticate requests. Doing so requires
making a string out of specific elements of your request and ``signing'' it
with your AWS private key. Even a small discrepancy will cause the request to
fail authentication. As a result, @racket[aws/s3] makes it easy for you to
create the authentication header correctly and successfully.

Plus, @racket[aws/s3] does provide wrappers and tries to help with
some wrinkles. For example, S3 may give you a 302 redirect when you do
a @tt{PUT} or @tt{POST}. You don't want to transmit the entire entity,
only to have S3 ignore it and you have to transmit it all over
again. Instead, you want to supply the request header @tt{Expect:
100-continue}, which lets S3 respond @italic{before} you transmit the
entity.

@subsection{Endpoint}

@defparam[s3-host v string?]{

The hostname used for the S3 REST API. Defaults to @tt{"s3.amazonaws.com"}.

}

@defparam[s3-scheme v (or/c "http" "https")]{

The scheme used for the S3 REST API. Defaults to @tt{"http"}. Set to
@tt{"https"} to connect using SSL.

}


@subsection{Authentication signatures}

@defproc[(bucket&path->uri [bucket string?][path-to-resource string?]) string?]{

Given @racket[bucket] and @racket[path] (both of which should @italic{not}
start with a leading @racket["/"]), use @racket[s3-scheme] and @racket[s3-host]
to make the URI for the resource.

Example:
@racketblock[
> (bucket&path->uri "bucket" "path/to/file")
"http://s3.amazonaws.com/bucket/path/to/file"
]

}


@defproc[(bucket+path->bucket&path&uri [b+p string?])
(values string? string? string?)]{

Given a combined bucket+path string such as @racket["bucket/path/to/resource"],
return the bucket portion, path portion and URI.

Example:
@racketblock[
> (bucket+path->bucket&path&uri "bucket/path/to/file")
"bucket"
"path/to/file"
"http://s3.amazonaws.com/bucket/path/to/file"
]

}


@defproc[(uri&headers [b+p string?][method string?][headers dict?])
(values string? dict?)]{

Return the URI and headers for which to make an HTTP request to
S3. Constructs an @tt{Authorization} header based on the inputs.

}

@subsection{Conveniences}

@defproc[(create-bucket
[bucket-name string?]
) void?]{

Create a bucket named @racket[bucket-name].

Keep in mind that bucket names on S3 are global---shared among all users of
S3. You may want to make your bucket names include a domain name that you
own.

If you try to create a bucket with a name that is already used by
@italic{another} AWS account, you will get a @tt{409 Conflict} response.

If you create a bucket that already exists under your @italic{own} account,
this operation is idempotent (it's not an error, it's simply a no-op).

}


@defproc[(delete-bucket
[bucket-name string?]
) void?]{

Delete a bucket named @racket[bucket-name].

This operation is idempotent (it is a no-op to delete a bucket that has already
been deleted).

}


@defproc[(list-buckets) (listof string?)]{

List all the buckets belonging to your AWS account.

}


@defproc[(ls
[bucket+path string?]
) (listof string?)]{

List the names of objects whose names start with the pathname
@racket[bucket+path] (which is the form @racket["bucket/path/to/resource"]).
}


@defproc[(ll
[bucket+path string?]
) (listof (list/c string? string? xexpr?))]{

List objects whose names start with the path in @racket[bucket+path] (which is
the form @racket["bucket/path/to/resource"]):

@itemize[
@item{ name (as with @racket[ls]) }
@item{ response headers from a @tt{HEAD} request (as with @racket[head]) }
@item{ an @racket[xexpr] representing the ACL (as with @racket[get-acl]) }
]

}


@defproc[(head
[bucket+path string?]
) string?]{

Make a @tt{HEAD} request for @racket[bucket+path] (which is the form
@racket["bucket/path/to/resource"]) and return the headers as a @racket[string]
in @racket[net/head] format.

@racket[bucket+path] is the form @racket["bucket/path/to/resource"].

}


@defproc[(delete
[bucket+path string?]
) void?]{

Make a @tt{DELETE} request to delete @racket[bucket+path] (which is the form
@racket["bucket/path/to/resource"])

}


@defproc[(copy
[bucket+path/from string?]
[bucket+path/to string?]
) string?]{

@margin-note{Tip: To rename an object, @racket[copy] it then @racket[delete]
the original.}

Copy an existing S3 object @racket[bucket+path/from] to @racket[bucket+path/to],
including its metadata. Both names are of the form
@racket["bucket/path/to/resource"].

It is not an error to copy to an existing object (it will be replaced). It is
even OK to copy an existing object to itself.

}


@defproc[(get-acl
[bucket+path string?]
[heads dict? '()]
) xexpr?]{

Make a @tt{GET} request for the
@hyperlink["http://docs.amazonwebservices.com/AmazonS3/latest/dev/S3_ACLs_UsingACLs.html"
"ACL"] of the object @racket[bucket+path] (which is the form
@racket["bucket/path/to/resource"]).

S3 responds with an XML representation of the ACL, which is returned as an
@racket[xexpr?].

}


@defproc[(put-acl
[bucket+path string?]
[acl xexpr?]
) void]{

Make a @tt{PUT} request to set the
@hyperlink["http://docs.amazonwebservices.com/AmazonS3/latest/dev/S3_ACLs_UsingACLs.html"
"ACL"] of the object  @racket[bucket+path] to @racket[acl].

}


@defproc[(get
[bucket+path string?]
[reader (input-port? string? -> any/c)]
[heads dict? '()]
[range-begin (or/c #f exact-nonnegative-integer?) #f]
[range-end (or/c #f exact-nonnegative-integer?) #f]
) any/c]{

@margin-note{Although you may use @racket[get] directly, it is also a building
block for other procedures that you may find more convenient, such as
@racket[get/bytes] and @racket[get/file].}

Make a @tt{GET} request for @racket[bucket+path] (which is the form
@racket["bucket/path/to/resource"]).

The @racket[reader] procedure is called with an @racket[input-port?] and a
@racket[string?] respresenting the response headers. The @racket[reader] should
read the response entity from the port, being careful to read the exact number
of bytes as specified in the response header's @tt{Content-Length} field. The
return value of @racket[reader] is the return value of @racket[get].

You may pass request headers in the optional @racket[heads] argument.

The optional arguments @racket[range-begin] and @racket[range-end] are used to
supply an HTTP @tt{Range} request header. This header, which Amazon S3 supports,
enables a getting only a subset of the bytes.  Note that @racket[range-end] is
@italic{ex}clusive to be consistent with the Racket convention,
e.g. @racket[subbytes]. (The HTTP @tt{Range} header specifies the end as
@italic{in}clusive, so your @racket[range-end] argument is decremented to make
the value for the header.)

}


@defproc[(get/bytes
[bucket+path string?]
[heads dict? '()]
[range-begin (or/c #f exact-nonnegative-integer?) #f]
[range-end (or/c #f exact-nonnegative-integer?) #f]
) bytes?]{

Make a @tt{GET} request for @racket[bucket+path] (which is the form
@racket["bucket/path/to/resource"]) and return the response entity as
@racket[bytes?].

You may pass request headers in the optional @racket[heads] argument.

The optional arguments @racket[range-begin] and @racket[range-end] are used to
supply an optional @tt{Range} request header. This header, which Amazon S3
supports, enables a getting only a subset of the bytes.  Note that
@racket[range-end] is @italic{ex}clusive to be consistent with the Racket
convention, e.g. @racket[subbytes]. (The HTTP @tt{Range} header specifies the
end as @italic{in}clusive, so your @racket[range-end] argument is decremented
to make the value for the header.)

The response entity is held in memory; if it is very large and you want to
"stream" it instead, consider using @racket[get].

}


@defproc[(get/file
[bucket+path string?]
[pathname path-string?]
[heads dict? '()]
[#:mode mode-flag (or/c 'binary 'text) 'binary]
[#:exists exists-flag (or/c 'error 'append 'update 'replace 'truncate 'truncate/replace) 'error]
) void?]{

Make a @tt{GET} request for @racket[bucket+path] (which is the form
@racket["bucket/path/to/resource"]) and copy the the response entity directly to
the file specified by @racket[pathname]. The keyword arguments @racket[#:mode]
and @racket[#:exists] are identical to those for
@racket[call-with-output-file*].

You may pass request headers in the optional @racket[heads] argument.

}


@defproc[(put
[bucket+path string?]
[writer (output-port . -> . void?)]
[data-length (or/c #f exact-nonnegative-integer?)]
[mime-type string?]
[reader (input-port? string? . -> . any/c)]
[heads dict? '()]
) void?]{

@margin-note{Although you may use @racket[put] directly, it is also a building
block for other procedures that you may find more convenient, such as
@racket[put/bytes] and @racket[put/file].

To upload more than about 100 MB, see @racket[multipart-put].}

Makes a @tt{PUT} request for @racket[bucket+path] (which is the form
@racket["bucket/path/to/resource"]), using the @racket[writer] procedure to
write the request entity and the @racket[reader] procedure to read the
response entity.  Returns the response header (unless it raises
@racket[exn:fail:aws]).

The @racket[writer] procedure is given an @racket[output-port?] and a
@racket[string?] representing the response headers. It should write the
request entity to the port. The amount written should be exactly the same as
@racket[data-length], which is used to create a @tt{Content-Length} request
header. You must also supply @racket[mime-type] (for example
@racket["text/plain"]) which is used to create a @tt{Content-Type} request
header.

The @racket[reader] procedure is the same as for @racket[get]. The
response entity for a @tt{PUT} request usually isn't interesting, but you
should read it anyway.

Note: If you want a @tt{Content-MD5} request header, you must calculate and
supply it yourself in @racket[heads]. Supplying this allows S3 to verify the
upload integrity.

To use reduced redundancy storage, supply @racket[(hash 'x-amz-storage-class
"REDUCED_REDUNDANCY")] for @racket[heads].

}


@defproc[(put/bytes
[bucket+path string?]
[data bytes?]
[mime-type string?]
[heads dict? '()]
) void?]{

@margin-note{To upload more than about 100 MB, see @racket[multipart-put].}

Makes a @tt{PUT} request for @racket[bucket+path] (which is the form
@racket["bucket/path/to/resource"]), sending @racket[data] as the request entity
and creating a @tt{Content-Type} header from @racket[mime-type]. Returns the
response header (unless it raises @racket[exn:fail:aws]).

A @tt{Content-MD5} request header is automatically created from
@racket[data]. To ensure data integrity, S3 will reject the request if the
bytes it receives do not match the MD5 checksum.

To use reduced redundancy storage, supply @racket[(hash 'x-amz-storage-class
"REDUCED_REDUNDANCY")] for @racket[heads].

}


@defproc[(put/file
[bucket+path string?]
[pathname path-string?]
[#:mime-type mime-type (or/c #f string?) #f]
[#:mode mode-flag (or/c 'binary 'text) 'binary]
) void?]{

@margin-note{For files larger than about 100 MB, see
@racket[multipart-put/file].}

Upload the file @racket[pathname] to @racket[bucket+path].

Makes a @tt{PUT} request for @racket[bucket+path] (which is the form
@racket["bucket/path/to/resource"]) and copy the the request entity directly
from the file specified by @racket[pathname].  The @racket[#:mode-flag]
argument is identical to that for @racket[call-with-input-file*], which is
used.  Returns the response header (unless it raises @racket[exn:fail:aws]).

If @racket[#:mime-type] is @racket[#f], then the @tt{Content-Type} header is
guessed from the file extension, using a (very short!) list of common
extensions. If no match is found, then
@racket["application/x-unknown-content-type"] is used. You can customize the
MIME type guessing by setting the @racket[path->mime-proc] parameter to your
own procedure.

A @tt{Content-MD5} request header is automatically created from the contents of
the file represented by @racket[path]. To ensure data integrity, S3 will reject
the request if the bytes it receives do not match the MD5 checksum.

A @tt{Content-Disposition} request header is automatically created from
@racket[pathname]. For example if @racket[pathname] is
@racket["/foo/bar/test.txt"] or @racket["c:\\foo\\bar\\test.txt"] then the
header @racket["Content-Disposition:attachment; filename=\"test.txt\""] is
created.  This is helpful because a web browser that is given the URI for the
object will prompt the user to download it as a file.

To use reduced redundancy storage, supply @racket[(hash 'x-amz-storage-class
"REDUCED_REDUNDANCY")] for @racket[heads].

}


@defparam[path->mime-proc proc procedure?]{

A procedure which takes a @racket[path-string?] and returns a @racket[string?]
with a MIME type.

}


@subsection{Multipart uploads}

In addition to uploading an entire object in a single @tt{PUT} request, S3
lets you upload it in multiple 5 MB or larger chunks, using the
@hyperlink["http://docs.amazonwebservices.com/AmazonS3/2006-03-01/dev/UsingRESTAPImpUpload.html"
"multipart upload API"]. Amazon recommends using this when the total data to upload is bigger than about 100 MB.


@subsubsection{Convenience}


@defproc[(multipart-put
[bucket+path string?]
[num-parts exact-positive-integer?]
[get-part (exact-nonnegative-integer? . -> . bytes?)]
[mime-type string? "application/x-unknown-content-type"]
[heads dict? '()]
) string?]{

Upload @racket[num-parts] parts, where the data for each part is returned by
the @racket[get-part] procedure you supply. In other words, your
@racket[get-part] procedure is called @racket[num-parts] times, with values
@racket[(in-range num-parts)].

Each part must be at least 5 MB, except the last part.

The parts are uploaded using a small number of worker threads, to get some
parallelism and probably better performance.

}


@defproc[(multipart-put/file
[bucket+path string?]
[path path?]
[#:mime-type mime-type string? #f]
[#:mode mode-flag (or/c 'binary 'text) 'binary]
) string?]{

Like @racket[put/file] but uses multipart upload.

The parts are uploaded using a small number of worker threads, to get some
parallelism and probably better performance.

}


@subsubsection{Building blocks}

Use these if the data you're uploading is computed on the fly and you don't
know the total size in advance. Otherwise you may simply use
@racket[multipart-put] or @racket[multipart-put/file].


@defproc[(initiate-multipart-upload
[bucket+path string?]
[mime-type string?]
[heads dict?]
) string?]{

Initiate a multipart upload and return an upload ID.

}


@defproc[(upload-part
[bucket+path string?]
[upload-id string?]
[part-number (and/c exact-integer? (between/c 1 10000))]
[bstr bytes?]
) (cons/c (and/c exact-integer? (between/c 1 10000)) string?)]{

Upload one part for the multipart upload specified by the @racket[upload-id]
returned from @racket[initiate-multipart-upload].

Note that S3 part numbers start with @racket[1] (not @racket[0]).

@racket[bstr] must be at least 5 MB, unless it's the last part.

Returns a @racket[cons] of @racket[part-number] and the @tt{ETag} for the
part. You will need to supply a list of these, one for each part, to
@racket[complete-multipart-upload].

}


@defproc[(complete-multipart-upload
[bucket+path string?]
[upload-id string?]
[parts-list (listof (cons/c (and/c exact-integer? (between/c 1 10000)) string?))]
) xexpr?]{

Complete the multipart upload specified the by @racket[upload-id] returned
from @racket[initiate-multipart-upload], using a @racket[parts-list] of the
values returned from each @racket[upload-part]. The @racket[parts-list] does
not need to be in any particular order; it will be sorted for you.

Returns S3's XML response in the form of an @racket[xexpr?].

}


@defproc[(abort-multipart-upload
[bucket+path string?]
[upload-id string?]
) void?]{

Abort the multipart upload specified by the @racket[upload-id] returned from
@racket[initiate-multipart-upload].

}


@subsection{S3 examples}

@codeblock0{
(require aws/keys
         aws/s3)

(define (member? x xs)
  (not (not (member x xs))))

;; Make a random name for the bucket. Remember bucket names are a
;; global space shared by all AWS accounts. In a real-world app, if
;; you have a domain name, you probably want to include that as part
;; of your name.
(define test-bucket
  (for/fold ([s "test.bucket."])
      ([x (in-range 32)])
    (string-append s
                   (number->string (truncate (random 15)) 16))))

(ensure-have-keys)

(create-bucket test-bucket)
(member? test-bucket (list-buckets))

(define test-pathname "path/to/file")
(define b+p (string-append test-bucket "/" test-pathname))

(define data #"Hello, world.")
(put/bytes b+p data "text/plain")
(get/bytes b+p)
(get/bytes b+p '() 0 5)
(head b+p)

(ls (string-append test-bucket "/"))
(ls (string-append test-bucket "/" test-pathname))
(ls (string-append test-bucket "/" (substring test-pathname 0 2)))

(define p (build-path 'same
                      "tests"
                      "s3-test-file-to-get-and-put.txt"))
(put/file b+p p #:mime-type "text/plain")
(get/file b+p p #:exists 'replace)
(head b+p)
(member? test-pathname (ls b+p))

(define b+p/copy (string-append b+p "-copy"))
(copy b+p b+p/copy)
(ls (string-append test-bucket "/"))
(head b+p/copy)
(delete b+p/copy)

(delete b+p)
(delete-bucket test-bucket)
}

@; ----------------------------------------------------------------------------
@section{SDB (Database)}

@defmodule[aws/sdb]

@hyperlink["http://docs.amazonwebservices.com/AmazonSimpleDB/latest/DeveloperGuide/Welcome.html"
"SimpleDB"] is a ``schema-less'' database. You should review the SDB docs to
understand the basic concepts and names. For example an SDB @italic{domain} is
like a SQL table, an SDB @italic{item} is like a SQL row, and an SDB
@italic{item name} is like a SQL primary key value to unqiuely identify a row.

Instead of columns, each item has @italic{attributes}. Each attribute is a
key/value hash. In other words, unlike a SQL column which has one value, each
SDB attribute may have multiple values.  For example an attribute with the key
@racket["Sizes"] might have the values @racket["Small"], @racket["Medium"], and
@racket["Large"]. The values should be considered a strict set (just one
occurrence of each unique value).


@subsection{Making requests to SDB}

@defparam[sdb-endpoint v endpoint?]{

Set the endpoint for the service. Defaults to @racket[(endpoint
"sdb.amazonaws.com" #t)].

}


@defproc[(create-domain [name string?]) void?]{

Create an SDB domain, which is like a ``table'' in SQL. Remember that SDB
operations are idempotent; doing this more than once is equivalent to doing it
once.

Tip: Although you may put non alpha-numeric characters in a name, and most of
the API will work, the select procedures won't, unless you bracket the domain
name to make it legal SQL.

}


@defproc[(delete-domain [name string?]) void?]{

Delete an SDB domain. Remember that SDB operations are idempotent; doing this
more than once is equivalent to doing it once.

}


@defproc[(list-domains) (listof (list/c 'DomainName string?))]{

List all of the SDB domains associated with the AWS SDB account.

}


@defproc[(domain-metadata [name string?]) (listof (list/c symbol? string?))]{

Show metadata for a specific SDB domain.
}


@defparam[always-replace? always? boolean?]{

Set this parameter to @racket[#t] to make the @tt{Item.Replace} true for all
calls to @racket[put-attributes].  Else if at default @racket[#f] value,
@tt{Item.Replace} will be specified only if you do it for each attribute, using
@racket[(key val 'replace)] instead of @racket[(key val)].

}


@defproc[(put-attributes
[domain-name string?]
[item-name string?]
[attributes (listof (list/c symbol? string?))]
) any]{

Put the @racket[attributes] to @racket[item-name] in the
@racket[domain-name]. Remember that SDB operations are idempotent; doing this
more than once is equivalent to doing it once.

}


@defproc[(get-attributes
[domain-name string?]
[item-name string?]
) (listof (list/c symbol? string?))]{

Get the attributes for @racket[item-name] in @racket[domain-name]. Keep in mind
that SDB has ``eventual consistency''; it may take awhile for the result of
@racket[put-attributes] to be reflected in @racket[get-attributes].

}


@defproc[(delete-attributes
[domain-name string?]
[item-name string?]
[attributes (listof (list/c symbol? string?))]
) void?]{

Delete the @racket[attributes] for @racket[item-name] in
@racket[domain-name]. Remember that SDB operations are idempotent; doing this
more than once is equivalent to doing it once and is not an error.

}


@defproc[(delete-item
[domain-name string?]
[item-name string?])
void?]{

Delete @racket[item-name] from @racket[domain-name.]  Remember that SDB
operations are idempotent; doing this more than once is equivalent to doing it
once and is not an error.

}


@defproc[(select [expr string?]
) (listof (list/c symbol? string?))]{

Execute the SQL-ish @racket[expr]. See the SDB docs for the subset of SQL that
is supported.

}


@subsection{Batch}

@defproc[(batch-put-attributes
[domain-name string?]
[xs (listof (cons/c string? (listof (list/c symbol? string?))))]
) any]{

For efficiency, SDB provides this to put multiple attributes to multiple items
in one request.

}


@defproc[(batch-delete-attributes
[domain-name string?]
[xs (listof (cons/c string? (listof (list/c symbol? string?))))]
) void?]{

For efficiency, SDB provides this to delete put multiple attributes from
multiple items in one request.

}


@subsection{Hash/Set style}

@deftogether[(

@defproc[(put-attributes-hash
[domain string?][item string?]
[attribs (hash/c symbol? (set/c string?))]
) void?]

@defproc[(get-attributes-hash
[domain string?]
[item string?]
) (hash/c symbol? (set/c string?))]

@defproc[(select-hash
[expr string?]
) (listof item?)]

@defstruct[item (
[name string?]
[attribs (hash/c symbol? (set/c string?))]
)]

)]{

@racket[put-attributes] and @racket[get-attributes] are a low-level interface
that wraps SDB fairly thinly.  When you want exact control, use them.

These procedures provide a set-oriented interface. For a multi-valued attribute,
you get and set all its values together as one set.  The attribute's values are
represented as @racket[(set/c string?)]. A collection of attributes is
@racket[(hash/c symbol?  (set/c string?))]. When you get a multi-valued
attribute, all of its values are grouped and presented as a @racket[(set/c
string?)]. When you put the attribute set, all of its existing values in SDB
are replaced by the new set of values you supply. (At a lower level, this means
the first attribute is put to SDB using parameter @tt{Replace=true}---to clear
any/all existing values. The other values for the attribute are put with
@tt{Replace=false}---to preserve all of the multiple new values we are
setting.)

}


@subsection{Values as strings}

SDB stores all values as strings. You choose how a number is represented as a
string. Your choice matters for sorts and compares. The SDB docs recommend:

@itemize[
@item{ pad numbers with leading zeroes }
@item{ and apply an offset to negative numbers so they are stored as a nonnegative number }
]

Analgesic below.

@defproc[(int<->str
[width exact-positive-integer? 5]
[offset exact-nonnegative-integer? 0]
[pad-char character? #\0]
) (values (number? -> string?) (string? -> number))]{

This procedure creates a pair of procedures, to convert in each direction
between a @racket[number] and its padded/offset @racket[string] representation.

}


@deftogether[(

@defproc[(str->int/u8 [s string?]) number?]
@defproc[(int->str/u8 [n number?]) string?]
@defproc[(str->int/s8 [s string?]) number?]
@defproc[(int->str/s8 [n number?]) string?]

@defproc[(str->int/u16 [s string?]) number?]
@defproc[(int->str/u16 [n number?]) string?]
@defproc[(str->int/s16 [s string?]) number?]
@defproc[(int->str/s16 [n number?]) string?]

@defproc[(str->int/u32 [s string?]) number?]
@defproc[(int->str/u32 [n number?]) string?]
@defproc[(str->int/s32 [s string?]) number?]
@defproc[(int->str/s32 [n number?]) string?]

)]{

Converters created using @racket[int<->str] for signed and unsigned integers of
8, 16, and 32 bytes.

Examples:
@racketblock[
> (int->str/u8 0)
"000"
> (int->str/u8 255)
"255"
> (int->str/s8 -128)
"000"
> (int->str/s8 0)
"128"
> (int->str/s8 127)
"255"
> (int->str/u32 0)
"0000000000"
> (int->str/u32 (expt 2 32))
"4294967296"
> (int->str/s32 (- (expt 2 31)))
"0000000000"
> (int->str/s32 0)
"2147483648"
> (int->str/s32 (+ (expt 2 31)))
"4294967296"
]

}

@subsection{SDB Examples}

In the examples below, the reason for using @racket[sleep] is that SDB has an
``eventual consistency'' model. As a result, there may be a short delay before
the values we set are available to get.

@codeblock0{
(require aws/keys
         aws/sdb)

(ensure-have-keys)

(define test-domain "TestDomain")

(delete-domain test-domain)
(create-domain test-domain)
(sleep 1)
(member? `(DomainName ,test-domain) (list-domains))
(domain-metadata test-domain)

(define attribs '((BPM "130")
                  (Genre "Disco")))
(put-attributes test-domain "item" attribs)
(sleep 1)
(get-attributes test-domain "item")
(select (string-append "select Genre from " test-domain))
(delete-attributes test-domain "item" attribs)
(sleep 1)
(get-attributes test-domain "item")

(define cnt 5)
(for ([n (in-range cnt)])
    (put-attributes test-domain
                    (format "Item~a" n)
                    `((n ,(format "~a" n)))))
(for ([n (in-range cnt)])
    (displayln (get-attributes test-domain (format "Item~a" n))))

(select (string-append "SELECT Count(*) FROM " test-domain))
(select (string-append "SELECT * FROM " test-domain))

(for ([n (in-range cnt)])
    (delete-attributes test-domain
                       (format "Item~a" n)
                       `((n ,(format "~a" n)))))
(for ([n (in-range cnt)])
    (displayln (get-attributes test-domain (format "Item~a" n))))

;; BatchXxxAttributes
(define (batch-attribs n)
  (for/list ([i (in-range 6)])
      (list (string->symbol (format "key/~a/~a" n i))
            (format "val/~a/~a" n i))))
(define batch-item-count 5)
(define (batch-items)
  (for/list ([n (in-range batch-item-count)])
      (cons (format "item~a" n)
            (batch-attribs n))))
(batch-put-attributes test-domain (batch-items))
(sleep 3)
(for ([n (in-range batch-item-count)])
    (printf "item~a:\n" n)
    (displayln (get-attributes test-domain (format "item~a" n))))
(batch-delete-attributes test-domain (batch-items))
(sleep 3)
(for ([n (in-range batch-item-count)])
    (displayln (get-attributes test-domain (format "item~a" n))))

;; hash style
(define attribs/hash (hash 'bpm   (set "100")
                           'genre (set "Rock" "Metal")))
(put-attributes-hash test-domain "itemHash" attribs/hash)
(sleep 1)
(get-attributes-hash test-domain "itemHash")
(select-hash (format "SELECT * FROM ~a WHERE ItemName() = 'itemHash'"
                     test-domain))

(delete-domain test-domain)
}

@; ----------------------------------------------------------------------------
@section{SES (Email)}

@defmodule[aws/ses]

Please refer to the
@hyperlink["http://docs.amazonwebservices.com/ses/latest/DeveloperGuide/Welcome.html"
"SES documentation"] to understand concepts like a verified sending adddress.

@defparam[ses-endpoint v endpoint?]{

Set the endpoint for the service. Defaults to @racket[(endpoint
"email.us-east-1.amazonaws.com" #t)].

}


@defproc[(send-email
[#:from from string?]
[#:to to (listof string?)]
[#:subject subject string?]
[#:body body string?]
[#:cc cc (listof string?) '()]
[#:bcc bcc (listof string?) '()]
[#:reply-to reply-to (listof-string?) '()]
[#:return-path return-path string? from]
[#:html? html? boolean? #f]
[#:charset charset string? "UTF-8"]
)
void?]{

Send an email. Unless @racket[from] has been verified (for example using
@racket[verify-email-address] SES will fail.

If SES returns @tt{400 Bad Request} with
@tt{<Code>Throttling</Code><Message>Maximum sending rate exceeded.</Message>},
this repeatedly sleeps for a random 1-16 second interval then retries, until it
succeeds or SES fails with some other error.

}


@defproc[(send-raw-email [mail-from string?][rcpt-to string?][raw-message string?]) xexpr?]{

Send a raw email. SES requires a @tt{Source} to be specified. If a
@tt{Return-Path} mail header is supplied in @racket[raw-message] then that will
be used as the @tt{Source}, otherwise @racket[mail-from] will be used.

If SES returns @tt{400 Bad Request} with
@tt{<Code>Throttling</Code><Message>Maximum sending rate exceeded.</Message>},
this repeatedly sleeps for a random 1-16 second interval then retries, until it
succeeds or SES fails with some other error.

}


@defproc[(verify-email-address [address string?]) void?]{

Verify the email address with SES, so that it can be used to send emails via
SES.

}


@defproc[(delete-verified-email-address [address string?]) void?]{

Unverify the email address.

}


@defproc[(list-verified-email-addresses) (listof string?)]{

Return the list of email addresses currently verified with SES.

}


@deftogether[(

@defproc[(get-send-quota) send-quota?]

@defstruct[send-quota (
[sent-last-24-hours number?]
[max-24-hour-send number?]
[max-send-rate number?])]

)]{

Get the send quota.

}


@deftogether[(

@defproc[(get-send-statistics) (listof send-statistic?)]

@defstruct[send-statistic (
[time string?]
[delivery-attempts number?]
[rejects number?]
[bounces number?]
[complaints number?])]

)]{

Get send statistics. Although SES keeps statistics for only your last 14 days
of sending, each statistic is for a 15 minute bucket and the @racket[(listof
send-statistics)] may be quite long. Note that the list is not necessarily
sorted in any particular order.

}


@defproc[(ses [params (listof (list/c symbol? string?))]) xexpr?]{

The low-level procedure used by other procedures to make requests to SES.

If SES adds new actions and this library isn't updated to support them, you may
be able to support them by setting the @tt{Action} parameter.}


@; ----------------------------------------------------------------------------
@section{SNS (Notifications)}

@defmodule[aws/sns]


@hyperlink["http://docs.amazonwebservices.com/sns/latest/api/Welcome.html?r=9480"
"SNS"] lets you create topics to which notifications can be published. Each
topic has zero or more subscriptions.

Subscriptions can be of various types of endpoints, such as email, SMS, or an
HTTP @tt{POST} of a JSON-encoded message.

A new subscription must be confirmed by the recipient before it will receive
notifications, to prevent unwanted notifications.

Topics and subscriptions are unqiuely identified by a string referred to as an
ARN (Amazon Resource Name).  The string is composed of the service endpoint
hostname, your AWS account number, and a name. An example ARN is
@tt{arn:aws:sns:us-east-1:123456789012:My-Topic}.


@defparam[sns-endpoint v endpoint?]{

Set the endpoint for the service. Defaults to @racket[(endpoint
"sns.us-east-1.amazonaws.com" #f)].

}


@defproc[(create-topic [name string?]) string?]{

Create a topic and return its ARN.

}


@defproc[(delete-topic [arn string?]) void?]{

Delete a topic.

}


@defproc[(get-topic-attributes
[arn string?]
) (listof (cons/c symbol? string?))]{

Get the attributes for a topic as an association list.
}


@defproc[(list-topics) (listof string?)]{

Get the ARN for all SNS topics for the AWS account.

}


@deftogether[(
@defstruct[subscription ([protocol string?][owner string?][topic-arn string?][subscription-arn string?][endpoint string?])]
@defproc[(list-subscriptions) (listof subscription?)]
@defproc[(list-subscriptions-by-topic [arn string?]) (listof subscription?)]
)]{

Get subscriptions for all topic, or just those for a given topic.

}


@deftogether[(
@defproc[(sns-protocol? [s string?]) boolean?]
@defproc[(subscribe [endpoint string?][protocol sns-protocol?][topic-arn string?]) string?]
)]{

Subscribe to a topic, returning the ARN for the subscription. }


@defproc[(unsubscribe [subscription-arn string?]) void?]{

Delete a subscription.

}


@defproc[(publish
[topic-arn string?]
[message string?]
[#:subject subject ""]
[#:json? json? #f]
) void?]{

Publish a notification message to a topic. If @racket[#:json?] is @racket[#t]
then @racket[message] must be valid JSON or SNS will return an error.

}


@; ----------------------------------------------------------------------------
@section{SQS (Queues)}

@defmodule[aws/sqs]

@hyperlink["http://docs.amazonwebservices.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/Welcome.html" "SQS"] provides distributed queues.


@defparam[sqs-endpoint v endpoint?]{

Set the endpoint for the service. Defaults to @racket[(endpoint
"sqs.us-east-1.amazonaws.com" #f)].

}


@defproc[(create-queue [name string?]) string?]{

Create a queue and return its URI. The URI is used to identify the queue in
most of the other procedures. }


@defproc[(delete-queue [queue-uri string?]) void?]{

Delete a queue. }


@defproc[(list-queues) (listof string?)]{

List all the queues associated with the AWS account. }


@defproc[(get-queue-uri [name string?]) string?]{

Given the name of a queue, get its URI. }


@defproc[(send-message [queue-uri string][body string?][delay-seconds (or/c #f exact-nonnegative-integer?) #f]) void?]{

Send a message to a queue. See SQS docs for meaning of @racket[delay-seconds],
but, if not supplied the delay will default to that of the queue. }


@defproc[(get-queue-attributes [queue-uri string?]) (listof (list/c symbol? string?))]{

Get all the attributes for a queue. They are returned as a list instead of a
@racket[struct] because the list of attributes may grow in future versions of
SQS. }


@deftogether[(
@defstruct[message ([body string?][md5 string?][receipt-handle string?][attributes (listof (list/c symbol? string?))])]
@defproc[(receive-messages [queue-uri string?][max (and/c exact-integer? (between/c 1 10))][visibility-timeout (or/c #f exact-nonnegative-integer?) #f]) (listof message?)]
@defproc[(receive-message [queue-uri string?][visibility-timeout (or/c #f exact-nonnegative-integer?) #f]) (listof message?)]
)]

Get one or more messages from the queue.

@racket[receive-message] is simply sugar for @racket[receive-messages] with 1
supplied for @racket[max].

The @racket[receipt-handle] field of @racket[message] is used to identify the
message in procedures that operate on a specific message.

Note: The @racket[attributes] field of @racket[message] is the same format as
@racket[get-queue-attributes] and for the same reason.

@defproc[(delete-message [queue-uri string?][receipt-handle string?]) void?]{

Delete a message from a queue. }


@defproc[(change-message-visibility [queue-uri string?][receipt-handle string?][timeout exact-nonnegative-integer?]) void?]{

Change the visibility time of a message already in a queue. }


@; ----------------------------------------------------------------------------
@section{Route 53 (DNS)}

@defmodule[aws/r53]

@hyperlink["http://docs.amazonwebservices.com/Route53/latest/APIReference/Welcome.html" "Route 53"] provides DNS.


@defparam[r53-endpoint v endpoint?]{

Set the endpoint for the service. Defaults to @racket[(endpoint
"route53.amazonaws.com" #f)].

}


@defproc[(create-hosted-zone [name string?][unique string?][comment string? ""]) xexpr?]{

Create a hosted zone and return an @racket[xexpr?] respresenting the
response XML.

}


@defproc[(delete-hosted-zone [zone-id string?]) xexpr?]{

Delete a hosted zone and return an @racket[xexpr?] respresenting the
response XML.

Note that @racket[zone-id] is @italic{not} the domain name, it is the
"zone ID".

}


@defproc[(list-hosted-zones) xexpr?]{

List all the hosted zones associated with the AWS account.

}


@defproc[(get-hosted-zone [zone-id string?]) xexpr?]{

Given a @racket[zone-id], return information about the hosted zone.

}


@defproc[(domain-name->zone-id [name string?]) string?]{

Look up a zone ID from a domain name.

AWS requires the domain name to be in DNS style and end in a period,
such as @racket["foo.com."]. However if @racket[name] doesn't end in a
period, then @racket[domain-name->zone-id] automatically appends one
for you.

}


@defproc[(list-resource-record-sets
[zone-id string?]
[#:max-items max-items #f]
[#:name name #f]
[#:type type #f]
[#:id id #f]
) (listof xexpr?)]{

Return a list of @racket[ResourceRecordSet] @racket[xexpr?]s.

}


@defproc[(change-resource-record-sets [zone-id string?][changes xexpr?]) xexpr?]{

Make changes to the record sets for the the zone.

It's up to the caller to create an xexpr according to
@hyperlink["http://docs.amazonwebservices.com/Route53/latest/APIReference/API_ChangeResourceRecordSets.html"
"the AWS docs"].

Example:

@racketblock[
(change-resource-record-sets
 "/hostedzone/Z3K3IRK2M12WGD"
 `(ChangeResourceRecordSetsRequest
   ([xmlns "https://route53.amazonaws.com/doc/2012-02-29/"])
   (ChangeBatch
    (Comment "optional comment about the changes in this change batch request")
    (Changes (Change
              (Action "CREATE")
              (ResourceRecordSet (Name "foo2.com")
                                 (Type "A")
                                 (TTL "300")
                                 (ResourceRecords
                                  (ResourceRecord
                                   (Value "1.2.3.4")))))))))
]

}


@; ----------------------------------------------------------------------------
@section{Dynamo DB}

@defmodule[aws/dynamo]

@hyperlink["http://docs.amazonwebservices.com/amazondynamodb/latest/developerguide/Introduction.html" "Dynamo"] is Amazon's newer "NoSQL" service.


@defparam[dynamo-endpoint v endpoint?]{

Set the endpoint for the service. Defaults to @racket[(endpoint
"dynamodb.us-east-1.amazonaws.com" #f)].

}


@defparam[dynamo-region v string?]{

Set the region for the service. Defaults to @racket["us-east-1"].

}


@defthing[attribute-type/c (or/c "S" "N" "B")]{

A contract for Dynamo attribute types (string, number, base64 binary).

}


@defproc[(create-table
[name string?]
[read-units exact-positive-integer?]
[write-units exact-positive-integer?]
[hash-key-name string?]
[hash-key-type attribute-type/c]
[range-key-name string? #f]
[range-key-type attribute-type/c #f]
) jsexpr?]{

Create a table.

}


@defproc[(delete-table [name string?]) jsexpr?]{

Delete a table.

}


@defproc[(describe-table [name string?]) jsexpr?]{

Describe a table.

}


@defproc[(list-tables
[#:limit limit #f]
[#:from from #f]
) jsexpr?]{

List at most @racket[limit] tables, starting with the table name
@racket[from] (if continuing a listing that had previously stopped at
@racket[limit]).

}


@deftogether[(
@defproc[(put-item [js jsexpr?]) jsexpr?]
@defproc[(get-item [js jsexpr?]) jsexpr?]
@defproc[(delete-item [js jsexpr?]) jsexpr?]
@defproc[(update-item [js jsexpr?]) jsexpr?]
@defproc[(batch-get-item [js jsexpr?]) jsexpr?]
@defproc[(batch-write-item [js jsexpr?]) jsexpr?]
@defproc[(query [js jsexpr?]) jsexpr?]
@defproc[(scan [js jsexpr?]) jsexpr?]
@defproc[(update-table [js jsexpr?]) jsexpr?]
)]{

The remaining functions accept JSON which you must construct yourself
in the form of a @racket[jsexpr?]. The variation in the JSON is
sufficient that wrapping them in some arbitrary Racket structure
doesn't provide added value.  Instead, please see the Dynamo
documentation for these similarly-named functions.

@itemize[

@item{@hyperlink["http://docs.amazonwebservices.com/amazondynamodb/latest/developerguide/API_PutItem.html" "put-item"]}

@item{@hyperlink["http://docs.amazonwebservices.com/amazondynamodb/latest/developerguide/API_GetItem.html" "get-item"]}

@item{@hyperlink["http://docs.amazonwebservices.com/amazondynamodb/latest/developerguide/API_DeleteItem.html" "delete-item"]}

@item{@hyperlink["http://docs.amazonwebservices.com/amazondynamodb/latest/developerguide/API_UpdateItem.html" "update-item"]}

@item{@hyperlink["http://docs.amazonwebservices.com/amazondynamodb/latest/developerguide/API_BatchGetItems.html" "batch-get-item"]}

@item{@hyperlink["http://docs.amazonwebservices.com/amazondynamodb/latest/developerguide/API_BatchWriteItem.html" "batch-write-item"]}

@item{@hyperlink["http://docs.amazonwebservices.com/amazondynamodb/latest/developerguide/API_Query.html" "query"]}

@item{@hyperlink["http://docs.amazonwebservices.com/amazondynamodb/latest/developerguide/API_Scan.html" "scan"]}

@item{@hyperlink["http://docs.amazonwebservices.com/amazondynamodb/latest/developerguide/API_UpdateTable.html" "update-table"]}

]

}



@; ----------------------------------------------------------------------------
@section{CloudWatch (Monitoring)}

@defmodule[aws/cw]

Among the Amazon Web Services, the
@hyperlink["http://docs.amazonwebservices.com/AmazonCloudWatch/latest/DeveloperGuide/Welcome.html"
"CloudWatch"] API is the most quirky, least documented, and provides the fewest
specific examples of making requests.

As a result, there are more likely to be mistakes or problems in this
module. Your feedback or contributions to improve it are welcome.

Meanwhle, I've tried to focus mostly on @racket[put-metric-data], less on the
other metrics procedures, and least of all on alarms and alarm history. (This is
in line with the overall priority of this library, which is to support
applications not ``infrastucture.'')

@defparam[cw-endpoint v endpoint?]{

Set the endpoint for the service. Defaults to @racket[(endpoint
"monitoring.us-east-1.amazonaws.com" #t)].

}

@subsection{Contracts}

@defthing[unit/c
  (or/c 'None
        'Percent
        'Count
        'Seconds 'Microseconds 'Milliseconds
        'Bytes 'Kilobytes 'Megabytes 'Gigabytes 'Terabytes
        'Bits 'Kilobits 'Megabits 'Gigabits 'Terabits
        'Count/Second
        'Bytes/Second 'Kilobytes/Second 'Megabytes/Second
          'Gigabytes/Second 'Terabytes/Second
        'Bits/Second 'Kilobits/Second 'Megabits/Second
          'Gigabits/Second 'Terabits/Second
        )
]{

A contract for the @tt{Units} that CloudWatch accepts. }


@defthing[period/c
(make-flat-contract
 #:name 'non-zero-multiple-of-60
 #:first-order (lambda (x)
                 (and (>= x 60)
                      (zero? (modulo x 60)))))
]{

A contract for the @racket[period] argument to @racket[get-metric-statistcs]
and @racket[describe-alarms-for-metric]. CloudWatch requires the @tt{Period} to
be a non-zero multiple of 60 seconds. }


@defthing[statistic/c
  (or/c 'Sum 'Average 'Maximum 'Minimum 'SampleCount)
]{

A contract for the @tt{Statistic} values that CloudWatch knows about.}


@defthing[dimensions/c
 (listof (list/c symbol? string?))
]

@subsection{Putting metric data and getting statistics}

@defstruct[datum (
         [metric-name         string?]
         [value               (or/c #f number?)]
         [min                 (or/c #f number?)]
         [max                 (or/c #f number?)]
         [sum                 (or/c #f number?)]
         [sample-count        (or/c #f number?)]
         [unit                unit/c]
         [timestamp           exact-integer?]
         [dimensions          dimensions/c]
)]{

This struct is accepted by @racket[put-metric-data] and returned by
@racket[get-metric-statistics].

The @racket[timestamp] member is an @racket[exact-integer?], like
@racket[current-seconds], but all CloudWatch timestamps are UTC not local
time.

}


@defproc[(put-metric-data
[namespace string?]
[data (listof datum?)]
) void?]{

Put metric data to CloudWatch.

The @racket[value] member must not be @racket[#f].

The @racket[min], @racket[max], @racket[sum], and @racket[sample-count] members
may be @racket[#f] if you are putting individual values and will let CloudWatch
do the aggregation, or, they may be non-@racket[#f] if you are providing
CloudWatch data you have already aggregated.

}


@defproc[(get-metric-statistics
[#:metric-name metric-name string?]
[#:namespace namespace string?]
[#:statistics statistics (listof statistic/c)]
[#:unit unit unit/c]
[#:start-time start-time exact-integer?]
[#:end-time end-time exact-integer?]
[#:period period period/c 60]
[#:dimensions dimensions dimensions/c '()]
) (listof datum?)]{

Return statistics for a given metric @racket[metric-name] in
@racket[namespace]. The statistics are returned as a list of
@racket[datum] structs.

The @racket[value] member of @racket[datum] will always be @racket[#f]
because CloudWatch only returns aggregated data. Even if you put individual
values using @racket[put-metric-data], it will return only the aggregated
statistics.

Whether the @racket[min], @racket[max], @racket[sum], and @racket[sample-count]
members of @racket[datum] are @racket[#f], depends on whether you asked
those statistics to be returned by specifying them in @racket[statistics].  For
example if @racket[statistics] includes the symbol @racket['Sum], then the
@racket[sum] member will be non-@racket[#f], otherwise it will be @racket[#f].

}

@subsection{Listing metrics}

@defstruct[metric (
[name string?]
[namespace string?]
[dimensions dimensions/c]
)]


@defproc[(list-metrics
[#:metric-name metric-name (or/c #f string?) #f]
[#:namespace namespace (or/c #f string?) #f]
[#:dimensions dimensions dimensions/c '()]
) (listof metric?)]{

Return a list of @racket[metric?] meeting the criteria.

}

@subsection{Alarms}

@defstruct[alarm (
        [name string?]
        [description string?]
        [arn string?]
        [configuration-updated-timestamp exact-integer?]
        [metric-name string?]
        [namespace string?]
        [threshold number?]
        [comparison-operator string?]
        [alarm-actions xexpr?]
        [ok-actions xexpr?]
        [insufficient-data-actions xexpr?]
        [state-value string?]
        [state-reason string?]
        [state-reason-data string?]
        [state-updated-timestamp exact-integer?]
        [period period/c]
        [actions-enabled boolean?]
        [evaluation-periods exact-nonnegative-integer]
        [statistic string?]
        [dimensions dimensions/c])]{

A structure used by @racket[describe-alarms] and
@racket[describe-alarms-for-metric].

The @racket[configuration-updated-timestamp] and
@racket[state-updated-timestamp] members are @racket[exact-integer?] as with
@racket[current-seconds], but remember the CloudWatch times are UTC not local
time.

}


@defproc[(describe-alarms
[#:alarm-name-prefix alarm-name-prefix (or/c #f string?) #f]
[#:alarm-names alarm-names (listof string?) '()]
[#:state-value state-value (or/c #f 'OK 'ALARM 'INSUFFICIENT_DATA) #f]
) (listof metric-alarm?)]{

Return the alarms meeting the criteria.

}


@defproc[(describe-alarms-for-metric
[#:metric-name metric-name string?]
[#:namespace namespace string?]
[#:statistic statistic (or/c #f statistic/c) #f]
[#:unit unit (or/c #f unit/c) #f]
[#:period period (or/c #f period/c) #f]
[#:dimensions dimensions dimensions/c '()]
) (listof metric-alarm?)]{

Return the alarms meeting the criteria.

}


@defstruct[alarm-history (
[timestamp exact-integer?]
[item-type symbol?]
[name string?]
[data xexpr?]
[summary string?]
)]{

The @racket[data] is JSON, which you will get as an @racket[xexpr?] and need to
parse yourself.

The @racket[timestamp] member is an @racket[exact-integer?], like
@racket[current-seconds], but all CloudWatch timestamps are UTC not local
time.

}


@defproc[(describe-alarm-history
[#:alarm-name alarm-name (or/c #f string?) #f]
[#:start-date start-date (or/c #f exact-integer?) #f]
[#:end-date end-date (or/c #f exact-integer?) #f]
[#:history-type history-type (or/c #f 'ConfigurationUpdate 'StateUpdate 'Action) #f]
) (listof alarm-history?)]{

Return the history for alarms meeting the criteria.

}

@; ----------------------------------------------------------------------------
@section{Glacier (Archives)}

@defmodule[aws/glacier]

@hyperlink["http://docs.amazonwebservices.com/amazonglacier/latest/dev/introduction.html"
"Glacier"] provides storage for archiving. You can store objects less
expensively than using S3. The trade-off is that it is very slow to retreive
them.

@subsection{Region}

@defparam[region v string?]{

Set the region. Defaults to @racket["us-east-1"].

}

@subsection{Vaults}

@defproc[(create-vault
[name string?]
) (or/c #t exn:fail:aws?)]{

@hyperlink["http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-vault-put.html"
"Create a vault" #:underline? #f] and return @racket[#t] or raise
@racket[exn:aws:fail?]. Idempotent.

}

@defproc[(delete-vault
[name string?]
) (or/c #t exn:fail:aws?)]{

@hyperlink["http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-vault-delete.html" "Delete a vault" #:underline? #f] and return @racket[#t] or raise
@racket[exn:aws:fail?]. Idempotent.

}

@defproc[(list-vaults
) jsexpr?]{

@hyperlink["http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-vaults-get.html" "List vaults" #:underline? #f] in a @racket[jsexpr?].

@codeblock{
> (list-vaults)
'(#hasheq((VaultName . "testvault")
          (CreationDate . "2012-08-30T12:29:37.200Z")
          (LastInventoryDate . null)
          (NumberOfArchives . 0)
          (SizeInBytes . 0)
          (VaultARN . "arn:aws:glacier:us-east-1:203585791165:vaults/testvault")))
}

}

@defproc[(describe-vault
[name string?]
) jsexpr?]{

@hyperlink["http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-vault-get.html" "Describe a vault" #:underline? #f] in a @racket[jsexpr?].

}

@subsection{Vault notifications}

@defproc[(set-vault-notifications
[name string?]
[sns-topic string?]
[inventory? boolean?]
[archive? boolean?]
) (or/c #t exn:fail:aws?)]{

@hyperlink["http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-vault-notifications-put.html" "Set a vault's notification configuration" #:underline? #f].

}

@defproc[(get-vault-notifications
[name string?]
) jsexpr?]{

@hyperlink["http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-vault-notifications-get.html" "Get a vault's notification configuration" #:underline? #f].

}

@defproc[(delete-vault-notifications
[name string?]
) void?]{

@hyperlink["http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-vault-notifications-delete.html"
"Delete a vault's notification configuration" #:underline? #f].

}

@subsection{Archives}

@defproc[(create-archive
[vault-name string?]
[archive-description string?]
[data bytes?]
) string?]{

@hyperlink["http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-archive-post.html"
"Create an archive" #:underline? #f] containing the @racket[data] and return
its archive ID.

}


@defproc[(create-archive-from-file
[vault-name string?]
[path path?]
) string?]{

@hyperlink["http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-archive-post.html"
"Create an archive" #:underline? #f] with data from a file and return its
archive ID.

}

@defproc[(delete-archive
[vault-name string?]
[archive-id string?]
) (or/c #t exn:fail:aws?)]{

Delete an archive.

}

@subsection{Retrieval jobs}

@defproc[(retrieve-inventory
[vault-name string?]
[job-description string?]
[sns-topic (or/c string? #f) #f]
) string?]{

@hyperlink["http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-initiate-job-post.html" "Initiate a job" #:underline? #f] to retrieve an archive's inventory, and return the job ID.

}

@defproc[(retrieve-archive
[vault-name string?]
[job-description string?]
[sns-topic (or/c string? #f) #f]
) string?]{

@hyperlink["http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-initiate-job-post.html" "Initiate a job" #:underline? #f] to retrieve an archive's data, and return the job ID.

}


@defproc[(list-jobs
[vault-name string?]
) jsexpr?]{

@hyperlink["http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-jobs-get.html" "List jobs" #:underline? #f].

}

@defproc[(get-job-output
[vault-name string?]
[job-id string?]
) (or/c jsexpr? bytes?)]{

@hyperlink["http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-job-output-get.html"
"Get the output of a job" #:underline? #f]. If the @tt{Content-Type} of the
response is @tt{application/json}, return the result as a @racket[jsexpr?],
otherwise return it as @racket[bytes?].

}

@defproc[(get-output-job-to-file
[vault-name string?]
[job-id string?]
[path path?]
[exists (or/c 'error 'append 'update 'replace 'truncate 'truncate/replace)]
) boolean?]{

@hyperlink["http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-job-output-get.html"
"Get the output of an archive retrieval job" #:underline? #f] and put it in a
file. Return a @racket[boolean?] whether the output matches its
@tt{x-amz-sha256-tree-hash}.

}

@subsection{Example: Backup using Glacier and SDB}

This example can be found in @tt{examples/backup.rkt}.

@codeblock0{
#lang racket

;; Use Glacier for archival backups, and SDB to store the metadata.

(require aws/sdb
         aws/sns
         aws/glacier
         http/request)      ;just for seconds->gmt-8601-string

(define path->archive-domain "examplesBackupPathToArchive")
(define archive->meta-domain "examplesBackupArchiveToMeta")
(define vault "examples.backup")

(define (ensure-assets)
  ;; Creating a vault on Glacier is idempotent; harmless to do again.
  (create-vault vault)
  ;; Creating a domain on SDB is idempotent; harmless to do again.
  (create-domain path->archive-domain)
  (create-domain archive->meta-domain))

(define/contract (archive-file path)
  (path? . -> . void?)
  (define path/string (path->string path))
  ;; Upload to Glacier.
  (printf "~a\nUploading to Amazon Glacier ...\n" path/string)
  (define archive-id (create-archive-from-file vault path))
  ;; Store some metadata on SDB.
  ;;
  ;; Using the path for SDB's ItemName, store an attribute named
  ;; ArchiveId with the Glacier archive ID as the value. Remember that
  ;; SDB allows multiple values per attribute, so setting this more
  ;; than once will add more values rather than replace.
  (printf "Updating Amazon Simple Database with metadata ...\n")
  (put-attributes path->archive-domain
                  path/string
                  `([ArchiveId ,archive-id]))
  ;; Also store some info about this specific archive.
  (put-attributes archive->meta-domain
                  archive-id
                  `([Size ,(number->string (file-size path))]
                    [Date ,(seconds->gmt-8601-string)]
                    [Path ,path/string]))
  (void))

(define/contract (archive-directory path [sns-topic #f])
  ((path-string?) (string?) . ->* . void?)
  (printf "Ensuring Amazon SDB and Glacier resources are created ...\n")
  (ensure-assets)
  (printf "Starting archive of all files under ~a ...\n" path)
  (for ([x (in-directory path)])
      ;; Unless a directory or a dot file
      (unless (or (directory-exists? x)
                  (equal? #\. (string-ref (path->string x) 0)))
        (archive-file x)))
  (when sns-topic
    (publish sns-topic (format "Archive completed ~a." (seconds->gmt-string))))
  (void))

;; For example let's archive the file in our tests dir.
(define root-dir
  (path->string (simplify-path (path->complete-path (build-path 'up "tests")))))
;; Let's notify to our first SNS topic (if any)
(define sns-topic (match (list-topics) [(list x rest ...) x][else #f]))
(archive-directory root-dir sns-topic)

;; Let's look at the information from SDB
(select-hash (format "SELECT * FROM ~a" path->archive-domain))
(select-hash (format "SELECT * FROM ~a" archive->meta-domain))
}


@; ----------------------------------------------------------------------------
@section{Utilities}


@defmodule[aws/util]

Although the following are mainly for internal use, they're @racket[provide]d
in case you find them helpful.

@defstruct[endpoint ([host string?][ssl? boolean?])]{

Used to represent an AWS service endpoint.

}


@defproc[(dict->form-urlencoded [dictionary dict?]) string?]{

Like @racket[alist->form-urlencoded], but @racket[dictionary] is a Racket
@racket[dict], which includes but is not limited to an association list.

Also, and more importantly, the ``percent encoding'' is done using RFC 3986,
which encodes some extra characters compared to RFC 2396.  Doing so is
important especially for SDB and its @tt{Select} action: The SQL-like statement
contains characters like @racket[#\*], @racket[#\(], and @racket[#\)], which
SDB requires to be percent-encoded.

}


@defproc[(tags
[xexpr xexpr?]
[tag symbol?]
[direct-child-of (or/c #f symbol?) #f]
) (listof xexpr?)]{

Given @racket[xexpr] return a list of all the elements starting with
@racket[tag]. If @racket[direct-child-of] is @racket[#f], it will return
elements at any depth. It will even return nested elements multiple times --
once on their own, and once within their parent.  If you only want elements
that have a specific immediate parent, set @racket[direct-child-of] to that
symbol.

Although there are more sophisticated and correct ways to make sense of XML,
this is useful when the XML structure is small and predictable, and you care
about extracting a few specific elements.

}


@defproc[(first-tag-value
[xexpr xexpr?]
[tag symbol?]
[def any/c #f]
) string?]{

Given @racket[xexpr], return the value of just the first element having tag
@racket[tag], or if none found, @racket[def].

}


@; ----------------------------------------------------------------------------
@section{Unit tests}

The @racket[rackunit] tests use the @racket[test] submodule feature added in
Racket 5.3. To run all tests, use the shell command, @tt{raco test ./}.

Be aware that some of the tests make actual requests to AWS. They may take
awhile to complete. And to do so, the tests need some personally identifying
information from you, such email addresses, or names for certain AWS objects
that are safe to use for testing.

For example the tests for the @racket[aws/s3] module try to @tt{PUT} then
@tt{GET} an object to S3---but what bucket and object path name should the
tests use? Only you can say. Similarly the @racket[aws/ses] tests try to send
actual emails, and you need to supply suitable email addresses (including an
email address that is intentionally not ``verified'' with SES, to test its
response to that condition).

To supply this information, provide a @tt{~/.aws-tests-data} file containing
various personally identifying information required to run the tests.

Following is an example file, which you may also find in
@tt{tests/example-dot-aws-tests-data}. In this example, suggested default
values are provided, whereas others are blank. You should supply or review all
of them to make sure they are suitable for you.

@verbatim{
# Supply a file in this format as ~/.aws-tests-data.
# You should change the values to those suitable for your AWS account
# and email addresses.

# Name of a test bucket on S3. Do NOT use leading /
test/bucket = test-bucket

# Name of a test resource to put/get/delete on S3. Do NOT use leading /
test/path = path/to/test.txt

# Email address you HAVE verified with SES.
test/verified-sender = <email-address>

# Email address you have NOT verified with SES.
test/unverified-sender = <email-address>

# Email recipient of som test emails
test/recipient = <email-address>

# Name of a test domain (i.e. "table") on SDB.
test/domain = TestDomain
}

@; ----------------------------------------------------------------------------
@section{License}

Copyright (c) 2012, Greg Hendershott.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

@itemize[

@item{ Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer. }

@item{ Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution. }

] @;itemize

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

@; ----------------------------------------------------------------------------
