Amazon Web Services

# 1. Introduction

This libary provides support for many of the [Amazon Web
Services](http://aws.amazon.com/documentation/).

## 1.1. Which services?

The services supported are those most likely to be used both

* In an application.

* Via an AWS interface.

_Not_ supported are services like EC2 that are mainly about managing the
“infrastructure” for your application, such as creating servers on which
to run. The assumption is that you can use Amazon’s command line tools
or web app console to do that. (If your application is _about_ managing
infrastructure, sorry.)

Also not supported is the ElastiCache service. Its application use
interface is the usual `memcached` protocol. Amazon provides another
interface for managing the infrastructure of ElastiCache, not for using
it.

Likewise RDS: Although Amazon lets you programatically create and manage
database servers, your application uses them in the usual way, for
example via Racket’s `db` library.

## 1.2. Scope

The goal is to provide enough “wrapper” around a service’s HTTP
interface to make it convenient to use from Racket, but not obscure the
service’s semantics.

The single most error-prone and time-consuming thing about working with
AWS is calculating an authentication signature. If you don’t do it
_exactly_ right, the request will be rejected and the specific reason
why is often not apparent. Furthermore, the method can vary subtly among
some services. You can burn many hours on this, or you can use this
library.

In addition there is help for the tedium of marshalling values into and
out of the HTTP requests and responses.

This library uses my `http` library to make HTTP requests, instead of
`net/url`. Why? To use HTTP 1.1 capabilities such as the `Expect:
100-continue` request header (to fail `PUT` requests quickly) and the
`Range` request header (a sort of `subbytes` for `GET`s).

# 2. Names

The names of procedures and structs do _not_ have special prefixes to
“group” them.  Instead, use the `prefix-in` option for `require` if you
prefer a prefix (or need one to avoid a name collision).

For example if you want the `aws/sns` procedures to have an `sns-`
prefix, so that `create-topic` is renamed to `sns-create-topic`:

```racket
(require (prefix-in sns- aws/sns))
(sns-create-topic "foobar")       
```

# 3. AWS Keys

```racket
 (require aws/keys) package: aws
```

```racket
(public-key) -> string?  
(public-key key) -> void?
  key : string?          
```

Your AWS public key, a.k.a. “Access ID.”

```racket
(private-key) -> string?  
(private-key key) -> void?
  key : string?           
```

Your AWS private key, a.k.a. “Secret Key.”

```racket
(read-keys [file]) -> void?                                  
  file : path?                                               
       = (build-path(find-system-path 'home-dir) ".aws-keys")
```

Set the parameters `public-key` and `private-key` by reading their
values from a plain text file. The file should consist of two lines:

`AWSAccessKeyId=<key>`
`AWSSecretKey=<key>`  

By default this file is `~/.aws-keys`. You probably want to `chmod` the
permissions of this file carefully.

```racket
(ensure-have-keys) -> void?
```

If either `public-key` or `private-key` is `""`, calls `read-keys`. If
either is _still_ blank, calls `error` with a hopefully helpful reminder
about how to set the parameters.

# 4. Exception handling

Most of the functions do not return a failure value. Instead they raise
`exn:fail:aws`, which you need to “catch” using `with-handlers`.

```racket
 (require aws/exn) package: aws
```

```racket
(struct exn:fail:aws (http-code http-message aws-code aws-message)
        #:extra-constructor-name make-exn:fail:aws)               
  http-code : exact-positive-integer?                             
  http-message : string?                                          
  aws-code : string?                                              
  aws-message : string?                                           
```

Used by `header&response->exn:fail:aws` and `check-response`.

```racket
(header&response->exn:fail:aws headers                 
                               entity                  
                               ccm)    -> exn:fail:aws?
  headers : string?                                    
  entity : (or/c bytes? xexpr?)                        
  ccm : continuation-mark-set?                         
```

Given an HTTP response’s `headers` and `entity`, return a `exn:fail:aws`
constructed with information from the response.

```racket
(check-response in headers)               
 -> (or/c string? (raise/c exn:fail:aws?))
  in : input-port?                        
  headers : string?                       
```

Check `headers`. If the status code is one of 200, 201, 202, 204, 206,
301, 302, or 307, simply return `headers` (without reading any response
entity from `in`).

Otherwise, read the XML response body from `in` and use the information
to construct and raise `exn:fail:aws`.

Note: This does _not_ close the input port `in` before raising an
exception. It assumes you are using `call/requests`,
`call/input-request`, or `call/output-request` from the `http/request`
library (or using `dynamic-wind` or other exception handling, or a
custodian—or whatever) to make sure the port is closed!

# 5. Connection pooling

This library uses the [http](https://github.com/greghendershott/http)
package to make HTTP connections to AWS. You may cause connections to be
reused ("pooled") by setting the
[current-pool-timeout](https://github.com/greghendershott/http/blob/master/http/manual.md\#12-connections-and-requests)
parameter to some non-zero number of seconds.

This can be faster, especially for many small requests in a row.

In the following example, the first `list-buckets` request will leave
the connection open for 30 seconds. As a result, the second
`list-buckets` request will reuse the same connection. After another 30
seconds, the connection will be closed automatically.

```racket
(require http/request                    
         aws/s3)                         
(parameterize ([current-pool-timeout 30])
  (list-buckets)                         
  (list-buckets))                        
```

# 6. S3 (Storage)

```racket
 (require aws/s3) package: aws
```

[S3](http://docs.amazonwebservices.com/AmazonS3/latest/dev/Welcome.html)
provides a fairly simple and REST-ful interface. Uploading an object to
S3 is an HTTP `PUT` request. Download an object is a `GET` request. And
so on. As a result, you may feel you don’t need a lot of “wrapper”
around this.

Where you definitely _will_ want help is constructing the
`Authorization` header S3 uses to authenticate requests. Doing so
requires making a string out of specific elements of your request and
“signing” it with your AWS private key. Even a small discrepancy will
cause the request to fail authentication. As a result, `aws/s3` makes it
easy for you to create the authentication header correctly and
successfully.

Plus, `aws/s3` does provide wrappers and tries to help with some
wrinkles. For example, S3 may give you a 302 redirect when you do a
`PUT` or `POST`. You don’t want to transmit the entire entity, only to
have S3 ignore it and you have to transmit it all over again. Instead,
you want to supply the request header `Expect: 100-continue`, which lets
S3 respond _before_ you transmit the entity.

## 6.1. Request Method

```racket
(s3-path-requests?) -> boolean?
(s3-path-requests? v) -> void? 
  v : boolean?                 
```

The default value `#f` means "Virtual Hosted" style and `#t` means "Path
Style" as described
[here](http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAPI.html).
"Virtual Hosted" is preferred. (Use "Path Style" only if you have a
legacy US Standard bucket with a name that doesn’t meet the restrictions
for DNS – for which `(valid-bucket-name? name #t)` returns `#f`.)

## 6.2. Endpoint

```racket
(s3-host) -> string?
(s3-host v) -> void?
  v : string?       
```

The hostname used for the S3 REST API. Defaults to `"s3.amazonaws.com"`.
May be any value from [the Endpoint
column](http://docs.aws.amazon.com/general/latest/gr/rande.html\#s3\_region).

```racket
(s3-scheme) -> (or/c "http" "https")
(s3-scheme v) -> void?              
  v : (or/c "http" "https")         
```

The scheme used for the S3 REST API. Defaults to `"http"`. Set to
`"https"` to connect using SSL.

## 6.3. Authentication signatures

```racket
(bucket&path->uri bucket path-to-resource) -> string?
  bucket : string?                                   
  path-to-resource : string?                         
```

Given `bucket` and `path` (both of which should _not_ start with a
leading `"/"`), use `s3-path-requests?`, `s3-scheme` and `s3-host` to
make the URI for the resource.

Example:

```racket
> (bucket&path->uri "bucket" "path/to/file") 
"http://bucket.s3.amazonaws.com/path/to/file"
```

```racket
(bucket+path->bucket&path&uri b+p) -> string? string? string?
  b+p : string?                                              
```

Given a combined bucket+path string such as `"bucket/path/to/resource"`,
return the bucket portion, path portion and URI.

Example:

```racket
> (bucket+path->bucket&path&uri "bucket/path/to/file")
"bucket"                                              
"path/to/file"                                        
"http://bucket.s3.amazonaws.com/path/to/file"         
```

```racket
(uri&headers b+p method headers) -> string? dict?
  b+p : string?                                  
  method : string?                               
  headers : dict?                                
```

Return the URI and headers for which to make an HTTP request to S3.
Constructs an `Authorization` header based on the inputs.

## 6.4. Conveniences

```racket
(create-bucket bucket-name [location]) -> void?
  bucket-name : string?                        
  location : (or/c #f string?) = #f            
```

Create a bucket named `bucket-name` in `location`. For valid values of
`location` [see the Region
column](http://docs.aws.amazon.com/general/latest/gr/rande.html\#s3\_region).
Omitting or supplying `#f` for `location` means the US Standard region.

Keep in mind that bucket names on S3 are global—shared among all users
of S3. You may want to make your bucket names include a domain name that
you own.

If you try to create a bucket with a name that is already used by
_another_ AWS account, you will get a `409 Conflict` response.

If you create a bucket that already exists under your _own_ account,
this operation is idempotent (it’s not an error, it’s simply a no-op).

Use `valid-bucket-name?` to check the validity of the `bucket-name` you
want to use.

```racket
(valid-bucket-name?  bucket-name                 
                    [dns-compliant?]) -> boolean?
  bucket-name : string?                          
  dns-compliant? : boolean? = #t                 
```

Checks whether a bucket name meets the criteria described
[here](http://docs.amazonwebservices.com/AmazonS3/latest/dev/BucketRestrictions.html).
The `dns-compliant?` argument corresponds to the more-restrictive rules
required for non-US Standard buckets and required to use the so-called
Virtual Host request method corresponding to the default value `#f` of
the `s3-path-requests?` parameter.

```racket
(delete-bucket bucket-name) -> void?
  bucket-name : string?             
```

Delete a bucket named `bucket-name`.

This operation is idempotent (it is a no-op to delete a bucket that has
already been deleted).

```racket
(list-buckets) -> (listof string?)
```

List all the buckets belonging to your AWS account.

```racket
(ls/proc  bucket+path                                    
          proc                                           
          init                                           
         [max-each                                       
          #:delimiter delimiter]) -> any/c               
  bucket+path : string?                                  
  proc : (any/c (listof xexpr?) . -> . any/c)            
  init : any/c                                           
  max-each : ((and/c integer? (between/c 1 1000))) = 1000
  delimiter : (or/c #f string?) = #f                     
```

List objects whose names start with the pathname `bucket+path` (which is
the form `"bucket/path/to/resource"`). S3 is queried to return results
for at most `max-each` objects at a time.

For each such batch, `proc` is called. The first time `proc` is called,
its first argument is the `init` value; subsequent times it’s given the
previous value that it returned.

The second argument to `proc` is a `(listof xexpr?)`, where each
`xexpr?` respresents XML returned by S3 for each object. The XML is the
`Contents` or `CommonPrefixes` portions of the `ListBucketResults` XML
response, where `CommonPrefixes` are produced only for a non-`#f`
`delimiter`.

The return value of `ls/proc` is the final return value of `proc`.

For example, `ls` is implemented as simply:

```racket
(map (lambda (x) (first-tag-value x 'Key))
     (ls/proc b+p append '()))            
```

```racket
(ls bucket+path) -> (listof string?)
  bucket+path : string?             
```

List the names of objects whose names start with the pathname
`bucket+path` (which is the form `"bucket/path/to/resource"`).

```racket
(ll* bucket+path) -> (listof (list/c xexpr? string? xexpr?))
  bucket+path : string?                                     
```

List objects whose names start with the path in `bucket+path` (which is
the form `"bucket/path/to/resource"`). Return a list, each item of which
is a list consisting of:

* an `xexpr` (as with `ls/proc`)

* response headers from a `HEAD` request (as with `head`)

* an `xexpr` representing the ACL (as with `get-acl`)

```racket
(ll bucket+path) -> (listof (list/c string? string? xexpr?))
  bucket+path : string?                                     
```

List objects whose names start with the path in `bucket+path` (which is
the form `"bucket/path/to/resource"`). Return a list, each item of which
is a list consisting of:

* name (as with `ls`)

* response headers from a `HEAD` request (as with `head`)

* an `xexpr` representing the ACL (as with `get-acl`)

```racket
(head bucket+path) -> string?
  bucket+path : string?      
```

Make a `HEAD` request for `bucket+path` (which is the form
`"bucket/path/to/resource"`) and return the headers as a `string` in
`net/head` format.

`bucket+path` is the form `"bucket/path/to/resource"`.

```racket
(delete bucket+path) -> string?
  bucket+path : string?        
```

Make a `DELETE` request to delete `bucket+path` (which is the form
`"bucket/path/to/resource"`)

```racket
(delete-multiple bucket paths) -> string?
  bucket : string?                       
  paths : (listof string?)               
```

Make a request to delete all `paths` (each which is the form
`"path/to/resource"`) in `bucket`. The `paths` list must have no more
than 1000 elements.

```racket
(copy bucket+path/from bucket+path/to) -> string?
  bucket+path/from : string?                     
  bucket+path/to : string?                       
```

> Tip: To rename an object, `copy` it then `delete` the original.

Copy an existing S3 object `bucket+path/from` to `bucket+path/to`,
including its metadata. Both names are of the form
`"bucket/path/to/resource"`.

It is not an error to copy to an existing object (it will be replaced).
It is even OK to copy an existing object to itself.

```racket
(get-acl bucket+path [heads]) -> xexpr?
  bucket+path : string?                
  heads : dict? = '()                  
```

Make a `GET` request for the
[ACL](http://docs.amazonwebservices.com/AmazonS3/latest/dev/S3\_ACLs\_UsingACLs.html)
of the object `bucket+path` (which is the form
`"bucket/path/to/resource"`).

S3 responds with an XML representation of the ACL, which is returned as
an `xexpr?`.

```racket
(put-acl bucket+path acl) -> void
  bucket+path : string?          
  acl : xexpr?                   
```

Make a `PUT` request to set the
[ACL](http://docs.amazonwebservices.com/AmazonS3/latest/dev/S3\_ACLs\_UsingACLs.html)
of the object  `bucket+path` to `acl`.

```racket
(get/proc  bucket+path                                   
           reader                                        
          [heads                                         
           range-begin                                   
           range-end]) -> any/c                          
  bucket+path : string?                                  
  reader : (input-port? string? -> any/c)                
  heads : dict? = '()                                    
  range-begin : (or/c #f exact-nonnegative-integer?) = #f
  range-end : (or/c #f exact-nonnegative-integer?) = #f  
```

> Although you may use `get/proc` directly, it is also a building block
> for other procedures that you may find more convenient, such as
> `get/bytes` and `get/file`.

Make a `GET` request for `bucket+path` (which is the form
`"bucket/path/to/resource"`).

The `reader` procedure is called with an `input-port?` and a `string?`
respresenting the response headers. The `reader` should read the
response entity from the port, being careful to read the exact number of
bytes as specified in the response header’s `Content-Length` field. The
return value of `reader` is the return value of `get/proc`.

You may pass request headers in the optional `heads` argument.

The optional arguments `range-begin` and `range-end` are used to supply
an HTTP `Range` request header. This header, which Amazon S3 supports,
enables a getting only a subset of the bytes.  Note that `range-end` is
_ex_clusive to be consistent with the Racket convention, e.g.
`subbytes`. (The HTTP `Range` header specifies the end as _in_clusive,
so your `range-end` argument is decremented to make the value for the
header.)

```racket
(get/bytes  bucket+path                                  
           [heads                                        
            range-begin                                  
            range-end]) -> bytes?                        
  bucket+path : string?                                  
  heads : dict? = '()                                    
  range-begin : (or/c #f exact-nonnegative-integer?) = #f
  range-end : (or/c #f exact-nonnegative-integer?) = #f  
```

Make a `GET` request for `bucket+path` (which is the form
`"bucket/path/to/resource"`) and return the response entity as `bytes?`.

You may pass request headers in the optional `heads` argument.

The optional arguments `range-begin` and `range-end` are used to supply
an optional `Range` request header. This header, which Amazon S3
supports, enables a getting only a subset of the bytes.  Note that
`range-end` is _ex_clusive to be consistent with the Racket convention,
e.g. `subbytes`. (The HTTP `Range` header specifies the end as
_in_clusive, so your `range-end` argument is decremented to make the
value for the header.)

The response entity is held in memory; if it is very large and you want
to "stream" it instead, consider using `get/proc`.

```racket
(get/file  bucket+path                                                            
           pathname                                                               
          [heads                                                                  
           #:mode mode-flag                                                       
           #:exists exists-flag]) -> void?                                        
  bucket+path : string?                                                           
  pathname : path-string?                                                         
  heads : dict? = '()                                                             
  mode-flag : (or/c 'binary 'text) = 'binary                                      
  exists-flag : (or/c 'error 'append 'update 'replace 'truncate 'truncate/replace)
              = 'error                                                            
```

Make a `GET` request for `bucket+path` (which is the form
`"bucket/path/to/resource"`) and copy the the response entity directly
to the file specified by `pathname`. The keyword arguments `#:mode` and
`#:exists` are identical to those for `call-with-output-file*`.

You may pass request headers in the optional `heads` argument.

```racket
(put  bucket+path                                   
      writer                                        
      data-length                                   
      mime-type                                     
      reader                                        
     [heads])     -> void?                          
  bucket+path : string?                             
  writer : (output-port . -> . void?)               
  data-length : (or/c #f exact-nonnegative-integer?)
  mime-type : string?                               
  reader : (input-port? string? . -> . any/c)       
  heads : dict? = '()                               
```

> Although you may use `put` directly, it is also a building block for
> other procedures that you may find more convenient, such as `put/bytes`
> and `put/file`.
> To upload more than about 100 MB, see `multipart-put`.

Makes a `PUT` request for `bucket+path` (which is the form
`"bucket/path/to/resource"`), using the `writer` procedure to write the
request entity and the `reader` procedure to read the response entity.
Returns the response header (unless it raises `exn:fail:aws`).

The `writer` procedure is given an `output-port?` and a `string?`
representing the response headers. It should write the request entity to
the port. The amount written should be exactly the same as
`data-length`, which is used to create a `Content-Length` request
header. You must also supply `mime-type` (for example `"text/plain"`)
which is used to create a `Content-Type` request header.

The `reader` procedure is the same as for `get/proc`. The response
entity for a `PUT` request usually isn’t interesting, but you should
read it anyway.

Note: If you want a `Content-MD5` request header, you must calculate and
supply it yourself in `heads`. Supplying this allows S3 to verify the
upload integrity.

To use reduced redundancy storage, supply `(hash 'x-amz-storage-class
"REDUCED_REDUNDANCY")` for `heads`.

```racket
(put/bytes bucket+path data mime-type [heads]) -> void?
  bucket+path : string?                                
  data : bytes?                                        
  mime-type : string?                                  
  heads : dict? = '()                                  
```

> To upload more than about 100 MB, see `multipart-put`.

Makes a `PUT` request for `bucket+path` (which is the form
`"bucket/path/to/resource"`), sending `data` as the request entity and
creating a `Content-Type` header from `mime-type`. Returns the response
header (unless it raises `exn:fail:aws`).

A `Content-MD5` request header is automatically created from `data`. To
ensure data integrity, S3 will reject the request if the bytes it
receives do not match the MD5 checksum.

To use reduced redundancy storage, supply `(hash 'x-amz-storage-class
"REDUCED_REDUNDANCY")` for `heads`.

```racket
(put/file  bucket+path                      
           pathname                         
          [#:mime-type mime-type            
           #:mode mode-flag])    -> void?   
  bucket+path : string?                     
  pathname : path-string?                   
  mime-type : (or/c #f string?) = #f        
  mode-flag : (or/c 'binary 'text) = 'binary
```

> For files larger than about 100 MB, see `multipart-put/file`.

Upload the file `pathname` to `bucket+path`.

Makes a `PUT` request for `bucket+path` (which is the form
`"bucket/path/to/resource"`) and copy the the request entity directly
from the file specified by `pathname`.  The `#:mode-flag` argument is
identical to that for `call-with-input-file*`, which is used.  Returns
the response header (unless it raises `exn:fail:aws`).

If `#:mime-type` is `#f`, then the `Content-Type` header is guessed from
the file extension, using a (very short!) list of common extensions. If
no match is found, then `"application/x-unknown-content-type"` is used.
You can customize the MIME type guessing by setting the
`path->mime-proc` parameter to your own procedure.

A `Content-MD5` request header is automatically created from the
contents of the file represented by `path`. To ensure data integrity, S3
will reject the request if the bytes it receives do not match the MD5
checksum.

A `Content-Disposition` request header is automatically created from
`pathname`. For example if `pathname` is `"/foo/bar/test.txt"` or
`"c:\\foo\\bar\\test.txt"` then the header
`"Content-Disposition:attachment; filename=\"test.txt\""` is created.
This is helpful because a web browser that is given the URI for the
object will prompt the user to download it as a file.

To use reduced redundancy storage, supply `(hash 'x-amz-storage-class
"REDUCED_REDUNDANCY")` for `heads`.

```racket
(path->mime-proc) -> procedure?
(path->mime-proc proc) -> void?
  proc : procedure?            
```

A procedure which takes a `path-string?` and returns a `string?` with a
MIME type.

## 6.5. Multipart uploads

In addition to uploading an entire object in a single `PUT` request, S3
lets you upload it in multiple 5 MB or larger chunks, using the
[multipart upload
API](http://docs.amazonwebservices.com/AmazonS3/2006-03-01/dev/UsingRESTAPImpUpload.html).
Amazon recommends using this when the total data to upload is bigger
than about 100 MB.

### 6.5.1. Convenience

```racket
(multipart-put  bucket+path                                 
                num-parts                                   
                get-part                                    
               [mime-type                                   
                heads])     -> string?                      
  bucket+path : string?                                     
  num-parts : exact-positive-integer?                       
  get-part : (exact-nonnegative-integer? . -> . bytes?)     
  mime-type : string? = "application/x-unknown-content-type"
  heads : dict? = '()                                       
```

Upload `num-parts` parts, where the data for each part is returned by
the `get-part` procedure you supply. In other words, your `get-part`
procedure is called `num-parts` times, with values `(in-range
num-parts)`.

Each part must be at least 5 MB, except the last part.

The parts are uploaded using a small number of worker threads, to get
some parallelism and probably better performance.

```racket
(multipart-put/file  bucket+path                     
                     path                            
                    [#:mime-type mime-type           
                     #:mode mode-flag])    -> string?
  bucket+path : string?                              
  path : path?                                       
  mime-type : string? = #f                           
  mode-flag : (or/c 'binary 'text) = 'binary         
```

Like `put/file` but uses multipart upload.

The parts are uploaded using a small number of worker threads, to get
some parallelism and probably better performance.

### 6.5.2. Building blocks

Use these if the data you’re uploading is computed on the fly and you
don’t know the total size in advance. Otherwise you may simply use
`multipart-put` or `multipart-put/file`.

```racket
(initiate-multipart-upload bucket+path           
                           mime-type             
                           heads)      -> string?
  bucket+path : string?                          
  mime-type : string?                            
  heads : dict?                                  
```

Initiate a multipart upload and return an upload ID.

```racket
(upload-part bucket+path                                       
             upload-id                                         
             part-number                                       
             bstr)                                             
 -> (cons/c (and/c exact-integer? (between/c 1 10000)) string?)
  bucket+path : string?                                        
  upload-id : string?                                          
  part-number : (and/c exact-integer? (between/c 1 10000))     
  bstr : bytes?                                                
```

Upload one part for the multipart upload specified by the `upload-id`
returned from `initiate-multipart-upload`.

Note that S3 part numbers start with `1` (not `0`).

`bstr` must be at least 5 MB, unless it’s the last part.

Returns a `cons` of `part-number` and the `ETag` for the part. You will
need to supply a list of these, one for each part, to
`complete-multipart-upload`.

```racket
(complete-multipart-upload bucket+path                                             
                           upload-id                                               
                           parts-list) -> xexpr?                                   
  bucket+path : string?                                                            
  upload-id : string?                                                              
  parts-list : (listof (cons/c (and/c exact-integer? (between/c 1 10000)) string?))
```

Complete the multipart upload specified the by `upload-id` returned from
`initiate-multipart-upload`, using a `parts-list` of the values returned
from each `upload-part`. The `parts-list` does not need to be in any
particular order; it will be sorted for you.

Returns S3’s XML response in the form of an `xexpr?`.

```racket
(abort-multipart-upload bucket+path         
                        upload-id)  -> void?
  bucket+path : string?                     
  upload-id : string?                       
```

Abort the multipart upload specified by the `upload-id` returned from
`initiate-multipart-upload`.

## 6.6. S3 examples

```racket
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
```

# 7. SDB (Database)

```racket
 (require aws/sdb) package: aws
```

[SimpleDB](http://docs.amazonwebservices.com/AmazonSimpleDB/latest/DeveloperGuide/Welcome.html)
is a “schema-less” database. You should review the SDB docs to
understand the basic concepts and names. For example an SDB _domain_ is
like a SQL table, an SDB _item_ is like a SQL row, and an SDB _item
name_ is like a SQL primary key value to unqiuely identify a row.

Instead of columns, each item has _attributes_. Each attribute is a
key/value hash. In other words, unlike a SQL column which has one value,
each SDB attribute may have multiple values.  For example an attribute
with the key `"Sizes"` might have the values `"Small"`, `"Medium"`, and
`"Large"`. The values should be considered a strict set (just one
occurrence of each unique value).

## 7.1. Making requests to SDB

```racket
(sdb-endpoint) -> endpoint?
(sdb-endpoint v) -> void?  
  v : endpoint?            
```

Set the endpoint for the service. Defaults to `(endpoint
"sdb.amazonaws.com" #t)`.

```racket
(create-domain name) -> void?
  name : string?             
```

Create an SDB domain, which is like a “table” in SQL. Remember that SDB
operations are idempotent; doing this more than once is equivalent to
doing it once.

Tip: Although you may put non alpha-numeric characters in a name, and
most of the API will work, the select procedures won’t, unless you
bracket the domain name to make it legal SQL.

```racket
(delete-domain name) -> void?
  name : string?             
```

Delete an SDB domain. Remember that SDB operations are idempotent; doing
this more than once is equivalent to doing it once.

```racket
(list-domains) -> (listof (list/c 'DomainName string?))
```

List all of the SDB domains associated with the AWS SDB account.

```racket
(domain-metadata name) -> (listof (list/c symbol? string?))
  name : string?                                           
```

Show metadata for a specific SDB domain.

```racket
(always-replace?) -> boolean?     
(always-replace? always?) -> void?
  always? : boolean?              
```

Set this parameter to `#t` to make the `Item.Replace` true for all calls
to `put-attributes`.  Else if at default `#f` value, `Item.Replace` will
be specified only if you do it for each attribute, using `(key val
'replace)` instead of `(key val)`.

```racket
(put-attributes domain-name                     
                item-name                       
                attributes) -> any              
  domain-name : string?                         
  item-name : string?                           
  attributes : (listof (list/c symbol? string?))
```

Put the `attributes` to `item-name` in the `domain-name`. Remember that
SDB operations are idempotent; doing this more than once is equivalent
to doing it once.

```racket
(get-attributes domain-name item-name)
 -> (listof (list/c symbol? string?)) 
  domain-name : string?               
  item-name : string?                 
```

Get the attributes for `item-name` in `domain-name`. Keep in mind that
SDB has “eventual consistency”; it may take awhile for the result of
`put-attributes` to be reflected in `get-attributes`.

```racket
(delete-attributes domain-name                  
                   item-name                    
                   attributes) -> void?         
  domain-name : string?                         
  item-name : string?                           
  attributes : (listof (list/c symbol? string?))
```

Delete the `attributes` for `item-name` in `domain-name`. Remember that
SDB operations are idempotent; doing this more than once is equivalent
to doing it once and is not an error.

```racket
(delete-item domain-name item-name) -> void?
  domain-name : string?                     
  item-name : string?                       
```

Delete `item-name` from `domain-name.`  Remember that SDB operations are
idempotent; doing this more than once is equivalent to doing it once and
is not an error.

```racket
(select expr) -> (listof (list/c symbol? string?))
  expr : string?                                  
```

Execute the SQL-ish `expr`. See the SDB docs for the subset of SQL that
is supported.

## 7.2. Batch

```racket
(batch-put-attributes domain-name xs) -> any                      
  domain-name : string?                                           
  xs : (listof (cons/c string? (listof (list/c symbol? string?))))
```

For efficiency, SDB provides this to put multiple attributes to multiple
items in one request.

```racket
(batch-delete-attributes domain-name xs) -> void?                 
  domain-name : string?                                           
  xs : (listof (cons/c string? (listof (list/c symbol? string?))))
```

For efficiency, SDB provides this to delete put multiple attributes from
multiple items in one request.

## 7.3. Hash/Set style

```racket
(put-attributes-hash domain item attribs) -> void?
  domain : string?                                
  item : string?                                  
  attribs : (hash/c symbol? (set/c string?))      
(get-attributes-hash domain item)                 
 -> (hash/c symbol? (set/c string?))              
  domain : string?                                
  item : string?                                  
(select-hash expr) -> (listof item?)              
  expr : string?                                  
(struct item (name attribs)                       
        #:extra-constructor-name make-item)       
  name : string?                                  
  attribs : (hash/c symbol? (set/c string?))      
```

`put-attributes` and `get-attributes` are a low-level interface that
wraps SDB fairly thinly.  When you want exact control, use them.

These procedures provide a set-oriented interface. For a multi-valued
attribute, you get and set all its values together as one set.  The
attribute’s values are represented as `(set/c string?)`. A collection of
attributes is `(hash/c symbol? (set/c string?))`. When you get a
multi-valued attribute, all of its values are grouped and presented as a
`(set/c string?)`. When you put the attribute set, all of its existing
values in SDB are replaced by the new set of values you supply. (At a
lower level, this means the first attribute is put to SDB using
parameter `Replace=true`—to clear any/all existing values. The other
values for the attribute are put with `Replace=false`—to preserve all of
the multiple new values we are setting.)

## 7.4. Values as strings

SDB stores all values as strings. You choose how a number is represented
as a string. Your choice matters for sorts and compares. The SDB docs
recommend:

* pad numbers with leading zeroes

* and apply an offset to negative numbers so they are stored as a
  nonnegative number

Analgesic below.

```racket
(int<->str [width offset pad-char])         
 -> (number? -> string?) (string? -> number)
  width : exact-positive-integer? = 5       
  offset : exact-nonnegative-integer? = 0   
  pad-char : character? = #\0               
```

This procedure creates a pair of procedures, to convert in each
direction between a `number` and its padded/offset `string`
representation.

```racket
(str->int/u8 s) -> number? 
  s : string?              
(int->str/u8 n) -> string? 
  n : number?              
(str->int/s8 s) -> number? 
  s : string?              
(int->str/s8 n) -> string? 
  n : number?              
(str->int/u16 s) -> number?
  s : string?              
(int->str/u16 n) -> string?
  n : number?              
(str->int/s16 s) -> number?
  s : string?              
(int->str/s16 n) -> string?
  n : number?              
(str->int/u32 s) -> number?
  s : string?              
(int->str/u32 n) -> string?
  n : number?              
(str->int/s32 s) -> number?
  s : string?              
(int->str/s32 n) -> string?
  n : number?              
```

Converters created using `int<->str` for signed and unsigned integers of
8, 16, and 32 bytes.

Examples:

```racket
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
```

## 7.5. SDB Examples

In the examples below, the reason for using `sleep` is that SDB has an
“eventual consistency” model. As a result, there may be a short delay
before the values we set are available to get.

```racket
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
```

# 8. SES (Email)

```racket
 (require aws/ses) package: aws
```

Please refer to the [SES
documentation](http://docs.amazonwebservices.com/ses/latest/DeveloperGuide/Welcome.html)
to understand concepts like a verified sending adddress.

```racket
(ses-endpoint) -> endpoint?
(ses-endpoint v) -> void?  
  v : endpoint?            
```

Set the endpoint for the service. Defaults to `(endpoint
"email.us-east-1.amazonaws.com" #t)`.

```racket
(send-email  #:from from                       
             #:to to                           
             #:subject subject                 
             #:body body                       
            [#:cc cc                           
             #:bcc bcc                         
             #:reply-to reply-to               
             #:return-path return-path         
             #:html? html?                     
             #:charset charset])       -> void?
  from : string?                               
  to : (listof string?)                        
  subject : string?                            
  body : string?                               
  cc : (listof string?) = '()                  
  bcc : (listof string?) = '()                 
  reply-to : (listof-string?) = '()            
  return-path : string? = from                 
  html? : boolean? = #f                        
  charset : string? = "UTF-8"                  
```

Send an email. Unless `from` has been verified (for example using
`verify-email-address` SES will fail.

If SES returns `400 Bad Request` with
`<Code>Throttling</Code><Message>Maximum sending rate
exceeded.</Message>`, this repeatedly sleeps for a random 1-16 second
interval then retries, until it succeeds or SES fails with some other
error.

```racket
(send-raw-email mail-from             
                rcpt-to               
                raw-message) -> xexpr?
  mail-from : string?                 
  rcpt-to : string?                   
  raw-message : string?               
```

Send a raw email. SES requires a `Source` to be specified. If a
`Return-Path` mail header is supplied in `raw-message` then that will be
used as the `Source`, otherwise `mail-from` will be used.

If SES returns `400 Bad Request` with
`<Code>Throttling</Code><Message>Maximum sending rate
exceeded.</Message>`, this repeatedly sleeps for a random 1-16 second
interval then retries, until it succeeds or SES fails with some other
error.

```racket
(verify-email-address address) -> void?
  address : string?                    
```

Verify the email address with SES, so that it can be used to send emails
via SES.

```racket
(delete-verified-email-address address) -> void?
  address : string?                             
```

Unverify the email address.

```racket
(list-verified-email-addresses) -> (listof string?)
```

Return the list of email addresses currently verified with SES.

```racket
(get-send-quota) -> send-quota?                                      
(struct send-quota                                (sent-last-24-hours
                                                  max-24-hour-send   
                                                  max-send-rate)     
        #:extra-constructor-name make-send-quota)                    
  sent-last-24-hours : number?                                       
  max-24-hour-send : number?                                         
  max-send-rate : number?                                            
```

Get the send quota.

```racket
(get-send-statistics) -> (listof send-statistic?)                      
(struct send-statistic                                (time            
                                                      delivery-attempts
                                                      rejects          
                                                      bounces          
                                                      complaints)      
        #:extra-constructor-name make-send-statistic)                  
  time : string?                                                       
  delivery-attempts : number?                                          
  rejects : number?                                                    
  bounces : number?                                                    
  complaints : number?                                                 
```

Get send statistics. Although SES keeps statistics for only your last 14
days of sending, each statistic is for a 15 minute bucket and the
`(listof send-statistics)` may be quite long. Note that the list is not
necessarily sorted in any particular order.

```racket
(ses params) -> xexpr?                      
  params : (listof (list/c symbol? string?))
```

The low-level procedure used by other procedures to make requests to
SES.

If SES adds new actions and this library isn’t updated to support them,
you may be able to support them by setting the `Action` parameter.

# 9. SNS (Notifications)

```racket
 (require aws/sns) package: aws
```

[SNS](http://docs.amazonwebservices.com/sns/latest/api/Welcome.html?r=9480)
lets you create topics to which notifications can be published. Each
topic has zero or more subscriptions.

Subscriptions can be of various types of endpoints, such as email, SMS,
or an HTTP `POST` of a JSON-encoded message.

A new subscription must be confirmed by the recipient before it will
receive notifications, to prevent unwanted notifications.

Topics and subscriptions are unqiuely identified by a string referred to
as an ARN (Amazon Resource Name).  The string is composed of the service
endpoint hostname, your AWS account number, and a name. An example ARN
is `arn:aws:sns:us-east-1:123456789012:My-Topic`.

```racket
(sns-endpoint) -> endpoint?
(sns-endpoint v) -> void?  
  v : endpoint?            
```

Set the endpoint for the service. Defaults to `(endpoint
"sns.us-east-1.amazonaws.com" #f)`.

```racket
(create-topic name) -> string?
  name : string?              
```

Create a topic and return its ARN.

```racket
(delete-topic arn) -> void?
  arn : string?            
```

Delete a topic.

```racket
(get-topic-attributes arn) -> (listof (cons/c symbol? string?))
  arn : string?                                                
```

Get the attributes for a topic as an association list.

```racket
(list-topics) -> (listof string?)
```

Get the ARN for all SNS topics for the AWS account.

```racket
(struct subscription                                (protocol       
                                                    owner           
                                                    topic-arn       
                                                    subscription-arn
                                                    endpoint)       
        #:extra-constructor-name make-subscription)                 
  protocol : string?                                                
  owner : string?                                                   
  topic-arn : string?                                               
  subscription-arn : string?                                        
  endpoint : string?                                                
(list-subscriptions) -> (listof subscription?)                      
(list-subscriptions-by-topic arn) -> (listof subscription?)         
  arn : string?                                                     
```

Get subscriptions for all topic, or just those for a given topic.

```racket
(sns-protocol? s) -> boolean?                     
  s : string?                                     
(subscribe endpoint protocol topic-arn) -> string?
  endpoint : string?                              
  protocol : sns-protocol?                        
  topic-arn : string?                             
```

Subscribe to a topic, returning the ARN for the subscription.

```racket
(unsubscribe subscription-arn) -> void?
  subscription-arn : string?           
```

Delete a subscription.

```racket
(publish topic-arn                 
         message                   
         #:subject subject         
         #:json? json?)    -> void?
  topic-arn : string?              
  message : string?                
  subject : ""                     
  json? : #f                       
```

Publish a notification message to a topic. If `#:json?` is `#t` then
`message` must be valid JSON or SNS will return an error.

# 10. SQS (Queues)

```racket
 (require aws/sqs) package: aws
```

[SQS](http://docs.amazonwebservices.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/Welcome.html)
provides distributed queues.

```racket
(sqs-endpoint) -> endpoint?
(sqs-endpoint v) -> void?  
  v : endpoint?            
```

Set the endpoint for the service. Defaults to `(endpoint
"sqs.us-east-1.amazonaws.com" #f)`.

```racket
(create-queue name) -> string?
  name : string?              
```

Create a queue and return its URI. The URI is used to identify the queue
in most of the other procedures.

```racket
(delete-queue queue-uri) -> void?
  queue-uri : string?            
```

Delete a queue.

```racket
(list-queues) -> (listof string?)
```

List all the queues associated with the AWS account.

```racket
(get-queue-uri name) -> string?
  name : string?               
```

Given the name of a queue, get its URI.

```racket
(send-message queue-uri body [delay-seconds]) -> void?     
  queue-uri : string                                       
  body : string?                                           
  delay-seconds : (or/c #f exact-nonnegative-integer?) = #f
```

Send a message to a queue. See SQS docs for meaning of `delay-seconds`,
but, if not supplied the delay will default to that of the queue.

```racket
(get-queue-attributes queue-uri)     
 -> (listof (list/c symbol? string?))
  queue-uri : string?                
```

Get all the attributes for a queue. They are returned as a list instead
of a `struct` because the list of attributes may grow in future versions
of SQS.

```racket
(struct message (body md5 receipt-handle attributes)            
        #:extra-constructor-name make-message)                  
  body : string?                                                
  md5 : string?                                                 
  receipt-handle : string?                                      
  attributes : (listof (list/c symbol? string?))                
(receive-messages  queue-uri                                    
                   max                                          
                  [visibility-timeout]) -> (listof message?)    
  queue-uri : string?                                           
  max : (and/c exact-integer? (between/c 1 10))                 
  visibility-timeout : (or/c #f exact-nonnegative-integer?) = #f
(receive-message  queue-uri                                     
                 [visibility-timeout]) -> (listof message?)     
  queue-uri : string?                                           
  visibility-timeout : (or/c #f exact-nonnegative-integer?) = #f
```

Get one or more messages from the queue.

`receive-message` is simply sugar for `receive-messages` with 1 supplied
for `max`.

The `receipt-handle` field of `message` is used to identify the message
in procedures that operate on a specific message.

Note: The `attributes` field of `message` is the same format as
`get-queue-attributes` and for the same reason.

```racket
(delete-message queue-uri receipt-handle) -> void?
  queue-uri : string?                             
  receipt-handle : string?                        
```

Delete a message from a queue.

```racket
(change-message-visibility queue-uri              
                           receipt-handle         
                           timeout)       -> void?
  queue-uri : string?                             
  receipt-handle : string?                        
  timeout : exact-nonnegative-integer?            
```

Change the visibility time of a message already in a queue.

# 11. Route 53 (DNS)

```racket
 (require aws/r53) package: aws
```

[Route
53](http://docs.amazonwebservices.com/Route53/latest/APIReference/Welcome.html)
provides DNS.

```racket
(r53-endpoint) -> endpoint?
(r53-endpoint v) -> void?  
  v : endpoint?            
```

Set the endpoint for the service. Defaults to `(endpoint
"route53.amazonaws.com" #f)`.

```racket
(create-hosted-zone name unique [comment]) -> xexpr?
  name : string?                                    
  unique : string?                                  
  comment : string? = ""                            
```

Create a hosted zone and return an `xexpr?` respresenting the response
XML.

```racket
(delete-hosted-zone zone-id) -> xexpr?
  zone-id : string?                   
```

Delete a hosted zone and return an `xexpr?` respresenting the response
XML.

Note that `zone-id` is _not_ the domain name, it is the "zone ID".

```racket
(list-hosted-zones) -> xexpr?
```

List all the hosted zones associated with the AWS account.

```racket
(get-hosted-zone zone-id) -> xexpr?
  zone-id : string?                
```

Given a `zone-id`, return information about the hosted zone.

```racket
(domain-name->zone-id name) -> string?
  name : string?                      
```

Look up a zone ID from a domain name.

AWS requires the domain name to be in DNS style and end in a period,
such as `"foo.com."`. However if `name` doesn’t end in a period, then
`domain-name->zone-id` automatically appends one for you.

```racket
(list-resource-record-sets zone-id               
                           #:max-items max-items 
                           #:name name           
                           #:type type           
                           #:id id)              
 -> (listof xexpr?)                              
  zone-id : string?                              
  max-items : #f                                 
  name : #f                                      
  type : #f                                      
  id : #f                                        
```

Return a list of `ResourceRecordSet` `xexpr?`s.

```racket
(change-resource-record-sets zone-id           
                             changes) -> xexpr?
  zone-id : string?                            
  changes : xexpr?                             
```

Make changes to the record sets for the the zone.

It’s up to the caller to create an xexpr according to [the AWS
docs](http://docs.amazonwebservices.com/Route53/latest/APIReference/API\_ChangeResourceRecordSets.html).

Example:

```racket
(change-resource-record-sets                                         
 "/hostedzone/Z3K3IRK2M12WGD"                                        
 `(ChangeResourceRecordSetsRequest                                   
   ([xmlns "https://route53.amazonaws.com/doc/2012-02-29/"])         
   (ChangeBatch                                                      
    (Comment "optional comment about the changes in this change batch
request")                                                            
    (Changes (Change                                                 
              (Action "CREATE")                                      
              (ResourceRecordSet (Name "foo2.com")                   
                                 (Type "A")                          
                                 (TTL "300")                         
                                 (ResourceRecords                    
                                  (ResourceRecord                    
                                   (Value "1.2.3.4")))))))))         
```

# 12. Dynamo DB

```racket
 (require aws/dynamo) package: aws
```

[Dynamo](http://docs.amazonwebservices.com/amazondynamodb/latest/developerguide/Introduction.html)
is Amazon’s newer "NoSQL" service.

```racket
(dynamo-endpoint) -> endpoint?
(dynamo-endpoint v) -> void?  
  v : endpoint?               
```

Set the endpoint for the service. Defaults to `(endpoint
"dynamodb.us-east-1.amazonaws.com" #f)`.

```racket
(dynamo-region) -> string?
(dynamo-region v) -> void?
  v : string?             
```

Set the region for the service. Defaults to `"us-east-1"`.

```racket
attribute-type/c : (or/c "S" "N" "B")
```

A contract for Dynamo attribute types (string, number, base64 binary).

```racket
(create-table  name                       
               read-units                 
               write-units                
               hash-key-name              
               hash-key-type              
              [range-key-name             
               range-key-type]) -> jsexpr?
  name : string?                          
  read-units : exact-positive-integer?    
  write-units : exact-positive-integer?   
  hash-key-name : string?                 
  hash-key-type : attribute-type/c        
  range-key-name : string? = #f           
  range-key-type : attribute-type/c = #f  
```

Create a table.

```racket
(delete-table name) -> jsexpr?
  name : string?              
```

Delete a table.

```racket
(describe-table name) -> jsexpr?
  name : string?                
```

Describe a table.

```racket
(list-tables #:limit limit #:from from) -> jsexpr?
  limit : #f                                      
  from : #f                                       
```

List at most `limit` tables, starting with the table name `from` (if
continuing a listing that had previously stopped at `limit`).

```racket
(put-item js) -> jsexpr?        
  js : jsexpr?                  
(get-item js) -> jsexpr?        
  js : jsexpr?                  
(delete-item js) -> jsexpr?     
  js : jsexpr?                  
(update-item js) -> jsexpr?     
  js : jsexpr?                  
(batch-get-item js) -> jsexpr?  
  js : jsexpr?                  
(batch-write-item js) -> jsexpr?
  js : jsexpr?                  
(query js) -> jsexpr?           
  js : jsexpr?                  
(scan js) -> jsexpr?            
  js : jsexpr?                  
(update-table js) -> jsexpr?    
  js : jsexpr?                  
```

The remaining functions accept JSON which you must construct yourself in
the form of a `jsexpr?`. The variation in the JSON is sufficient that
wrapping them in some arbitrary Racket structure doesn’t provide added
value.  Instead, please see the Dynamo documentation for these
similarly-named functions.

* [put-item](http://docs.amazonwebservices.com/amazondynamodb/latest/developerguide/API\_PutItem.html)

* [get-item](http://docs.amazonwebservices.com/amazondynamodb/latest/developerguide/API\_GetItem.html)

* [delete-item](http://docs.amazonwebservices.com/amazondynamodb/latest/developerguide/API\_DeleteItem.html)

* [update-item](http://docs.amazonwebservices.com/amazondynamodb/latest/developerguide/API\_UpdateItem.html)

* [batch-get-item](http://docs.amazonwebservices.com/amazondynamodb/latest/developerguide/API\_BatchGetItems.html)

* [batch-write-item](http://docs.amazonwebservices.com/amazondynamodb/latest/developerguide/API\_BatchWriteItem.html)

* [query](http://docs.amazonwebservices.com/amazondynamodb/latest/developerguide/API\_Query.html)

* [scan](http://docs.amazonwebservices.com/amazondynamodb/latest/developerguide/API\_Scan.html)

* [update-table](http://docs.amazonwebservices.com/amazondynamodb/latest/developerguide/API\_UpdateTable.html)

# 13. CloudWatch (Monitoring)

```racket
 (require aws/cw) package: aws
```

Among the Amazon Web Services, the
[CloudWatch](http://docs.amazonwebservices.com/AmazonCloudWatch/latest/DeveloperGuide/Welcome.html)
API is the most quirky, least documented, and provides the fewest
specific examples of making requests.

As a result, there are more likely to be mistakes or problems in this
module. Your feedback or contributions to improve it are welcome.

Meanwhle, I’ve tried to focus mostly on `put-metric-data`, less on the
other metrics procedures, and least of all on alarms and alarm history.
(This is in line with the overall priority of this library, which is to
support applications not “infrastucture.”)

```racket
(cw-endpoint) -> endpoint?
(cw-endpoint v) -> void?  
  v : endpoint?           
```

Set the endpoint for the service. Defaults to `(endpoint
"monitoring.us-east-1.amazonaws.com" #t)`.

## 13.1. Contracts

```racket
unit/c : (or/c 'None                                             
               'Percent                                          
               'Count                                            
               'Seconds 'Microseconds 'Milliseconds              
               'Bytes 'Kilobytes 'Megabytes 'Gigabytes 'Terabytes
               'Bits 'Kilobits 'Megabits 'Gigabits 'Terabits     
               'Count/Second                                     
               'Bytes/Second 'Kilobytes/Second 'Megabytes/Second 
                 'Gigabytes/Second 'Terabytes/Second             
               'Bits/Second 'Kilobits/Second 'Megabits/Second    
                 'Gigabits/Second 'Terabits/Second)              
```

A contract for the `Units` that CloudWatch accepts.

```racket
period/c : (make-flat-contract                           
            #:name 'non-zero-multiple-of-60              
            #:first-order (lambda (x)                    
                            (and (>= x 60)               
                                 (zero? (modulo x 60)))))
```

A contract for the `period` argument to `get-metric-statistcs` and
`describe-alarms-for-metric`. CloudWatch requires the `Period` to be a
non-zero multiple of 60 seconds.

```racket
statistic/c : (or/c 'Sum 'Average 'Maximum 'Minimum 'SampleCount)
```

A contract for the `Statistic` values that CloudWatch knows about.

```racket
dimensions/c : (listof (list/c symbol? string?))
```

## 13.2. Putting metric data and getting statistics

```racket
(struct datum                                (metric-name
                                             value       
                                             min         
                                             max         
                                             sum         
                                             sample-count
                                             unit        
                                             timestamp   
                                             dimensions) 
        #:extra-constructor-name make-datum)             
  metric-name : string?                                  
  value : (or/c #f number?)                              
  min : (or/c #f number?)                                
  max : (or/c #f number?)                                
  sum : (or/c #f number?)                                
  sample-count : (or/c #f number?)                       
  unit : unit/c                                          
  timestamp : exact-integer?                             
  dimensions : dimensions/c                              
```

This struct is accepted by `put-metric-data` and returned by
`get-metric-statistics`.

The `timestamp` member is an `exact-integer?`, like `current-seconds`,
but all CloudWatch timestamps are UTC not local time.

```racket
(put-metric-data namespace data) -> void?
  namespace : string?                    
  data : (listof datum?)                 
```

Put metric data to CloudWatch.

The `value` member must not be `#f`.

The `min`, `max`, `sum`, and `sample-count` members may be `#f` if you
are putting individual values and will let CloudWatch do the
aggregation, or, they may be non-`#f` if you are providing CloudWatch
data you have already aggregated.

```racket
(get-metric-statistics  #:metric-name metric-name 
                        #:namespace namespace     
                        #:statistics statistics   
                        #:unit unit               
                        #:start-time start-time   
                        #:end-time end-time       
                       [#:period period           
                        #:dimensions dimensions]) 
 -> (listof datum?)                               
  metric-name : string?                           
  namespace : string?                             
  statistics : (listof statistic/c)               
  unit : unit/c                                   
  start-time : exact-integer?                     
  end-time : exact-integer?                       
  period : period/c = 60                          
  dimensions : dimensions/c = '()                 
```

Return statistics for a given metric `metric-name` in `namespace`. The
statistics are returned as a list of `datum` structs.

The `value` member of `datum` will always be `#f` because CloudWatch
only returns aggregated data. Even if you put individual values using
`put-metric-data`, it will return only the aggregated statistics.

Whether the `min`, `max`, `sum`, and `sample-count` members of `datum`
are `#f`, depends on whether you asked those statistics to be returned
by specifying them in `statistics`.  For example if `statistics`
includes the symbol `'Sum`, then the `sum` member will be non-`#f`,
otherwise it will be `#f`.

## 13.3. Listing metrics

```racket
(struct metric (name namespace dimensions)   
        #:extra-constructor-name make-metric)
  name : string?                             
  namespace : string?                        
  dimensions : dimensions/c                  
```

```racket
(list-metrics [#:metric-name metric-name                    
               #:namespace namespace                        
               #:dimensions dimensions]) -> (listof metric?)
  metric-name : (or/c #f string?) = #f                      
  namespace : (or/c #f string?) = #f                        
  dimensions : dimensions/c = '()                           
```

Return a list of `metric?` meeting the criteria.

## 13.4. Alarms

```racket
(struct alarm                                (name                          
                                             description                    
                                             arn                            
                                             configuration-updated-timestamp
                                             metric-name                    
                                             namespace                      
                                             threshold                      
                                             comparison-operator            
                                             alarm-actions                  
                                             ok-actions                     
                                             insufficient-data-actions      
                                             state-value                    
                                             state-reason                   
                                             state-reason-data              
                                             state-updated-timestamp        
                                             period                         
                                             actions-enabled                
                                             evaluation-periods             
                                             statistic                      
                                             dimensions)                    
        #:extra-constructor-name make-alarm)                                
  name : string?                                                            
  description : string?                                                     
  arn : string?                                                             
  configuration-updated-timestamp : exact-integer?                          
  metric-name : string?                                                     
  namespace : string?                                                       
  threshold : number?                                                       
  comparison-operator : string?                                             
  alarm-actions : xexpr?                                                    
  ok-actions : xexpr?                                                       
  insufficient-data-actions : xexpr?                                        
  state-value : string?                                                     
  state-reason : string?                                                    
  state-reason-data : string?                                               
  state-updated-timestamp : exact-integer?                                  
  period : period/c                                                         
  actions-enabled : boolean?                                                
  evaluation-periods : exact-nonnegative-integer                            
  statistic : string?                                                       
  dimensions : dimensions/c                                                 
```

A structure used by `describe-alarms` and `describe-alarms-for-metric`.

The `configuration-updated-timestamp` and `state-updated-timestamp`
members are `exact-integer?` as with `current-seconds`, but remember the
CloudWatch times are UTC not local time.

```racket
(describe-alarms [#:alarm-name-prefix alarm-name-prefix     
                  #:alarm-names alarm-names                 
                  #:state-value state-value])               
 -> (listof metric-alarm?)                                  
  alarm-name-prefix : (or/c #f string?) = #f                
  alarm-names : (listof string?) = '()                      
  state-value : (or/c #f 'OK 'ALARM 'INSUFFICIENT_DATA) = #f
```

Return the alarms meeting the criteria.

```racket
(describe-alarms-for-metric  #:metric-name metric-name 
                             #:namespace namespace     
                            [#:statistic statistic     
                             #:unit unit               
                             #:period period           
                             #:dimensions dimensions]) 
 -> (listof metric-alarm?)                             
  metric-name : string?                                
  namespace : string?                                  
  statistic : (or/c #f statistic/c) = #f               
  unit : (or/c #f unit/c) = #f                         
  period : (or/c #f period/c) = #f                     
  dimensions : dimensions/c = '()                      
```

Return the alarms meeting the criteria.

```racket
(struct alarm-history (timestamp item-type name data summary)
        #:extra-constructor-name make-alarm-history)         
  timestamp : exact-integer?                                 
  item-type : symbol?                                        
  name : string?                                             
  data : xexpr?                                              
  summary : string?                                          
```

The `data` is JSON, which you will get as an `xexpr?` and need to parse
yourself.

The `timestamp` member is an `exact-integer?`, like `current-seconds`,
but all CloudWatch timestamps are UTC not local time.

```racket
(describe-alarm-history [#:alarm-name alarm-name                    
                         #:start-date start-date                    
                         #:end-date end-date                        
                         #:history-type history-type])              
 -> (listof alarm-history?)                                         
  alarm-name : (or/c #f string?) = #f                               
  start-date : (or/c #f exact-integer?) = #f                        
  end-date : (or/c #f exact-integer?) = #f                          
  history-type : (or/c #f 'ConfigurationUpdate 'StateUpdate 'Action)
               = #f                                                 
```

Return the history for alarms meeting the criteria.

# 14. Glacier (Archives)

```racket
 (require aws/glacier) package: aws
```

[Glacier](http://docs.amazonwebservices.com/amazonglacier/latest/dev/introduction.html)
provides storage for archiving. You can store objects less expensively
than using S3. The trade-off is that it is very slow to retreive them.

## 14.1. Region

```racket
(region) -> string?
(region v) -> void?
  v : string?      
```

Set the region. Defaults to `"us-east-1"`.

## 14.2. Vaults

```racket
(create-vault name) -> (or/c #t exn:fail:aws?)
  name : string?                              
```

[Create a
vault](http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-vault-put.html)
and return `#t` or raise `exn:aws:fail?`. Idempotent.

```racket
(delete-vault name) -> (or/c #t exn:fail:aws?)
  name : string?                              
```

[Delete a
vault](http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-vault-delete.html)
and return `#t` or raise `exn:aws:fail?`. Idempotent.

```racket
(list-vaults) -> jsexpr?
```

[List
vaults](http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-vaults-get.html)
in a `jsexpr?`.

```racket
> (list-vaults)                                                                   
'(#hasheq((VaultName . "testvault")                                               
          (CreationDate . "2012-08-30T12:29:37.200Z")                             
          (LastInventoryDate . null)                                              
          (NumberOfArchives . 0)                                                  
          (SizeInBytes . 0)                                                       
          (VaultARN . "arn:aws:glacier:us-east-1:203585791165:vaults/testvault")))
```

```racket
(describe-vault name) -> jsexpr?
  name : string?                
```

[Describe a
vault](http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-vault-get.html)
in a `jsexpr?`.

## 14.3. Vault notifications

```racket
(set-vault-notifications name                                 
                         sns-topic                            
                         inventory?                           
                         archive?)  -> (or/c #t exn:fail:aws?)
  name : string?                                              
  sns-topic : string?                                         
  inventory? : boolean?                                       
  archive? : boolean?                                         
```

Set a vault’s notification configuration.

```racket
(get-vault-notifications name) -> jsexpr?
  name : string?                         
```

Get a vault’s notification configuration.

```racket
(delete-vault-notifications name) -> void?
  name : string?                          
```

Delete a vault’s notification configuration.

## 14.4. Archives

```racket
(create-archive vault-name                    
                archive-description           
                data)               -> string?
  vault-name : string?                        
  archive-description : string?               
  data : bytes?                               
```

[Create an
archive](http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-archive-post.html)
containing the `data` and return its archive ID.

```racket
(create-archive-from-file vault-name path) -> string?
  vault-name : string?                               
  path : path?                                       
```

[Create an
archive](http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-archive-post.html)
with data from a file and return its archive ID.

```racket
(delete-archive vault-name archive-id) -> (or/c #t exn:fail:aws?)
  vault-name : string?                                           
  archive-id : string?                                           
```

Delete an archive.

## 14.5. Retrieval jobs

```racket
(retrieve-inventory  vault-name                
                     job-description           
                    [sns-topic])     -> string?
  vault-name : string?                         
  job-description : string?                    
  sns-topic : (or/c string? #f) = #f           
```

[Initiate a
job](http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-initiate-job-post.html)
to retrieve an archive’s inventory, and return the job ID.

```racket
(retrieve-archive  vault-name                
                   job-description           
                  [sns-topic])     -> string?
  vault-name : string?                       
  job-description : string?                  
  sns-topic : (or/c string? #f) = #f         
```

[Initiate a
job](http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-initiate-job-post.html)
to retrieve an archive’s data, and return the job ID.

```racket
(list-jobs vault-name) -> jsexpr?
  vault-name : string?           
```

[List
jobs](http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-jobs-get.html).

```racket
(get-job-output vault-name job-id) -> (or/c jsexpr? bytes?)
  vault-name : string?                                     
  job-id : string?                                         
```

[Get the output of a
job](http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-job-output-get.html).
If the `Content-Type` of the response is `application/json`, return the
result as a `jsexpr?`, otherwise return it as `bytes?`.

```racket
(get-output-job-to-file vault-name                                           
                        job-id                                               
                        path                                                 
                        exists)    -> boolean?                               
  vault-name : string?                                                       
  job-id : string?                                                           
  path : path?                                                               
  exists : (or/c 'error 'append 'update 'replace 'truncate 'truncate/replace)
```

[Get the output of an archive retrieval
job](http://docs.amazonwebservices.com/amazonglacier/latest/dev/api-job-output-get.html)
and put it in a file. Return a `boolean?` whether the output matches its
`x-amz-sha256-tree-hash`.

## 14.6. Example: Backup using Glacier and SDB

This example can be found in `examples/backup.rkt`.

```racket
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
```

# 15. Utilities

```racket
 (require aws/util) package: aws
```

Although the following are mainly for internal use, they’re `provide`d
in case you find them helpful.

```racket
(struct endpoint (host ssl?)                   
        #:extra-constructor-name make-endpoint)
  host : string?                               
  ssl? : boolean?                              
```

Used to represent an AWS service endpoint.

```racket
(dict->form-urlencoded dictionary) -> string?
  dictionary : dict?                         
```

Like `alist->form-urlencoded`, but `dictionary` is a Racket `dict`,
which includes but is not limited to an association list.

Also, and more importantly, the “percent encoding” is done using RFC
3986, which encodes some extra characters compared to RFC 2396.  Doing
so is important especially for SDB and its `Select` action: The SQL-like
statement contains characters like `#\*`, `#\(`, and `#\)`, which SDB
requires to be percent-encoded.

```racket
(tags xexpr tag [direct-child-of]) -> (listof xexpr?)
  xexpr : xexpr?                                     
  tag : symbol?                                      
  direct-child-of : (or/c #f symbol?) = #f           
```

Given `xexpr` return a list of all the elements starting with `tag`. If
`direct-child-of` is `#f`, it will return elements at any depth. It will
even return nested elements multiple times – once on their own, and once
within their parent.  If you only want elements that have a specific
immediate parent, set `direct-child-of` to that symbol.

Although there are more sophisticated and correct ways to make sense of
XML, this is useful when the XML structure is small and predictable, and
you care about extracting a few specific elements.

```racket
(first-tag-value xexpr tag [def]) -> string?
  xexpr : xexpr?                            
  tag : symbol?                             
  def : any/c = #f                          
```

Given `xexpr`, return the value of just the first element having tag
`tag`, or if none found, `def`.

# 16. Unit tests

The `rackunit` tests use the `test` submodule feature added in Racket
5.3. To run all tests, use the shell command, `raco test ./`.

Be aware that some of the tests make actual requests to AWS. They may
take awhile to complete. And to do so, the tests need some personally
identifying information from you, such email addresses, or names for
certain AWS objects that are safe to use for testing.

For example the tests for the `aws/s3` module try to `PUT` then `GET` an
object to S3—but what bucket and object path name should the tests use?
Only you can say. Similarly the `aws/ses` tests try to send actual
emails, and you need to supply suitable email addresses (including an
email address that is intentionally not “verified” with SES, to test its
response to that condition).

To supply this information, provide a `~/.aws-tests-data` file
containing various personally identifying information required to run
the tests.

Following is an example file, which you may also find in
`tests/example-dot-aws-tests-data`. In this example, suggested default
values are provided, whereas others are blank. You should supply or
review all of them to make sure they are suitable for you.

`# Supply a file in this format as ~/.aws-tests-data.`                 
`# You should change the values to those suitable for your AWS account`
`# and email addresses.`                                               
                                                                       
`# Name of a test bucket on S3. Do NOT use leading /`                  
`test/bucket = test-bucket`                                            
                                                                       
`# Name of a test resource to put/get/delete on S3. Do NOT use leading 
/`                                                                     
`test/path = path/to/test.txt`                                         
                                                                       
`# Email address you HAVE verified with SES.`                          
`test/verified-sender = <email-address>`                               
                                                                       
`# Email address you have NOT verified with SES.`                      
`test/unverified-sender = <email-address>`                             
                                                                       
`# Email recipient of som test emails`                                 
`test/recipient = <email-address>`                                     
                                                                       
`# Name of a test domain (i.e. "table") on SDB.`                       
`test/domain = TestDomain`                                             

# 17. License

Copyright (c) 2012, Greg Hendershott. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright notice,
  this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS
IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
