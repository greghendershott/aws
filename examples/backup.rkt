#lang racket

;; Use Glacier for archival backups, and SDB to store the meta data.

(require (planet gh/aws/sdb)
         (planet gh/aws/sns)
         (planet gh/aws/glacier)
         (planet gh/http/request))      ;just for seconds->gmt-8601-string

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
                  `((ArchiveId ,archive-id)))
  ;; Also store some info about this specific archive.
  (put-attributes archive->meta-domain
                  archive-id
                  `((Size ,(number->string (file-size path)))
                    (Date ,(seconds->gmt-8601-string))
                    (Path ,path/string)))
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
