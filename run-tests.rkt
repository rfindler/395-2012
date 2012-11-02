#lang racket
(require racket/runtime-path)
(define-runtime-path tests "tests")

(define num-workers (processor-count))
(define do-search? #t)

(command-line
 #:once-each
 [("-n" "--no-search") "do not enumerate all possibilities" (set! do-search? #f)]
 [("-j") num "number of parallel workers" 
         (begin
           (unless (string->number num)
             (raise-user-error 'run-tests.rkt "expected a number, got ~a" num))
           (set! num-workers (string->number num)))])

(define krun 
  (or (find-executable-path "krun")
      "k/bin/krun"))

(unless (file-exists? krun) (error 'run-tests.rkt "could not find krun executable"))

(define (main)
  (define test-threads
    (filter
     thread?
     (for/list ([file (in-directory tests)])
       (when (regexp-match #rx"[.]js$" (path->string file))
         (define out-files (find-out-files file))
         (cond
           [(null? out-files)
            (eprintf "expected to find at least one out file for ~a\n" file)]
           [else
            (run-a-test file out-files)])))))
  (catch-results/io test-threads))

(define (find-out-files in-file)
  (define-values (base name dir) (split-path in-file))
  ;; out files must not have a ~ in the suffix part of the name
  (define prefix-reg
    (regexp (string-append 
             "^" 
             (regexp-quote (regexp-replace #rx"imp$" (path->string name) "out"))
             "[^~]*$")))
  (filter
   values
   (for/list ([file (in-list (directory-list base))])
     (and (regexp-match? prefix-reg (path->string file))
          (build-path base file)))))

(define job-chan (make-channel))
(define results-chan (make-channel))
(define print-chan (make-channel))

(define die-chans
  (for/list ([i (in-range num-workers)])
    (define die-chan (make-channel))
    (thread
     (λ ()
       (let loop ()
         (sync
          (handle-evt
           job-chan
           (match-lambda
             [(list in-file out-file resp-chan)
              (define outp (open-output-string))
              (define errp (open-output-string))
              (channel-put print-chan (format "running ~a ...\n" in-file))
              (define lst (process*/ports outp (open-input-string "") errp 
                                          krun "--no-deleteTempDir" 
                                          (if do-search? "--search" "--no-config")
                                          (format "~a" in-file)))
              (define proc (list-ref lst 4))
              (define done-chan (make-channel))
              (thread (λ () (proc 'wait) (channel-put done-chan #t)))
              (define didnt-timeout (sync/timeout 30 done-chan))
              (unless didnt-timeout (proc 'kill) (proc 'wait))
              (channel-put resp-chan
                           (list (not didnt-timeout)
                                 (if do-search?
                                     (parse-io (get-output-string outp))
                                     (get-output-string outp))
                                 (get-output-string errp)))
              (loop)]))
          (handle-evt die-chan void)))))
    die-chan))
             
(define (parse-io str)
  (define sp (open-input-string str))
  (let loop ()
    (define next (regexp-match #rx"\nSolution [0-9]+, state [0-9]+:\n" sp))
    (cond
      [(not next) '()]
      [else
       (define m (regexp-match #rx"<out>\n *#buffer[(] *\"" sp))
       (define output (parse-to-close-quote sp))
       (cons output (loop))])))

(define (parse-to-close-quote sp)
  (apply
   string
   (let loop ([escaping? #f])
     (define c (read-char sp))
     (cond
       [(eof-object? c) (error 'parse-io "found eof in the middle of the buffer")]
       [else
        (define the-char
          (if escaping?
              (case c
                [(#\") #\"]
                [(#\\) #\\]
                [(#\n) #\newline]
                [else (error 'parse-io "unknown escape char: ~s" c)])
              (case c
                [(#\\) 'escape]
                [(#\") #f]
                [else c])))
        (cond
          [(eq? the-char 'escape)
           (loop #t)]
          [(not the-char)
           '()]
          [else
           (cons the-char (loop #f))])]))))



;; you may have to comment out the submodule if you're using an older version of Racket
#;
(module+ test
  (require rackunit)
  (check-equal? (parse-to-close-quote (open-input-string "1 1 2 3 5 8 13 21\\n\""))
                "1 1 2 3 5 8 13 21\n")
  (check-equal?
   (parse-io
    (string-append
     "Search results:\n\nSolution 1, state 90:\n"
     "<C> \n  <allocptr>\n   3 \n  </allocptr> \n"
     "  <in>\n   #buffer( \"null\\n\" )\n  </in> \n"
     "  <out>\n   #buffer( \"1 1 2 3 5 8 13 21\\n\" )\n"
     "  </out> \n  <threads>\n   .\n  </threads> \n"
     "  <store>   \n    0 |-> 21\n    1 |-> 34\n    2 |-> 1\n  </store> \n"
     "</C>\n"))
   '("1 1 2 3 5 8 13 21\n")))


(define (catch-results/io thds)
  (let loop ([thds thds])
    (cond
      [(null? thds) 
       (printf "~a tests run\n" num-tests)]
      [else
       (apply
        sync
        (handle-evt
         print-chan
         (λ (str) (display str) (loop thds)))
        (handle-evt 
         results-chan
         (λ (results) (show-results results) (loop thds)))
        (map (λ (thd)
               (handle-evt thd (λ (_) (loop (remq thd thds)))))
             thds))])))
    
(define (run-a-test in-file out-files)
  (thread
   (λ ()
     (define resp-chan (make-channel))
     (channel-put job-chan (list in-file out-files resp-chan))
     (define actual-answer (channel-get resp-chan))
     (channel-put results-chan (list* in-file out-files actual-answer)))))

(define num-tests 0)

(define (show-results lst)
  (set! num-tests (+ num-tests 1))
  (define-values (in-file out-files timedout? stdout stderr) (apply values lst))
  (cond
    [timedout?
     (printf "~a timed out\n" in-file)]
    [(not (equal? "" stderr))
     (printf "~a has stderr output:\n----------\n~a\n----------\n" in-file stderr)]
    [else
     (define result-candidates 
       (for/list ([out-file (in-list out-files)])
         (define sp (open-output-string))
         (call-with-input-file out-file (λ (port) (copy-port port sp)))
         (get-output-string sp)))
     (cond
       [do-search? 
        (define failed? #f)
        (define (show-failed)
          (unless failed?
            (set! failed? #t)
            (printf "~a failed\n" in-file)))
        (for ([got (in-list stdout)])
          (unless (member got result-candidates)
            (show-failed)
            (printf "       got ~s, but didn't expect it\n" got)))
        (for ([expected (in-list result-candidates)])
          (unless (member expected stdout)
            (show-failed)
            (printf "  expected ~s, but didn't get it\n" expected)))]
       [else
        (unless (ormap (λ (x) (equal? x stdout)) result-candidates)
          (printf "~a failed\n       got ~s\n" in-file stdout)
          (for ([result (in-list result-candidates)])
            (printf "  expected ~s\n" result)))])]))
  
(main)
