#lang racket
(require racket/runtime-path)
(define-runtime-path tests "tests")

(define num-workers (processor-count))
(command-line
 #:once-each
 [("-j") num "number of parallel workers" 
         (begin
           (unless (string->number num)
             (raise-user-error 'run-tests.rkt "expected a number, got ~a" num))
           (set! num-workers (string->number num)))])

(define krun 
  (or (find-executable-path "krun")
      "/Users/robby/unison/proj/courses/395-2012-fall/k/k-latest/bin/krun"))

(unless (file-exists? krun) (error 'run-tests.rkt "could not find krun executable"))

(define (main)
  (define test-threads
    (filter
     thread?
     (for/list ([file (in-directory tests)])
       (when (regexp-match #rx"[.]imp$" (path->string file))
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
                                          krun "--no-config" (format "~a" in-file)))
              (define proc (list-ref lst 4))
              (define done-chan (make-channel))
              (thread (λ () (proc 'wait) (channel-put done-chan #t)))
              (define didnt-timeout (sync/timeout 30 done-chan))
              (unless didnt-timeout (proc 'kill) (proc 'wait))
              (channel-put resp-chan
                           (list (not didnt-timeout)
                                 (get-output-string outp)
                                 (get-output-string errp)))
              (loop)]))
          (handle-evt die-chan void)))))
    die-chan))
             
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
     (unless (ormap (λ (x) (equal? x stdout)) result-candidates)
       (printf "~a failed\n       got ~s\n" in-file stdout)
       (for ([result (in-list result-candidates)])
         (printf "  expected ~s\n" result)))]))
  
(main)
