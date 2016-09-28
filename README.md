# bazon-thread-pool - Common Lisp thread pooling library.

## Overview

Creating and destroying a thread and its associated resources is an expensive 
process in terms of time. bazon-thread-pool intended to negate overhead of 
this processes.

## Examples

### Using fixed thread pool

    (asdf:load-system :bazon-thread-pool)
    (defvar *fixed-pool* (bazon-thread-pool:make-fixed-thread-pool "example" :size 5))
    (bazon-thread-pool:start-pool *fixed-pool*)
    (bazon-thread-pool:execute *fixed-pool* #'(lambda () (format t "From worker~%")))
    (bazon-thread-pool:stop-pool *fixed-pool*)

### Using cached thread pool

Sometimes it a good idea to create additional threads and reuse them on high
load and terminate them on low load freeing resources. That approach realized 
with cached thread pool. keep-alive-time determines idle time to live of pool 
worker. 

    (asdf:load-system :bazon-thread-pool)
    (defvar *cached-pool* (bazon-thread-pool:make-cached-thread-pool "example" :size 2 :max-size 4 :keep-alive-time 2))
    (bazon-thread-pool:start-pool *cached-pool*)
    (bazon-thread-pool:execute *cached-pool* #'(lambda () (format t "From worker1~%")))
    (bazon-thread-pool:execute *cached-pool* #'(lambda () (format t "From worker2~%")))
    (bazon-thread-pool:execute *cached-pool* #'(lambda () (format t "From worker3~%")))
    (bazon-thread-pool:execute *cached-pool* #'(lambda () (format t "From worker4~%")))
    (bazon-thread-pool:execute *cached-pool* #'(lambda () (format t "From worker5~%")))
    (sleep 5)
    (bazon-thread-pool:execute *cached-pool* #'(lambda () (format t "From worker6~%")))
    (bazon-thread-pool:stop-pool *cached-pool*)
 
