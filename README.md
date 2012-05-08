# enhanced-thread-pool - Common Lisp thread pooling library.

## Overview

Creating and destroying a thread and its associated resources is an expensive 
process in terms of time. enhanced-thread-pool intended to negate overhead of 
this processes.

## Examples

### Using fixed thread pool

    (asdf:load-system :enhanced-thread-pool)
    (defvar *fixed-pool* (enhanced-thread-pool:make-fixed-thread-pool "example" :size 5))
    (enhanced-thread-pool:start-pool *fixed-pool*)
    (enhanced-thread-pool:execute *fixed-pool* #'(lambda () (format t "From worker~%")))
    (enhanced-thread-pool:stop-pool *fixed-pool*)

### Using cached thread pool

Sometimes it a good idea to create additional threads and reuse them on high
load and terminate them on low load freeing resources. That approach realized 
with cached thread pool. keep-alive-time determines idle time to live of pool 
worker. 

    (asdf:load-system :enhanced-thread-pool)
    (defvar *cached-pool* (enhanced-thread-pool:make-cached-thread-pool "example" :size 2 :max-size 4 :keep-alive-time 2))
    (enhanced-thread-pool:start-pool *cached-pool*)
    (enhanced-thread-pool:execute *cached-pool* #'(lambda () (format t "From worker1~%")))
    (enhanced-thread-pool:execute *cached-pool* #'(lambda () (format t "From worker2~%")))
    (enhanced-thread-pool:execute *cached-pool* #'(lambda () (format t "From worker3~%")))
    (enhanced-thread-pool:execute *cached-pool* #'(lambda () (format t "From worker4~%")))
    (enhanced-thread-pool:execute *cached-pool* #'(lambda () (format t "From worker5~%")))
    (sleep 5)
    (enhanced-thread-pool:execute *cached-pool* #'(lambda () (format t "From worker6~%")))
    (enhanced-thread-pool:stop-pool *cached-pool*)
 
