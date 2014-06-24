;;;; termite.import.scm - GENERATED BY CHICKEN 4.8.0.5 -*- Scheme -*-

(eval '(import
         chicken
         scheme
         srfi-1
         srfi-4
         srfi-18
         mailbox
         mailbox-threads
         tcp
         extras
         posix
         ports
         defstruct
         termite-match
         termite-recv
         termite-deftype
         (prefix lookup-table chicken:)
         (prefix s11n s11n:)
         srfi-4
         termite-match
         termite-recv
         termite-deftype))
(import chicken scheme)
(##core#begin
  (include "match-support.scm")
  (include "match.scm")
  (include "recv.scm")
  (include "deftype.scm"))
(##sys#register-compiled-module
  'termite
  (list)
  '((self . termite#self)
    (! . termite#!)
    (? . termite#?)
    (?? . termite#??)
    (!? . termite#!?)
    (on . termite#on)
    (make-node . termite#make-node)
    (spawn . termite#spawn)
    (pid? . termite#pid?)
    (spawn-link . termite#spawn-link)
    (remote-spawn . termite#remote-spawn)
    (remote-spawn-link . termite#remote-spawn-link)
    (make-tag . termite#make-tag)
    (current-node . termite#current-node)
    (inbound-link . termite#inbound-link)
    (outbound-link . termite#outbound-link)
    (full-link . termite#full-link)
    (spawn-output-port . termite#spawn-output-port)
    (spawn-input-port . termite#spawn-input-port)
    (termite-exception? . termite#termite-exception?)
    (migrate-task . termite#migrate-task)
    (migrate/proxy . termite#migrate/proxy)
    (warning . #%warning)
    (debug . termite#debug)
    (info . termite#info)
    (node-init . termite#node-init)
    (node? . termite#node?)
    (node-host . termite#node-host)
    (node-port . termite#node-port)
    (make-server-plugin . termite#make-server-plugin)
    (server:start . termite#server:start)
    (server:start-link . termite#server:start-link)
    (server:call . termite#server:call)
    (server:cast . termite#server:cast)
    (server:stop . termite#server:stop)
    (make-dict . termite#make-dict)
    (dict? . termite#dict?)
    (dict->list . termite#dict->list)
    (dict-for-each . termite#dict-for-each)
    (dict-search . termite#dict-search)
    (dict-set! . termite#dict-set!)
    (dict-ref . termite#dict-ref)
    (dict-length . termite#dict-length)
    (publish-service . termite#publish-service)
    (unpublish-service . termite#unpublish-service)
    (resolve-service . termite#resolve-service)
    (remote-service . termite#remote-service)
    (node1 . termite#node1)
    (node2 . termite#node2)
    (ping . termite#ping))
  (list)
  (list))

;; END OF FILE