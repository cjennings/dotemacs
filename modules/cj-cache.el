;;; cj-cache.el --- Generic TTL cache with build-guard -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Generic "rebuild a long-running computation behind a TTL" cache,
;; with a building-flag guard that prevents duplicate concurrent
;; rebuilds in async-build scenarios.
;;
;; Used by org-agenda-config and org-refile-config which previously
;; carried parallel hand-rolled implementations of this exact shape.
;; See docs/design/cache-helper-design.org for the API contract,
;; consumer migration shape, and rationale for the deliberate "nil
;; cached value reads as invalid" decision.
;;
;; Out of scope: buffer-local key-based caches like the modeline VC
;; cache (different lifecycle).

;;; Code:

(require 'cl-lib)

(defun cj/cache-make (&rest plist)
  "Return a fresh cache state.
PLIST keywords:
- =:ttl= seconds to retain a built value (default 3600)."
  (let ((ttl (or (plist-get plist :ttl) 3600)))
    (list :value nil :time nil :ttl ttl :building nil)))

(defun cj/cache-valid-p (cache)
  "Return non-nil when CACHE has a fresh, non-nil value within its TTL.
A nil cached value reads as invalid by design -- a build that legitimately
returns nil rebuilds on the next request, matching the prior agenda/refile
contract."
  (let ((value (plist-get cache :value))
        (time (plist-get cache :time))
        (ttl (plist-get cache :ttl)))
    (and value
         time
         (< (- (float-time) time) ttl))))

(defun cj/cache-building-p (cache)
  "Return non-nil when a build is currently in progress on CACHE."
  (plist-get cache :building))

(defun cj/cache-invalidate (cache)
  "Clear CACHE's value and timestamp.  TTL is preserved."
  (plist-put cache :value nil)
  (plist-put cache :time nil))

(cl-defun cj/cache-value-or-rebuild (cache build-fn
                                           &key force-rebuild
                                           on-hit
                                           on-build-start
                                           on-build-success
                                           on-build-error)
  "Return CACHE's value, calling BUILD-FN to rebuild when invalid.

When CACHE is valid and FORCE-REBUILD is nil, return the stored value
and call ON-HIT (if given) with the value.  Otherwise call BUILD-FN,
store its result, and return it.

The four callbacks let the consumer log without this helper printing on
its behalf:
- ON-HIT (value)
- ON-BUILD-START ()
- ON-BUILD-SUCCESS (value)
- ON-BUILD-ERROR (err)

The :building flag is set before BUILD-FN runs and cleared inside an
`unwind-protect' regardless of outcome.  Errors from BUILD-FN are
rethrown after ON-BUILD-ERROR fires."
  (cond
   ((and (not force-rebuild) (cj/cache-valid-p cache))
    (let ((value (plist-get cache :value)))
      (when on-hit (funcall on-hit value))
      value))
   (t
    (when on-build-start (funcall on-build-start))
    (plist-put cache :building t)
    (unwind-protect
        (condition-case err
            (let ((value (funcall build-fn)))
              (plist-put cache :value value)
              (plist-put cache :time (float-time))
              (when on-build-success (funcall on-build-success value))
              value)
          (error
           (when on-build-error (funcall on-build-error err))
           (signal (car err) (cdr err))))
      (plist-put cache :building nil)))))

(provide 'cj-cache)
;;; cj-cache.el ends here
