;;; clojure-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (clojure-mode) "clojure-mode" "clojure-mode.el"
;;;;;;  (21898 50086 295843 535000))
;;; Generated autoloads from clojure-mode.el

(autoload 'clojure-mode "clojure-mode" "\
Major mode for editing Clojure code.

\\{clojure-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.\\(clj[csx]?\\|dtm\\|edn\\)\\'" . clojure-mode))

(add-to-list 'auto-mode-alist '("\\`build.boot\\'" . clojure-mode))

;;;***

;;;### (autoloads nil nil ("clojure-mode-pkg.el") (21898 50086 397492
;;;;;;  425000))

;;;***

(provide 'clojure-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; clojure-mode-autoloads.el ends here
