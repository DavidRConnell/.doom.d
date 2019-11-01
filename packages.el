;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

(package! julia-repl :recipe
  (:fetcher github :repo "tpapp/julia-repl"))

(package! matlab-emacs :recipe
  (:fetcher github :repo "ayonga/matlab-emacs"))

(package! org-ref)
(package! org-noter)
(package! ivy-bibtex)
(package! counsel-org-clock)
(package! define-word)
(package! powerthesaurus)
