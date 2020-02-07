;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

(package! org-ref)
(package! org-noter)
(package! ivy-bibtex)
(package! counsel-org-clock)
(package! define-word)
(package! powerthesaurus)
(package! evil-matchit)
(package! scad-mode)
(package! matlab-mode)
(package! flycheck-julia)
(package! ripgrep)
(package! ace-link)
(package! function-args)

(package! evil-matchit-matlab :recipe
  (:local-repo "~/.doom.d/evil-matchit-matlab"))

(package! evil-snipe :disable t)
(package! writegood-mode :disable t)
