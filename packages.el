;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

(package! org-ref)
(package! ivy-bibtex)
(package! counsel-org-clock)
(package! define-word)
(package! powerthesaurus)
(package! mw-thesaurus)
(package! evil-matchit)
(package! scad-mode)
(package! matlab-mode)
(package! flycheck-julia)
(package! ripgrep)
(package! ace-link)
(package! function-args)
(package! wordnut)
(package! ebib)

(package! evil-matchit-matlab :recipe
  (:local-repo "~/.doom.d/evil-matchit-matlab"))

(package! evil-snipe :disable t)
