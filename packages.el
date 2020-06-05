;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

(package! org-ref)
(package! org-drill)
(package! ivy-bibtex)
(package! counsel-org-clock)
(package! define-word)
(package! synosaurus)
(package! wiki-summary)
(package! mw-thesaurus)
(package! evil-matchit)
(package! scad-mode)
(package! matlab-mode)
(package! flycheck-julia)
(package! ripgrep)
(package! function-args)
(package! wordnut)
(package! ebib)
(package! org-ref-ox-hugo :recipe
  (:host github :repo "jethrokuan/org-ref-ox-hugo" :branch "custom/overrides"))


(package! evil-matchit-matlab :recipe
  (:local-repo "~/.doom.d/evil-matchit-matlab"))

(package! evil-snipe :disable t)
