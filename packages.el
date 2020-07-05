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
(package! lsp-julia :recipe (:host github :repo "non-jedi/lsp-julia"))
(package! ripgrep)
(package! multi-vterm)
(package! function-args)
(package! wordnut)
(package! ebib)
(package! sed-mode)
(package! spell-fu)
(package! pdfgrep)
(package! spacemacs-theme)
(package! evil-smartparens)
(package! nov)
;; (package! exwm)

(package! org-ref-ox-hugo :recipe
  (:host github :repo "jethrokuan/org-ref-ox-hugo" :branch "custom/overrides"))

(package! flycheck-textlint :recipe
  (:host github :repo "kisaragi-hiu/flycheck-textlint"))

(package! evil-matchit-matlab :recipe
  (:local-repo "~/.doom.d/evil-matchit-matlab"))

(package! evil-snipe :disable t)
