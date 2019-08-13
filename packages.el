;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;; (package! matlab-mode :recipe (:fetcher github :repo "yuhonglin/matlab-mode"))
(package! matlab-mode
  :recipe (:fetcher git
           :url "https://git.code.sf.net/p/matlab-emacs/src"
           :files ("*.el" "*.m"
                   ("toolbox" "toolbox/*.m")
                   ("templates" "templates/*.srt"))))
