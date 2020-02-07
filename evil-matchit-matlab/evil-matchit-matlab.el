;;; ~/.doom.d/extras/+evilmi-matlab.el -*- lexical-binding: t; -*-
;; detect tag in current line and return the result in variable rlt
;; the rlt will be used by evilmi-matlab-jump as the first parameter.
;; if NO tag found, the rlt SHOULD be nil
;;
;; @return the data to be used by evilmi-matlab-jump which should be a list
;;         the first element of the list is the position of cursor before jump
;;         we use it to select/delete tag. The other elements of the list could
;;         be any data type

(require 'evil-matchit-sdk)

(defvar evilmi-matlab-match-tags
  '(("if" ("elseif" "else") "end")
    (("function" "for" "while" "methods" "classdef" "properties") () "end")
    ("switch" ("case" "otherwise") "end")
    ("try" "catch" "end")))

;;;###autoload
(defun evilmi-matlab-get-tag ()
  (evilmi-sdk-get-tag evilmi-matlab-match-tags
                      evilmi-sdk-extract-keyword-howtos))

;;;###autoload
(defun evilmi-matlab-jump (rlt NUM)
  (evilmi-sdk-jump rlt
                   NUM
                   evilmi-matlab-match-tags
                   evilmi-sdk-extract-keyword-howtos))

(provide 'evil-matchit-matlab)
