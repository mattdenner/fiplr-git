;;; fiplr-git.el --- Extension to fiplr to use .gitignore for globs

;; Copyright © 2013 Chris Corbyn
;;
;; Author: Matthew Denner <matt.denner@gmail.com>
;; URL: https://github.com/mattdenner/fiplr
;; Version: 0.0.1
;; Keywords: convenience, usability, project, git

;; This file is NOT part of GNU Emacs.

;;; --- License

;;  Licensed under the same terms as Emacs.

(define-package "fiplr-git" "0.0.1"
  "Extension to fiplr to support gitignore globs"
  '((fiplr "0.2.6") (cl-lib "0.1")))
