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

;;; --- Commentary

;; Overview:
;;
;; Fiplr is a fuzzy finder for files and this is an extension to that using the
;; contents of the .gitignore file found in the root of the project.
;;
;;   M-x fiplr-find-file-with-git-ignore
;;
;; Performs exactly as `fiplr-find-file' except that if a .gitignore file is
;; found in the project root the contents, along with the normal
;; `fiplr-ignored-globs', are used as the ignore globs when looking for files.

;;; --- Future Work

;; * `fiplr-find-directory-with-git-ignore'
;; * use global .gitignore too
;; * support negative globs from .gitignore

(eval-when-compile
  (require 'cl)
  (require 'fiplr))

;;; --- Support functions

(defun merge-alists (function default alist1 alist2)
  (flet ((keys (alist) (mapcar #'car alist))
         (lookup (key alist) (or (cdr (assoc key alist)) default)))
    (loop with keys = (union (keys alist1) (keys alist2) :test 'equal)
          for k in keys collect
          (cons k (funcall function (lookup k alist1) (lookup k alist2))))))

(defun fiplr-suggested-ignore-globs (globs)
  (merge-alists
   (lambda (a b) (list (append (car a) (car b))))
   '()
   fiplr-ignored-globs
   (list (list 'files globs) (list 'directories globs))))

(defun fiplr-ignored-globs-from-file (path)
  (if (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (split-string (buffer-string) "\n" t))
    ()))


;;; --- Public Functions

;;;###autoload
(defun fiplr-find-file-with-gitignore ()
  "Opens fiplr so that it is using the .gitignore information if it is present."
  (interactive)
  (let* ((root-path        (fiplr-root))
         (git-ignore-globs (fiplr-ignored-globs-from-file (concat root-path ".gitignore")))
         (suggested-globs  (fiplr-suggested-ignore-globs git-ignore-globs)))
    (fiplr-find-file-in-directory root-path suggested-globs)))

(provide 'fiplr-git)

;;; fiplr-git.el ends here
