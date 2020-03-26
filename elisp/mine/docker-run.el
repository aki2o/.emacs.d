;;; docker-run.el --- Support to execute on docker container

;; Copyright (C) 2018  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: convenience docker
;; URL: https://github.com/aki2o/emacs-docker-run
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; For more infomation, see <https://github.com/aki2o/emacs-docker-run/blob/master/README.md>

;;; Dependencies:
;; 

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;; 
;; (require 'docker-run)

;;; Configuration:
;; 
;; Nothing

;;; Customization:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "docker-run:" :docstring t)
;; 
;;  *** END auto-documentation

;;; API:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'macro :prefix "docker-run:" :docstring t)
;; 
;;  *** END auto-documentation
;; [EVAL] (autodoc-document-lisp-buffer :type 'function :prefix "docker-run:" :docstring t)
;; 
;;  *** END auto-documentation
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "docker-run:" :docstring t)
;; 
;;  *** END auto-documentation
;; [Note] Functions and variables other than listed above, Those specifications may be changed without notice.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 24.5.1 (x86_64-apple-darwin14.5.0, NS apple-appkit-1348.17) of 2016-06-16 on 192.168.102.190
;; - docker ... Docker version 1.12.6, build 78d1802


;; Enjoy!!!


;;; Code:
(eval-when-compile (require 'cl))
(require 'viassh nil t)

(defgroup docker-run nil
  "Support to execute on docker container."
  :group 'convenience
  :prefix "docker-run:")

(defcustom docker-run:project-cache-file (concat user-emacs-directory ".docker-run-project")
  "Filepath stores project configuration."
  :type 'string
  :group 'docker-run)

(defcustom docker-run:project-root-detect-function 'projectile-project-root
  "Function detect project root path for `current-buffer'."
  :type 'symbol
  :group 'docker-run)


(defvar docker-run:project-root nil)
(defvar docker-run:project-cache-hash nil)


(defun docker-run::project-cached-value (cache-name)
  (when docker-run:project-root
    (plist-get (docker-run::project-cache)
               cache-name)))

(defun docker-run::project-cache ()
  (gethash docker-run:project-root (docker-run::ensure-project-cache-hash)))

(defun docker-run::project-root-on (buf)
  (or (with-current-buffer buf
        (funcall docker-run:project-root-detect-function))
      (error "Can't detect project root path for %s" (buffer-file-name buf))))

(defun docker-run::ensure-project-cache-hash ()
  (or docker-run:project-cache-hash
      (setq docker-run:project-cache-hash
            (or (docker-run::load-project-cache-hash)
                (make-hash-table :test 'equal)))))

(defun docker-run::load-project-cache-hash ()
  (when (file-exists-p docker-run:project-cache-file)
    (read (with-temp-buffer
            (insert-file-contents docker-run:project-cache-file)
            (buffer-string)))))

(defun* docker-run::store-project-cache (&key container)
  (puthash docker-run:project-root `(:container ,container) (docker-run::ensure-project-cache-hash))
  (with-temp-buffer
    (insert (prin1-to-string (docker-run::ensure-project-cache-hash)))
    (write-file docker-run:project-cache-file)))

(defun* docker-run::select-container (&key (use-cache t))
  (or (when use-cache (docker-run::project-cached-value :container))
      (let* ((cmd "ps --format='{{.Names}} {{.ID}}'")
             (res (docker-run::call-docker-command 'shell-command-to-string cmd))
             (container-id-alist (loop for line in (split-string res "\n")
                                       if (> (length line) 1)
                                       collect (apply 'cons (split-string line " "))))
             (container-names (mapc 'car container-id-alist))
             (container-name (completing-read "select container: " container-names nil t nil '())))
        (assoc-default container-name container-id-alist))))

(defun docker-run::call-docker-command (func cmd)
  (if (and (not (executable-find "docker"))
           (not (viassh-p)))
      (error "Not found 'docker' command. You sure docker has been installed.")
    (funcall func (format "docker %s" cmd))))


;;;;;;;;;;;;;;;;;;;
;; User Function

;;;###autoload
(defun docker-run:exec (func cmd)
  (let* ((docker-run:project-root (or docker-run:project-root
                                      (docker-run::project-root-on (current-buffer))))
         (container (docker-run::select-container)))
    (when (not (docker-run::project-cache))
      (docker-run::store-project-cache :container container))
    (docker-run::call-docker-command func (format "exec -t %s %s" container cmd))))

;;;###autoload
(defun docker-run:exec-viassh (func cmd)
  (viassh (docker-run:exec func cmd)))

;;;###autoload
(defun docker-run:configure-current-project ()
  (interactive)
  (let* ((docker-run:project-root (docker-run::project-root-on (current-buffer)))
         (container (docker-run::select-container :use-cache nil)))
    (docker-run::store-project-cache :container container)))

;;;###autoload
(defun docker-run:configure-current-project-viassh ()
  (interactive)
  (viassh (docker-run:configure-current-project)))


(provide 'docker-run)
;;; docker-run.el ends here
