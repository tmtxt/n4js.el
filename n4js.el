;;; n4js.el --- Neo4j Shell

;;; Copyright (C) 2020 TruongTx

;;; Author: TruongTx <me@truongtx.me>
;;; Maintainer: odanoburu <bcclaro+emacs@gmail.com>
;;; Version: 0.2
;;; URL: https://github.com/odanoburu/n4js.el
;;; Package-Requires: ((emacs "25.1"))
;;; Keywords: languages, terminals

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a comint mode for the cypher shell, a client used to
;; communicate with the Neo4j database.  ‘n4js’ should be compatible with any other
;; shell that uses the BOLT protocol and the Cypher query language, such as the
;; Memgraph client.

;; To install ‘n4js’ manually, put this file in your Emacs Lisp load path (see the
;; ‘load-path’ variable) and add the following line to your configuration file:

;; (require 'n4js)

;; You may also install it from MELPA, and employ ‘use-package’ to configure it:

;; (use-package n4js
;;   :custom (n4js-user "neo4j")
;;           (n4js-address "bolt://example.com:7867"))

;; Type M-x n4js-start to run the cypher shell on a comint buffer.  See also
;; `comint-mode' to see key bindings available to all comint buffers.

(require 'comint)

;;; Code:
(defgroup n4js nil
  "Customization group for cypher-shell (part of Neo4j)."
  :prefix 'n4js-
  :group 'Applications)

(defcustom n4js-cli-program "cypher-shell"
  "The CLI program to start Neo4j cypher shell."
  :type 'string)

(defcustom n4js-address "bolt://localhost:7687"
  "Address and port to connect to."
  :type 'string)

(defcustom n4js-username "neo4j"
  "The Neo4j username to use."
  :type 'string)

(defcustom n4js-password t
  "Neo4j password for ‘n4js-username’.

If t, password will be read interactively.  If nil, do not use
password."
  :type '(choice (const :tag "No password" nil)
		 (const :tag "Ask for password" t)
		 (string :tag "Password")))

(defcustom n4js-cli-arguments nil
  "List of command line arguments to pass to neo4j cypher shell cli program."
  :type '(repeat string))

(defvar n4js-font-lock-keywords (if (require 'cypher-mode nil t) cypher-font-lock-keywords)
  "Font lock keywords list, default is taken from `cypher-mode'.")

(defcustom n4js-pop-to-buffer nil
  "Whether to pop up the neo4j shell buffer after sending command to execute."
  :type 'boolean)

(defcustom n4js-pop-to-buffer-function 'pop-to-buffer
  "The function to pop up the neo4j shell buffer."
  :type 'function)

(defcustom n4js-buffer-name "*cypher-shell*"
  "Buffer name for cypher shell."
  :type 'string)

(defcustom n4js-process-name "neo4j-cypher-shell"
  "Buffer name for cypher shell."
  :type 'string)

(defcustom n4js-switch-to-buffer-key (kbd "C-c C-.")
  "Key that calls ‘n4js-switch-to-buffer’ in the buffer where ‘n4js-start’ was invoked."
  :type 'key-sequence)


(define-derived-mode n4js-mode comint-mode "Neo4j Cypher Shell"
  "Major mode for `n4js-start'."
  ;; not allow the prompt to be deleted
  (setq comint-prompt-read-only t)
  (define-key n4js-mode-map (kbd "<RET>") #'n4js-send-input)
  ;; font lock keywords
  (if n4js-font-lock-keywords
      (set (make-local-variable 'font-lock-defaults) '(n4js-font-lock-keywords t))))

(defun n4js-pop-to-buffer ()
  "Pop the neo4j cypher shell buffer to the current window."
  (apply n4js-pop-to-buffer-function (list n4js-buffer-name)))

(defun n4js-send-input (&optional no-newline artificial)
  "Send input to cypher-shell process.

It checks if the cypher statement is over, and if so it sends it
to the process.  If not, it accumulates the input, adding a
newline.  The arguments NO-NEWLINE and ARTIFICIAL are passed on to
‘comind-send-input’."
  (interactive)
  (if (prog2
	  (beginning-of-line)
	  (or (looking-at-p (rx (* anything) ";" (0+ space) line-end))
	      (looking-at-p (rx (0+ space) ":")))
	(end-of-line))
      (comint-send-input no-newline artificial)
    (comint-accumulate)))

;;; Taken from masteringemacs with some changes
;;; https://www.masteringemacs.org/article/comint-writing-command-interpreter
;;;###autoload
(defun n4js-start (password)
  "Start neo4j shell comint mode.

PASSWORD is read using ‘read-passwd’ id ‘n4js-password’ is non-nil."
  (interactive
   (list
    (cond
     ((stringp n4js-password)
      n4js-password)
     (n4js-password
      (read-passwd "Neo4j password: ")))))
  (message "Logging as ‘%s’ at ‘%s’" n4js-username n4js-address)
  ;; this'll set the command in all buffers with the same major mode,
  ;; but I think that's okay
  (local-set-key n4js-switch-to-buffer-key #'n4js-switch-to-buffer)
  ;; NOTE: we build the argument list to the cypher-shell here so that we can use
  ;; buffer-, file-, and directory-local variables to change addresses, usernames,
  ;; and passwords
  (let* ((auth-args (when password
		      (list "--username" n4js-username
			    "--password" password)))
	 (cli-args (append (list "--address" n4js-address)
			   auth-args
			   n4js-cli-arguments)))
    (let ((buffer (comint-check-proc n4js-buffer-name)))
      ;; pop to the `n4js-buffer-name' buffer if the process is dead, the
      ;; buffer is missing or it's got the wrong mode.
      (pop-to-buffer-same-window
       (if (or buffer (not (derived-mode-p 'n4js-mode))
               (comint-check-proc (current-buffer)))
           (get-buffer-create n4js-buffer-name)
	 (current-buffer)))
      ;; create the comint process if there is no buffer.
      (unless buffer
	(apply 'make-comint-in-buffer n4js-process-name n4js-buffer-name n4js-cli-program
	       nil
	       cli-args)
	(n4js-mode)))))

;;; Send the query string to neo4j shell to execute
(defun n4js-send-string (string)
  "Send the STRING to neo4j shell process."
  (interactive "sNeo4j query: ")
  (if (not (comint-check-proc n4js-buffer-name))
      (message "No neo4j shell process started")
    (progn
      (process-send-string n4js-process-name (concat string "\n"))
      (when n4js-pop-to-buffer
        (n4js-pop-to-buffer)))))

(defun n4js-send-region (beg end)
  "Send the region from BEG to END to neo4j process."
  (interactive "r")
  (let ((string (buffer-substring-no-properties beg end)))
    (n4js-send-string string)))

(defun n4js-send-current-region ()
  "Send the selected region to neo4j shell process."
  (interactive)
  (let* ((beg (region-beginning))
         (end (region-end)))
    (n4js-send-region beg end)))


(defun n4js-send-buffer ()
  "Send the current buffer to neo4j shell process."
  (interactive)
  (let* ((beg (point-min))
         (end (point-max)))
    (n4js-send-region beg end)))


(defun n4js-send-paragraph (beg end)
  "Send the current paragraph to neo4j shell process.

BEG and END delimit the paragraph to be sent."
  (interactive (list (save-excursion
		       (backward-paragraph)
		       (point))
		     (save-excursion
		       (forward-paragraph)
		       (point))))
  (n4js-send-region beg end))

(defun n4js-send-region-or-buffer ()
  "Send the selected region if presented, otherwise, send the whole buffer."
  (interactive)
  (if (use-region-p)
      (n4js-send-current-region)
    (n4js-send-buffer)))

(defun n4js-send-dwim ()
  "Send the selected region presented, otherwise, send the current paragraph."
  (interactive)
  (if (use-region-p)
      (n4js-send-current-region)
    (call-interactively #'n4js-send-paragraph)))

;;;###autoload
(defun n4js-switch-to-buffer ()
  "Switch to neo4j shell buffer."
  (interactive)
  (if (comint-check-proc n4js-buffer-name)
      (switch-to-buffer n4js-buffer-name)
    (call-interactively #'n4js-start)))


(provide 'n4js)

;;; n4js.el ends here
