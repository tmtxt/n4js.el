;;; n4js.el --- Neo4j Shell

;;; Copyright (C) 2015 TruongTx

;;; Author: TruongTx <me@truongtx.me>
;;; Version: 0.1
;;; URL: https://github.com/tmtxt/n4js.el
;;; Package-Requires: ((emacs "24") (cypher-mode "0"))
;;; Keywords: neo4j, shell, comint

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

;; Put this file in your Emacs Lisp path (e.g. ~/.emacs.d/site-lisp)
;; and add the following line to your .emacs:

;; (require 'n4js)

;; Type M-x n4js-start to run Neo4j Shell.
;; See also `comint-mode' to check key bindings.

(require 'comint)
(require 'cypher-mode)

;;; Code:
(defvar n4js-cli-program "cypher-shell"
  "The cli program to start neo4j cypher shell.")

(defvar n4js-username "neo4j")

(defvar n4js-password t
  "Neo4j password for ‘n4js-username’.
If t, password will be read interactively. If nil, do not use
password.")

(defvar n4js-cli-other-arguments nil
  "List of command line arguments to pass to neo4j cypher shell cli program.")

(defvar n4js-font-lock-keywords cypher-font-lock-keywords
  "Font lock keywords list, default is to taken from cypher-mode.")

(defvar n4js-pop-to-buffer nil
  "Whether to pop up the neo4j shell buffer after sending command to execute.")

(defvar n4js-pop-to-buffer-function 'pop-to-buffer
  "The function to pop up the neo4j shell buffer.")

(defvar n4js-buffer-name "*cypher-shell*"
  "Buffer name for cypher shell.")

(defvar n4js-process-name "neo4j-cypher-shell"
  "Buffer name for cypher shell.")

(defvar n4js-switch-to-buffer-key (kbd "C-c c-s")
  "Key that calls ‘n4js-switch-to-buffer’ in the buffer where ‘n4js-start’ was invoked.")

(define-derived-mode n4js-mode comint-mode "Neo4j Cypher Shell"
  "Major mode for `n4js-start'."
  ;; not allow the prompt to be deleted
  (setq comint-prompt-read-only t)
  (define-key n4js-mode-map (kbd "<RET>") #'n4js-send-input)
  ;; font lock keywords
  (set (make-local-variable 'font-lock-defaults) '(n4js-font-lock-keywords t)))

(defun n4js-pop-to-buffer ()
  "Pop the neo4j cypher shell buffer to the current window."
  (apply n4js-pop-to-buffer-function (list n4js-buffer-name)))

(defun n4js-send-input (&optional no-newline artificial)
  "Send input to cypher-shell process.

It checks if the cypher statement is over, and if so it sends it
to the process. If not, it accumulates the input, adding a
newline. The arguments NO-NEWLINE and ARTIFICIAL are passed on to
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
  ;; this'll set the command in all buffers with the same major mode,
  ;; but I think that's okay
  (local-set-key n4js-switch-to-buffer-key #'n4js-switch-to-buffer)
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
      (let ((auth-args (when password
			 (list "-u" (or n4js-username "neo4j") "-p" password))))
	(apply 'make-comint-in-buffer n4js-process-name n4js-buffer-name n4js-cli-program
	       nil
	       (append auth-args n4js-cli-other-arguments)))
      (n4js-mode))))

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
