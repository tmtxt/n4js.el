;;; dependencies: cypher-mode

;;; emacs neo4j shell
(require 'cypher-mode)
(defvar n4js-cli-program "neo4j-shell"
  "The cli program to start neo4j shell")

(defvar n4js-cli-arguments '()
  "List of command line arguments to pass to neo4j shell cli programm")

(defvar n4js-font-lock-keywords cypher-font-lock-keywords
  "Font lock keywords list, default is to taken from cypher-mode")

(defvar n4js-pop-to-buffer nil
  "Whether to pop up the neo4j shell buffer after sending command to execute")

(defvar n4js-pop-to-buffer-function 'pop-to-buffer
  "The function to pop up the neo4j shell buffer")

(define-derived-mode neo4j-shell-mode comint-mode "Neo4j Shell"
  "Major mode for `n4js-start'."
  ;; not allow the prompt to be deleted
  (setq comint-prompt-read-only t)
  ;; font lock keywords
  (set (make-local-variable 'font-lock-defaults) '(n4js-font-lock-keywords t)))

(defun n4js-pop-to-buffer ()
  "Pop the neo4j shell buffer to the current window"
  (apply n4js-pop-to-buffer-function '("*neo4j-shell*")))

;;; Taken from masteringemacs with some changes
;;; https://www.masteringemacs.org/article/comint-writing-command-interpreter
(defun n4js-start ()
  "Start neo4j shell comint mode"
  (interactive)
  (let ((buffer (comint-check-proc "*neo4j-shell*")))
    ;; pop to the "*neo4j-shell*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'neo4j-shell-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create "*neo4j-shell*")
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "neo4j-shell" nil n4js-cli-program nil
             n4js-cli-arguments)
      (neo4j-shell-mode))))

;;; Send the query string to neo4j shell to execute
(defun n4js-send-string (string)
  "Send the input string to neo4j shell process"
  (if (not (comint-check-proc "*neo4j-shell*"))
      (message "No neo4j shell process started")
    (progn
      (process-send-string "*neo4j-shell*" (concat string "\n"))
      (when n4js-pop-to-buffer
        (n4js-pop-to-buffer)))))

(defun n4js-send-region (beg end)
  "Send the region from beg to end to neo4j process"
  (let ((string (buffer-substring-no-properties beg end)))
    (n4js-send-string string)))

(defun n4js-send-current-region ()
  "Send the selected region to neo4j shell process"
  (interactive)
  (let* ((beg (region-beginning))
         (end (region-end)))
    (n4js-send-region beg end)))

(defun n4js-send-buffer ()
  "Send the current buffer to neo4j shell process"
  (interactive)
  (let* ((beg (point-min))
         (end (point-max)))
    (n4js-send-region beg end)))

(defun n4js-send-paragraph ()
  "Send the current paragraph to neo4j shell process"
  (interactive)
  (let ((beg (save-excursion
               (backward-paragraph)
               (point)))
        (end (save-excursion
               (forward-paragraph)
               (point))))
    (n4js-send-region beg end)))

(defun n4js-send-region-or-buffer ()
  "Send the selected region if presented, otherwise, send the whole buffer"
  (interactive)
  (if (use-region-p)
      (n4js-send-current-region)
    (n4js-send-buffer)))

(defun n4js-send-dwim ()
  "Send the selected region presented, otherwise, send the current paragraph"
  (interactive)
  (if (use-region-p)
      (n4js-send-current-region)
    (n4js-send-paragraph)))

(defun n4js-switch-to-buffer ()
  "Switch to neo4j shell buffer"
  (interactive)
  (if (comint-check-proc "*neo4j-shell*")
      (switch-to-buffer "*neo4j-shell*")
    (n4js-start)))

(provide 'n4js)
