;;; company-rg.el --- rg backend for company-mode  -*- lexical-binding: t -*-

;;; Code:

(eval-when-compile
  (require 'cl-macs))
(require 'company)

(defgroup company-rg ()
  "Rg company backend."
  :group 'company
  :prefix "company-rg-")

(defconst company-rg--rg-executable
  (if-let ((executable (executable-find "rg")))
      executable
    (warn "No rg executable found in PATH."))
  "The PATH of the `rg' executable.
A warning is issued if it can't be found on loading.")

;; TODO fix type so it can be nil (probably a choice)
(defcustom company-rg-default-directory nil
  "Default directory used when runing rg"
  :group 'company-rg
  :type '(string function))

(defvar-local company-rg--debounce-state nil)

(defun company-rg--prefix-to-string (prefix)
  "Return a string or nil from a prefix.
  `company-grab-symbol-cons' can return (\"prefix\" . t) or just
  \"prefix\", but we only care about the string."
  (if (consp prefix)
      (car prefix)
    prefix))

(defun company-rg--string-prefix-p (a b)
  (string-prefix-p (company-rg--prefix-to-string a) (company-rg--prefix-to-string b)))

(defun company-rg--prefix-to-string (prefix)
  "Return a string or nil from a prefix.
  `company-grab-symbol-cons' can return (\"prefix\" . t) or just
  \"prefix\", but we only care about the string."
  (if (consp prefix)
      (car prefix)
    prefix))

(defun company-rg--debounce-callback (prefix callback)
  (lambda (candidates)
    (let ((current-prefix (car company-rg--debounce-state))
          (current-callback (cdr company-rg--debounce-state)))
      (when (and current-prefix
                 (company-rg--string-prefix-p prefix current-prefix))
        (setq company-rg--debounce-state nil)
        (funcall current-callback (all-completions current-prefix candidates))))))

(defun company-rg--debounce-async (prefix candidate-fn)
  "Return a function that will properly debounce candidate queries by comparing the
in-flight query's prefix to PREFIX. CANDIDATE-FN should take two arguments, PREFIX
and the typical async callback.
Note that the candidate list provided to the callback by CANDIDATE-FN will be
filtered via `all-completions' with the most current prefix, so it is not necessary
to do this filtering in CANDIDATE-FN.
Use like:
  (cons :async (company-rg--debounce-async arg 'your-query-fn))"
  (lambda (callback)
    (let ((current-prefix (car company-rg--debounce-state)))
      (unless (and current-prefix
                   (company-rg--string-prefix-p prefix current-prefix))
        (funcall candidate-fn prefix (company-rg--debounce-callback prefix callback)))
      (setq company-rg--debounce-state (cons (company-rg--prefix-to-string prefix) callback)))))

(defun company-rg--prefix ()
  "Grab prefix for rg."
  (or (company-grab-symbol-cons "\\." 1)
      'stop))

(defun company-rg--receive-checker-output (process output)
  "Receive a syntax checking PROCESS OUTPUT."
  (push output (process-get process 'company-rg-pending-output)))

(defun company-rg--handle-signal (process _event)
  (when (memq (process-status process) '(signal exit))
    (let ((callback (process-get process 'company-rg-callback))
          (prefix (process-get process 'company-rg-prefix)))
      (if (and (eq (process-status process) 'exit)
               (eq (process-exit-status process) 0))
          (funcall callback (->> process
                                 company-rg--get-output
                                 company-rg--parse-output
                                 ;; Remove nils
                                 (--filter it)))
        (funcall callback nil)))))

(defun company-rg--parse-output (output)
  (split-string output "\n"))

(defun company-rg--get-output (process)
  "Get the complete output of PROCESS."
  (with-demoted-errors "Error while retrieving process output: %S"
    (let ((pending-output (process-get process 'company-rg-pending-output)))
      (apply #'concat (nreverse pending-output)))))

(defun company-rg-ignore-me ()
  (shell-command-to-string
   (concat (executable-find "rg")
           " -ioIN "
           (shell-quote-argument "\\bnet([\\w_]|::)*\\b")
           " | sort | uniq -c | sort -r | awk '{print $2}'")))

(defun company-rg-default-directory ()
  "Compute default directory"
  (or
   (if (functionp company-rg-default-directory)
       (funcall company-rg-default-directory)
     company-rg-default-directory)
   default-directory))

(defun company-rg--candidates-query (prefix callback)
  (let* ((default-directory (company-rg-default-directory))
         (command
          (list "bash"
                "-c"
                (concat
                 "rg -ioIN "
                 (shell-quote-argument
                  (concat "\\b" prefix "([\\w_-]|::)*\\b"))
                 " | sort | uniq -c | sort -r | awk '{print $2}'")))
         (process-connection-type t)
         (process (apply 'start-process "company-rg" nil command)))
    (set-process-sentinel process #'company-rg--handle-signal)
    (set-process-filter process #'company-rg--receive-checker-output)
    (process-put process 'company-rg-callback callback)
    (process-put process 'company-rg-prefix prefix)))

;;;###autoload
(defun company-rg (command &optional arg &rest _args)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-rg))
    (prefix (company-rg--prefix))
    (sorted t)
    (duplicates nil)
    (candidates (cons :async (company-rg--debounce-async arg #'company-rg--candidates-query)))))

(provide 'company-rg)
;;; company-rg.el ends here
