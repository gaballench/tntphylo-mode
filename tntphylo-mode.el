;;; tntphylo-mode.el --- An Emacs Engine for TNT -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Gustavo  A. Ballen

;; Author: Gustavo A. Ballen <gaballench@gmail.com>
;; Keywords: lisp, languages, files
;; Created: 12 november 2018
;; Version: 0.0.1
;; Package-Requires:
;; URL: https://github.com/gaballench/tntphylo-mode

;; This program is free software; you can redistribute it and/or modify
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

;; The package tntphylo-mode provides a platform for interactive work with TNT, a program for phylogenetic analyses based on parsimony.  It provides the usual advantages of a major mode, such as syntax highlight and text redirection to TNT processes.  This project itself was heavily based on `essh' and `julia-mode' although it is becoming less similar to these with time. In the end, this is a modified fork from rev-mode, an emacs package for basically the same kind of interactive work with RevBayes, a program for bayesian phylogenetic inference using graphic probabilistic models, see more in https://github.com/gaballench/rev-mode

;; Installation is a simple process.  Just clone the github repository and add the following to your .emacs file:
;(add-to-list 'load-path "/path/to/tntphylo-mode")
;(require 'tntphylo-mode)
;;; associate the .Rev extension to tntphylo-mode
;(setq auto-mode-alist
;        (cons '("\\.Rev" . tntphylo-mode) auto-mode-alist))

;;; Code:

;;;; define autoload as an instance of sh-mode as suggested here https://www.gnu.org/software/emacs/manual/html_node/elisp/Derived-Modes.html
;;;###autoload
(define-derived-mode tntphylo-mode sh-mode "tntphylo mode"
  "Major mode for editing and evaluating TNT")


;;;;;;;;;;;;;;;;;;;;;;;;;
; run-tntphylo modified from run-julia in julia-mode. working properly
;; Code for `inferior-tntphylo-mode'
(require 'comint)

(defcustom tntphylo-program "tnt"
  "Path to the program used by `inferior-tntphylo'."
  :type 'string
  :group 'tntphylo)

(defcustom tntphylo-arguments '()
  "Commandline arguments to pass to `tntphylo-program'."
  :type 'string
  :group 'tntphylo)

(defvar tntphylo-prompt-regexp "^\\w*> "
  "Regexp for matching `inferior-tntphylo' prompt.")

(defvar inferior-tntphylo-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map (kbd "TAB") 'tntphylo-latexsub-or-indent)
    map)
  "Basic mode map for `inferior-tntphylo-mode'.")

;;;###autoload
(defun inferior-tntphylo ()
    "Run an inferior instance of `tnt' inside Emacs."
    (interactive)
    (let ((tntphylo-program tntphylo-program)
          (buffer (get-buffer-create "*tnt*")))
      (when (not (comint-check-proc "*tnt*"))
            (apply #'make-comint-in-buffer "tnt" "*tnt*" tntphylo-program tntphylo-arguments))
      (pop-to-buffer-same-window "*tnt*")
      (inferior-tntphylo-mode)))

(defun inferior-tntphylo--initialize ()
    "Helper function to initialize `inferior-tntphylo'."
    (setq comint-use-prompt-regexp t))

(define-derived-mode inferior-tntphylo-mode comint-mode "tntphylo"
  "Major mode for `inferior-tntphylo'.

\\<inferior-tntphylo-mode-map>"
  nil "tntphylo"
  (setq comint-prompt-regexp tntphylo-prompt-regexp)
  (setq comint-prompt-read-only t)
  ;(set (make-local-variable 'font-lock-defaults) '(tntphylo-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) tntphylo-prompt-regexp)
  (set (make-local-variable 'indent-line-function) 'tntphylo-indent-line))

(add-hook 'inferior-tntphylo-mode-hook 'inferior-tntphylo--initialize)

;;;###autoload
(defalias 'run-tntphylo #'inferior-tntphylo
  "Run an inferior instance of `tntphylo' inside Emacs.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;code modified from essh.el, it turns out to contain code that the developer of essh borrowed from sh-mode.el
;; function taken from ess package
(defun tntphylo-next-code-line (&optional arg)
  "Move ARG lines of code forward (backward if ARG is negative).
Skips past all empty and comment lines.	 Default for ARG is 1.

On success, return 0.  Otherwise, go as far as possible and return -1."
  (interactive "p")
  (or arg (setq arg 1))
  (beginning-of-line)
  (let ((n 0)
	(inc (if (> arg 0) 1 -1)))
    (while (and (/= arg 0) (= n 0))
      (setq n (forward-line inc)); n=0 is success
      (while (and (= n 0)
		  (looking-at "\\s-*\\($\\|\\s<\\)"))
	(setq n (forward-line inc)))
      (setq arg (- arg inc)))
    n))

(defun process-tntphylo ()
  "Return a list with existing shell process."
  (interactive)
  (setq listpr (process-list))
  (setq lengthpr (length listpr))
  (setq i 0)
  (setq listshellp '())
  (while (< i lengthpr)
    (setq pos (string-match "tnt" (prin1-to-string (elt listpr i))))
    (if pos (add-to-list 'listshellp (process-name (get-process (elt listpr i)))))
    (setq i (+ 1 i)))
  listshellp)


(defun process-tntphylo-choose ()
  "Return which process to use."
(interactive)
(setq outpr 0)
(setq cbuf (current-buffer))
(setq shelllist (process-tntphylo))
(setq shelln (length shelllist))
(if (eq shelln 0)
    (progn (shell)
	   (switch-to-buffer cbuf)
	   (setq outpr (get-process "tnt"))
	   (sleep-for 0.5)))
(if (eq shelln 1)
    (setq outpr (get-process (elt shelllist 0))))
(if (> shelln 1)
(progn
(setq proc (completing-read "Send code to:" shelllist nil t (elt shelllist 0)))
(setq outpr (get-process proc))))
outpr)


(defun tntphylo-eval-line (sprocess command)
  "Evaluate SPROCESS with a redirected COMMAND.
Argument SPROCESS evaluates a single command.
Argument COMMAND is a string to be redirected."
  (setq sbuffer (process-buffer sprocess))
  (setq command (concat command "\n"))
  (accept-process-output sprocess 0 10)
  (with-current-buffer sbuffer
    (end-of-buffer) ;point is not seen being moved (unless sbuffer is focused)
    (insert command)			;pastes the command to shell
    (set-marker (process-mark sprocess) (point-max))
    (process-send-string sprocess command)
    ;; (accept-process-output sprocess 0 10)
    ))

; commented function, maybe not needed at all
;(defun shell-cd-current-directory ()
;  "Changes the shell working directory to the current buffer's one."
;  (interactive)
;  (setq sprocess (process-shell-choose))
;  (setq com (format "cd %s" (file-name-directory default-directory)))
;  (shell-eval-line sprocess com))


(defun pipe-line-to-tntphylo (&optional step)
  "Evaluate the current line to the tntphylo interpreter.
Optional argument STEP ."
  (interactive ())
  (if (process-tntphylo) nil
		  (run-tntphylo))
  (setq com (buffer-substring (point-at-bol) (point-at-eol)))
  (if (> (length com) 0)
      (progn
	(setq sprocess (process-tntphylo-choose))
	(tntphylo-eval-line sprocess com)
	(when step (tntphylo-next-code-line)))
    (message "No command in this line")))

(defun pipe-line-to-tntphylo-and-step ()
  "Evaluate the current line to the tntphylo interpreter and go to next line."
  (interactive)
  (if (process-tntphylo) nil
		  (run-tntphylo))
  (pipe-line-to-tntphylo t))

(defun pipe-region-to-tntphylo (start end)
  "Sends a region to the tntphylo interpreter.
Argument START .
Argument END ."
  (interactive "r")
  (if (process-tntphylo) nil
		  (run-tntphylo))
  (setq com (buffer-substring start end))	       ;reads command
  (setq lcom (length com))		       ;count chars
  (setq lastchar (substring com (1- lcom) lcom)) ;get last char
  (unless (string-match "\n" lastchar) ;if last char is not "\n", then...
    (setq com (concat com "\n")))	     ;...add it!
  (setq sprocess (process-tntphylo-choose))
  (setq sbuffer (process-buffer sprocess))
  (while (> (length com) 0)
    (setq pos (string-match "\n" com))
    (setq scom (substring com 0 pos))
    (setq com (substring com (min (length com) (1+ pos))))
    (tntphylo-eval-line sprocess scom)
    (accept-process-output sprocess 0 10)
    ))


(defun pipe-buffer-to-tntphylo ()
  "Evaluate whole buffer to the tntphylo interpreter."
  (interactive)
  (if (process-tntphylo) nil
		  (run-tntphylo))
  (pipe-region-to-tntphylo (point-min) (point-max)))

(defun pipe-function-to-tntphylo ()
"Evaluate function to the tntphylo interpreter."
(interactive)
  (if (process-tntphylo) nil
		  (run-tntphylo))
(setq beg-end (essh-beg-end-of-function))
(if beg-end
    (save-excursion
      (setq beg (nth 0 beg-end))
      (setq end (nth 1 beg-end))
      (goto-line beg)
      (setq origin (point-at-bol))
      (goto-line end)
      (setq terminal (point-at-eol))
      (pipe-region-to-tntphylo origin terminal))
  (message "No function at current point.")))

(defun tntphylo-beg-end-of-function ()
  "Return the lines where the function start and ends.  If there is no function at current line, it return nil."
  (interactive)
  (setq curline (line-number-at-pos))	;current line
  (setq curcom (buffer-substring (point-at-bol) (point-at-eol)))
  (setq pos (string-match "function" curcom))
  (save-excursion
    (if pos
	(progn
	  (setq beg curline))
      (progn
	(while (not pos)
	  (setq curline (1- curline))	;current line
	  (previous-line)			;go to previous line
	  (setq curcom (buffer-substring (point-at-bol) (point-at-eol)))
	  (setq pos (string-match "function" curcom)))
      (setq beg curline)))
    (beginning-of-line)
    (forward-list)			; move pointer to first matching brace
    (setq end (line-number-at-pos)))
  ;; (message (format  "%d %d" beg end))
  (if (and (<= (line-number-at-pos) end) (>= (line-number-at-pos) beg))
      (list beg end)
    nil))
;;;;;;;;;;;;;;

;;;; In development
;; code for autodetecting dot files and compiling with graphviz
;; still something missing as it is detecting temp files (#)
;(first-error (lgrep "digraph" "-r --exclude=\\# --exclude=\\*.Rev" (file-name-directory buffer-file-name)))
;(defun rev-mode-preview-graph ()
;  (interactive)
;  (grep-compute-defaults)
;  (lgrep "digraph" "--exclude=\*.Rev --exclude=\*.el" "*" (file-name-directory buffer-file-name) t)
;  (next-error)
;  (message "I keep doing things after first-error...")
;  (graphviz-dot-preview))

;; Keybindings
(defun tntphylo-mode-sh-hook ()                                             "."
  (define-key sh-mode-map "\C-c\C-r" 'pipe-region-to-tntphylo)        ;;
  (define-key sh-mode-map "\C-c\C-b" 'pipe-buffer-to-tntphylo)        ;;
  (define-key sh-mode-map "\C-c\C-j" 'pipe-line-to-tntphylo)          ;;
  (define-key sh-mode-map "\C-c\C-n" 'pipe-line-to-tntphylo-and-step) ;;
  (define-key sh-mode-map "\C-c\C-f" 'pipe-function-to-tntphylo))      ;;
;  (define-key sh-mode-map "\C-c\C-d" 'tntphylo-cd-current-directory)) ;;
(add-hook 'sh-mode-hook 'tntphylo-mode-sh-hook)
;; setup files ending in “.tntphylo” to open in rev-mode
;(add-to-list 'auto-mode-alist '("\\.rev\\'" . rev-mode))

(provide 'tntphylo-mode)
;;; tntphylo-mode.el ends here
