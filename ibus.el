;;; ibus.el -- iBus client for GNU Emacs

;; Copyright (C) 2010 S. Irie

;; Author: S. Irie
;; Maintainer: S. Irie
;; Keywords: Input Method, i18n

(defconst ibus-mode-version "0.0.1.2")

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; iBus is a new input method framework under active development
;; which is designed to overcome the limitations of SCIM.

;; iBus uses D-Bus protocol for communication between the ibus-daemon
;; and clients (engines, panel, config tools). Since the components
;; run in separate processes there is enhanced modularity and stability.
;; Client processes can be loaded, started and stopped independently.
;; iBus supports Gtk2 and XIM, and has input method engines for anthy,
;; chewing, hangul, m17n, pinyin, rawcode, and large tables. Engines
;; and clients can be written in any language with a dbus binding.

;; This program is iBus client for GNU Emacs. It is, however,
;; not part of official iBus distribution.

;;
;; Installation:
;;
;; First, save this file as ibus.el and byte-compile in
;; a directory that is listed in load-path, and also save
;; ibus-agent.py somewhere in your system.
;;
;; Put the following in your .emacs file:
;;
;;   (require 'ibus)
;;   (setq ibus-agent-command "/PATH/TO/ibus-agent.py")
;;
;; After that, execute Emacs by typing on command line:
;;
;;   XMODIFIERS=@im=none emacs
;;
;; and turn on ibus-mode:
;;
;;   M-x ibus-mode
;;
;; Note that this program requires GNU Emacs 22 or later.
;;

;;; History:
;; 2010-04-12  S. Irie
;;         * Version 0.0.1
;;         * Initial experimental version

;;; Code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ibus-agent-buffer-name " *iBus*")

(defvar ibus-agent-command "/usr/share/pyshared/ibus-mode/ibus-mode-agent.py")
(defvar ibus-python-command nil)

(defvar ibus-agent-timeout 3.0)

(defvar ibus-log-buffer "*ibus-mode log*")
(defvar ibus-debug nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ibus-mode nil)
(make-variable-buffer-local 'ibus-mode)
(put 'ibus-mode 'permanent-local t)

(defvar ibus-mode-map
  (let ((map (make-keymap))
	(i 0))
    (while (< i 128)
      (define-key map (char-to-string i) 'ibus-handle-event)
      (setq i (+ 1 i)))
    (define-key map [(control ?\s)] 'ibus-handle-event)
    (define-key map [return] 'ibus-handle-event)
    (define-key map [backspace] 'ibus-handle-event)
    (define-key map [ibus-receive-event] 'ibus-exec-callback)
    (setq anthy-preedit-keymap map)))

(defvar ibus-keysym-alist
  '((backspace . ?\xFF08)
    (return . ?\xFF0D)))

(defvar ibus-modifier-alist
  (mapcar (lambda (pair)
	    (cons (car pair)
		  (ash 1 (cdr pair))))
	  '((shift . 0)
	    (control . 2)
	    (meta . 3))))

(defvar ibus-agent-process nil)
(defvar ibus-callback-queue nil)
(defvar ibus-current-buffer nil)

(defvar ibus-preedit-text "")
(make-variable-buffer-local 'ibus-preedit-text)
(put 'ibus-preedit-text 'permanent-local t)

(defvar ibus-preedit-attrs nil)
(make-variable-buffer-local 'ibus-preedit-attrs)
(put 'ibus-preedit-attrs 'permanent-local t)

(defvar ibus-auxiliary-text "")
(make-variable-buffer-local 'ibus-auxiliary-text)
(put 'ibus-auxiliary-text 'permanent-local t)

(defvar ibus-preedit-overlay nil)
(make-variable-buffer-local 'ibus-preedit-overlay)
(put 'ibus-preedit-overlay 'permanent-local t)

(defvar ibus-auxiliary-overlay nil)
(make-variable-buffer-local 'ibus-auxiliary-overlay)
(put 'ibus-auxiliary-overlay 'permanent-local t)

(defvar ibus-imcontext-id nil)
(make-variable-buffer-local 'ibus-imcontext-id)
(put 'ibus-imcontext-id 'permanent-local t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Messages & Log
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibus-log1 (format-string args)
  (let ((log-str (apply 'format format-string args)))
    (with-current-buffer (get-buffer-create ibus-log-buffer)
      (let ((window (get-buffer-window (current-buffer))))
	(save-selected-window
	  (if window (select-window window))
	  (goto-char (point-max))
	  (insert (concat (format log-str) "\n"))
	  (if window (recenter -1)))))))

(defun ibus-log (format-string &rest args)
  (if (and ibus-debug
	   format-string)
      (ibus-log1 format-string args)))

(defun ibus-log-undo-list (format-string &rest args)
  (when ibus-debug
    (if format-string
	(ibus-log1 format-string args))
    (if (not (listp buffer-undo-list))
	(ibus-log1 "undo list (disabled): %S" (list buffer-undo-list))
      (ibus-log1 " top: %S" (list (car buffer-undo-list)))
      (ibus-log1 " 2nd: %S" (list (cadr buffer-undo-list)))
      (ibus-log1 " 3rd: %S" (list (nth 2 buffer-undo-list)))
      (ibus-log1 " 4th: %S" (list (nth 3 buffer-undo-list))))))

(defun ibus-message (format-string &rest args)
  (apply 'message (concat "iBus: " format-string) args)
  (apply 'ibus-log (concat "message: " format-string) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start/stop agent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibus-kill-agent ()
  (when (processp ibus-agent-process)
    (let ((buffer(process-buffer ibus-agent-process)))
      (with-current-buffer buffer
	(remove-hook 'after-change-functions
		     'ibus-agent-receive-passively t))
      (kill-buffer buffer))
    (delete-process ibus-agent-process)
    (scim-log "process: %s  status: %s" proc (process-status proc))
    )
  (setq ibus-agent-process nil))

(defun ibus-agent-process-sentinel (proc stat)
  (ibus-message "process: %s  status: %s" proc (substring stat 0 -1))
  (ibus-mode-quit))

(defun ibus-start-agent ()
  (ibus-kill-agent)
  (condition-case err
      (let ((proc (if ibus-python-command
		      (start-process "ibus-agent"
				     ibus-agent-buffer-name
				     ibus-python-command
				     ibus-agent-command)
		    (start-process "ibus-agent"
				   ibus-agent-buffer-name
				   ibus-agent-command))))
	(when (processp proc)
	  (set-process-query-on-exit-flag proc nil)
	  (set-process-coding-system proc 'utf-8 'utf-8)
	  (set-process-sentinel proc 'ibus-agent-process-sentinel)
	  (with-current-buffer (process-buffer proc)
	    (buffer-disable-undo)
	    (erase-buffer)
	    (add-hook 'after-change-functions
		      'ibus-agent-receive-passively nil t)))
	(setq ibus-agent-process proc))
    (error
     (ibus-message "%S: %S" (car error) (cadr error))
     (setq ibus-agent-process nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Communicate with agent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibus-agent-send (string &rest objects)
  (condition-case err
      (with-current-buffer (process-buffer ibus-agent-process)
	(let ((command (apply 'format string objects))
	      (inhibit-modification-hooks t))
	  (erase-buffer)
	  (ibus-log "process: %s  status: %s" ibus-agent-process (process-status ibus-agent-process))
	  (ibus-log "send: %S" command)
	  (process-send-string ibus-agent-process (concat command "\n"))
	  t)) ; Succeeded
    (error
     (ibus-message "Couldn't send command to agent %S" err)
     nil))) ; Failed

(defun ibus-agent-receive (&optional passive)
  (let (repl)
    (save-current-buffer
      (when (or passive
		(and (processp ibus-agent-process)
		     (set-buffer (process-buffer ibus-agent-process))))
	(let ((inhibit-modification-hooks t)
	      (sec (and (floatp ibus-agent-timeout) ibus-agent-timeout))
	      (msec (and (integerp ibus-agent-timeout) ibus-agent-timeout)))
	  (when (= (point-max) 1)
	    (accept-process-output ibus-agent-process sec msec t))
	  (ibus-log "receive:\n%s" (buffer-substring (point-min) (point-max)))
	  (goto-char (point-min))
	  (while (let ((pos (point)))
		   (condition-case err
		       (push (read (current-buffer)) repl)
		     (end-of-file
		      (goto-char pos)
		      nil)
		     (invalid-read-syntax
		      (goto-char (point-max))
		      nil))))
	  (delete-region (point-min) (point))
	  (setq repl (nreverse repl))
	  (setq unread-command-events
		(delq 'ibus-receive-event
		      (delq 'ibus-dummy-event unread-command-events))))
	(if repl
	    (ibus-process-signals repl)
	  (ibus-message "Data reception became timeout."))))))

(defun ibus-agent-send-receive (string &rest objects)
  (and (apply 'ibus-agent-send string objects)
       (ibus-agent-receive)))

(defun ibus-agent-receive-passively (beg &optional end lng)
  (ibus-log "passively receive")
  (ibus-agent-receive t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Execute callbacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibus-exec-callback (&optional sexplist)
  (interactive)
  (when (interactive-p)
;    (unless (eq last-command 'ibus-handle-event)
;      (setq ibus-string-insertion-failed nil))
    (setq this-command last-command
	  unread-command-events
	  (delq 'ibus-receive-event
		(delq 'ibus-dummy-event unread-command-events))))
  (when (buffer-live-p ibus-current-buffer)
    (with-current-buffer ibus-current-buffer
      (ibus-log "buffer: %s" (current-buffer))
      (ibus-log "imcontext-id: %s" ibus-imcontext-id)
      (mapc (lambda (sexp)
	      (ibus-log "execute: %S" sexp)
	      (eval sexp))
	    (or sexplist
		ibus-callback-queue))
;      (ibus-do-update-preedit)
      )))

(defun ibus-process-signals (sexplist &optional passive)
  (ibus-log "this-command: %s" this-command)
  (ibus-log "last-command: %s" last-command)
;  (ibus-log "ibus-last-command-event: %s" ibus-last-command-event)
  (ibus-log "before-change-functions: %s" before-change-functions)
  (if (not passive)
      (ibus-exec-callback sexplist)
    (if ibus-callback-queue
	(nconc ibus-callback-queue sexplist)
      (setq ibus-callback-queue sexplist))
    (setq unread-command-events
	  (cons 'ibus-receive-event
		(delq 'ibus-receive-event
		      (delq 'ibus-dummy-event unread-command-events))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Callbacks for preediting & commit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibus-commit-text-cb (ic text)
  (insert text))

(defun ibus-hide-preedit-text-cb (ic)
  (if ibus-preedit-overlay
      (delete-overlay ibus-preedit-overlay)))

(defun ibus-show-preedit-text-cb (ic)
  (ibus-hide-preedit-text-cb ic)
  (setq ibus-preedit-overlay (make-overlay (point) (point)))
  (overlay-put ibus-preedit-overlay 'before-string
	       (propertize ibus-preedit-text 'face 'underline)))

(defun ibus-update-preedit-text-cb (ic text cursor-pos visible &rest attrs)
  (setq ibus-preedit-text text
	ibus-preedit-attrs attrs)
  (if visible
      (ibus-show-preedit-text-cb ic)
    (ibus-hide-preedit-text-cb ic)))

(defun ibus-hide-auxiliary-text-cb (ic)
  (if ibus-auxiliary-overlay
      (delete-overlay ibus-auxiliary-overlay)))

(defun ibus-show-auxiliary-text-cb (ic)
  (ibus-hide-auxiliary-text-cb ic)
  (setq ibus-auxiliary-overlay (make-overlay (point) (point)))
  (overlay-put ibus-auxiliary-overlay 'before-string
	       (propertize ibus-auxiliary-text 'face 'underline)))

(defun ibus-update-auxiliary-text-cb (ic text visible)
  (setq ibus-auxiliary-text text)
  (if visible
      (ibus-show-auxiliary-text-cb ic)
    (ibus-hide-auxiliary-text-cb ic)))

(defun ibus-hide-lookup-table-cb (ic)
  (message nil))

(defun ibus-show-lookup-table-cb (ic current-table)
  (message (let ((i 0))
	     (mapconcat (lambda (candidate)
			  (format "%s. %s" (setq i (1+ i)) (eval candidate)))
			current-table
			" "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manage IMContexts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibus-create-imcontext ()
  (unless ibus-imcontext-id
    (setq ibus-imcontext-id 'RQ) ; Set symbol to avoid multiple request
    (let ((time-limit (+ (float-time)
			 (or (and (floatp ibus-agent-timeout)
				  ibus-agent-timeout)
			     (/ ibus-agent-timeout 1000.0)))))
      (ibus-agent-send-receive "create_imcontext()")
      (while (and (not (numberp ibus-imcontext-id))
		  (< (float-time) time-limit))
	(sit-for 0.1)))
    (unless (numberp ibus-imcontext-id)
      (ibus-mode-off)
      (ibus-message "Couldn't register imcontext. Turned off ibus-mode.")
      (error))))

(defun ibus-create-imcontext-cb (ic)
  (setq ibus-imcontext-id ic))

(defun ibus-destroy-imcontext ()
  (when (numberp ibus-imcontext-id)
    (ibus-agent-send "destroy_imcontext(%d)" ibus-imcontext-id)
    (kill-local-variable 'ibus-imcontext-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manage buffer selecting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibus-check-current-buffer ()
  (setq ibus-current-buffer (current-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process key events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibus-process-key-event (event)
  (let ((modifiers (event-modifiers event))
	(keyval (event-basic-type event))
	(mask 0))
    (ibus-check-current-buffer)
    (unless (numberp keyval)
      (setq keyval (or (cdr (assq keyval ibus-keysym-alist))
			 0)))
    (mapc (lambda (modifier)
	    (setq mask (logior mask
			       (or (cdr (assq modifier ibus-modifier-alist))
				   0))))
	  modifiers)
    (ibus-agent-send-receive "process_key_event(%d, %d, %d, %d)"
			     ibus-imcontext-id keyval 0 mask)))

(defun ibus-restore-keymap ()
  (setq ibus-mode t)
  (remove-hook 'pre-command-hook 'ibus-restore-keymap))

(defun ibus-process-key-event-cb (ic handled)
  (unless handled
    (push event unread-command-events)
    (setq ibus-mode nil)
    (add-hook 'pre-command-hook 'ibus-restore-keymap)))

(defun ibus-handle-event (&optional arg)
  (interactive "*p")
  (ibus-process-key-event last-command-event))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup minor mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibus-update-mode-line ()
  (force-mode-line-update)
  ibus-mode)

(defun ibus-mode-on ()
  (unless (minibufferp)
    (unless (processp ibus-agent-process)
      (ibus-start-agent))
    (when (processp ibus-agent-process)
      (setq ibus-current-buffer (current-buffer))
      (ibus-create-imcontext)
      (when ibus-imcontext-id
	(add-hook 'kill-buffer-hook 'ibus-destroy-imcontext nil t)
	(add-hook 'post-command-hook 'ibus-check-current-buffer)
	(setq ibus-mode t)
	(ibus-update-mode-line)))))

(defun ibus-mode-quit ()
  (remove-hook 'kill-buffer-hook 'ibus-destroy-imcontext t)
  (remove-hook 'post-command-hook 'ibus-check-current-buffer)
  (kill-local-variable 'ibus-imcontext-id)
  (setq ibus-mode nil)
  (ibus-update-mode-line))

(defun ibus-mode-off ()
  (condition-case err
    (ibus-destroy-imcontext)
    (error
     (message "%S: %S" (car error) (cdr error))))
  (ibus-mode-quit))

(defun ibus-mode-off-all ()
  (mapc (lambda (buffer)
	  (with-current-buffer buffer
	    (if ibus-mode
		(ibus-mode-off))))
	(buffer-list)))

(add-hook 'kill-emacs-hook 'ibus-mode-off-all)

(defun ibus-mode (&optional arg)
  "Start/stop iBus conversion system."
  (interactive "P")
  (if (if (null arg)
	  (not ibus-mode)
	(> (prefix-numeric-value arg) 0))
      (ibus-mode-on)
    (ibus-mode-off)))

(unless (assq 'ibus-mode minor-mode-alist)
  (setq minor-mode-alist
	(cons '(ibus-mode " iBus") minor-mode-alist)))

(unless (assq 'ibus-mode minor-mode-map-alist)
  (setq minor-mode-map-alist
	(cons (cons 'ibus-mode ibus-mode-map) minor-mode-map-alist)))

;(define-key mode-line-mode-menu [ibus-mode]
;  `(menu-item ,(purecopy "Intelligent Input Bus (iBus)") ibus-mode
;	      :help "Support the input of various languages"
;	      :button (:toggle . (bound-and-true-p ibus-mode))))


(provide 'ibus)

;;;
;;; ibus.el ends here
