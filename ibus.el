;;; ibus.el -- iBus client for GNU Emacs

;; Copyright (C) 2010 S. Irie

;; Author: S. Irie
;; Maintainer: S. Irie
;; Keywords: Input Method, i18n

(defconst ibus-mode-version "0.0.2.16")

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
;; ibus-mode-agent.py somewhere in your system.
;;
;; Put the following in your .emacs file:
;;
;;   (require 'ibus)
;;   (add-hook 'after-init-hook 'ibus-mode-on)
;;   (setq ibus-agent-command "/PATH/TO/ibus-mode-agent.py")
;;
;; To disable XIM in Emacs, put the following in ~/.Xresources:
;;
;;   Emacs*useXIM: false
;;
;; and restart X session or execute a shell command:
;;
;;   xrdb ~/.Xresources
;;
;;
;; Here is the example of settings in .emacs:
;;
;;   (require 'ibus)
;;   ;; Turn on ibus-mode automatically after loading .emacs
;;   (add-hook 'after-init-hook 'ibus-mode-on)
;;   ;; Specify file path of ibus-mode-agent.py
;;   (setq ibus-agent-command "/PATH/TO/ibus-mode-agent.py")
;;   ;; Use C-SPC for Set Mark command
;;   (ibus-define-common-key ?\C-\s nil)
;;   ;; Use C-/ for Undo command
;;   (ibus-define-common-key ?\C-/ nil)
;;   ;; Change cursor color depending on iBus status
;;   (setq ibus-cursor-color '("red" "blue" "limegreen"))
;;
;;
;; Note that this program requires GNU Emacs 22 or later, and
;; doesn't work when X server is not running.
;;

;;; History:
;; 2010-05-09  S. Irie
;;         * Version 0.0.2
;;         * Imported many functions from scim-bridge.el
;;
;; 2010-04-12  S. Irie
;;         * Version 0.0.1
;;         * Initial experimental version

;; ToDo:

;;  * surrounding text
;;  * Japanese thumb shift input
;;  * leim

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup ibus nil
  "Intelligent Input Bus (iBus)"
  :prefix "ibus-"
  :group 'editing :group 'wp)

;; Basic settings
(defgroup ibus-basic nil
  "Settings of operation, such as mode management and keyboard"
  :group 'ibus)

(defcustom ibus-mode-local t
  "If the value is non-nil, IMContexts are registered for each buffer
so that the input method of buffers can be toggled individually.
Otherwise, the input method is globally toggled."
  :type 'boolean
  :group 'ibus-basic)

(defcustom ibus-imcontext-temporary-for-minibuffer t
  "If non-nil, an one-time IMContext is used for a minibuffer so that
the minibuffer always starts with iBus's input status off. This option
is effective only when the option `ibus-mode-local' is active (non-nil)."
  :type 'boolean
  :group 'ibus-basic)

(defun ibus-customize-isearch (var value)
  (set var value)
  (if (and (fboundp 'ibus-setup-isearch)
	   (bound-and-true-p ibus-mode))
      (ibus-setup-isearch)))

(defcustom ibus-use-in-isearch-window t
  "If non-nil, iBus can be used with isearch-mode. Otherwise, it can't."
  :set 'ibus-customize-isearch
  :type 'boolean
  :group 'ibus-basic)

(defun ibus-customize-key (var value)
  (set var value)
  (if (and (fboundp 'ibus-update-key-bindings)
	   (bound-and-true-p ibus-mode))
      (ibus-update-key-bindings var)))

(defcustom ibus-common-function-key-list
  '((control ".")
    (control ",")
    (control "<")
    (control ">")
    (control "/")
    (control " ")
    (shift " ")
    (control alt left)
    (control alt right)
    (control alt up)
    (control alt down)
    (zenkaku-hankaku)
    (henkan)
    (shift henkan)
    (muhenkan)
    (hiragana-katakana)
    (alt romaji)
    (f6)
    (f7)
    (f8)
    (shift f8)
    (f9)
    (f10)
    (f11)
    (f12)
    (kp-space)
    (kp-equal)
    (kp-multiply)
    (kp-add)
    (kp-separator)
    (kp-subtract)
    (kp-decimal)
    (kp-divide)
    (kp-0)
    (kp-1)
    (kp-2)
    (kp-3)
    (kp-4)
    (kp-5)
    (kp-6)
    (kp-7)
    (kp-8)
    (kp-9))
  "This list indicates which keystrokes iBus takes over at both direct
insert mode and preediting mode. You can also add/remove the elements
using the function `ibus-define-common-key'.
NOTICE: Don't set prefix keys in this option, such as ESC and C-x.
If you do so, operating Emacs might become impossible."
  :set 'ibus-customize-key
  :type '(repeat (list :format "%v"
		       (set :format "%v"
			    :inline t
			    (const :format "M- " meta)
			    (const :format "C- " control)
			    (const :format "S- " shift)
			    (const :format "H- " hyper)
			    (const :format "s- " super)
			    (const :format "A- " alt))
		       (restricted-sexp :format "%v"
					:match-alternatives
					(symbolp stringp))))
  :group 'ibus-basic)

(defcustom ibus-preedit-function-key-list
  '((escape)
    (left)
    (right)
    (up)
    (down)
    (home)
    (end)
    (prior)
    (next)
    (return)
    (shift left)
    (shift right)
    (shift up)
    (shift down)
    (shift return)
    (tab)
    (iso-lefttab)
    (shift tab)
    (shift iso-lefttab)
    (backtab)
    (backspace)
    (delete)
    (kp-enter)
    (kp-tab))
  "This list indicates which keystrokes iBus takes over when the
preediting area exists. You can also add/remove the elements using
the function `ibus-define-preedit-key'."
  :set 'ibus-customize-key
  :type '(repeat (list :format "%v"
		       (set :format "%v"
			    :inline t
			    (const :format "M- " meta)
			    (const :format "C- " control)
			    (const :format "S- " shift)
			    (const :format "H- " hyper)
			    (const :format "s- " super)
			    (const :format "A- " alt))
		       (restricted-sexp :format "%v"
					:match-alternatives
					(symbolp stringp))))
  :group 'ibus-basic)

(defcustom ibus-use-ja-onbiki-key nil
  "If you use Japanese kana typing method with jp-106 keyboard, turn
on (non-nil) this option to input a kana prolonged sound mark (`ー')
without pushing the shift key.
 This option is made effectual by temporarily modifying the X-window
system's keyboard configurations with a shell command `xmodmap'."
  :set 'ibus-customize-key
  :type 'boolean
  :group 'ibus-basic)

(defcustom ibus-simultaneous-pressing-time nil
  "If you use Japanese thumb shift typing method on iBus-Anthy,
specify the time interval (in seconds) which is corresponding to
`simultaneous pressing time' setting of iBus-Anthy. Two keystrokes
within this time interval are sent to iBus as a simultaneous keystroke."
  :type '(choice (const :tag "none" nil)
		 (number :tag "interval (sec.)" :value 0.1))
  :group 'ibus-basic)

(defcustom ibus-undo-by-committed-string nil
  "If the value is nil, undo is performed bringing some short
committed strings together or dividing the long committed string
within the range which does not exceed 20 columns. Otherwise, undo
is performed to each committed string."
  :type 'boolean
  :group 'ibus-basic)

(defcustom ibus-clear-preedit-when-unexpected-event nil
  "If the value is non-nil, the preediting area is cleared in the
situations that the unexpected event happens during preediting.
The unexpected event is, for example, that the string is pasted
with the mouse."
  :type 'boolean
  :group 'ibus-basic)

;; Appearance
(defgroup ibus-appearance nil
  "Faces, candidate window, etc."
  :group 'ibus)

(defun ibus-customize-cursor-color (var value)
  (set var value)
  (if (and (fboundp 'ibus-set-cursor-color)
	   (bound-and-true-p ibus-mode))
      (ibus-set-cursor-color)))

(defcustom ibus-cursor-color
  nil
  "If the value is a string, it specifies the cursor color applied
when iBus is on. If a cons cell, its car and cdr are the cursor colors
which indicate that iBus is on and off, respectively. If a list, the
first, second and third (if any) elements correspond to that iBus is
on, off and disabled, respectively. The value nil means that the cursor
color is not controlled at all."
  :set 'ibus-customize-cursor-color
  :type '(choice (const :tag "none (nil)" nil)
		 (color :tag "red" :format "red (%{sample%})\n" :value "red")
		 (color :tag "blue" :format "blue (%{sample%})\n" :value "blue")
		 (color :tag "green" :format "green (%{sample%})\n" :value "limegreen")
		 (color :tag "other" :value "red")
		 (cons  :tag "other (ON . OFF)"
			(color :format "ON: %v (%{sample%})  " :value "red")
			(color :format "OFF: %v (%{sample%})\n" :value "blue"))
		 (list  :tag "other (ON OFF)"
			(color :format "ON: %v (%{sample%})  " :value "red")
			(color :format "OFF: %v (%{sample%})  DISABLED: none\n"
			       :value "blue"))
		 (list  :tag "other (ON OFF DISABLED)"
			(color :format "ON: %v (%{sample%})  " :value "red")
			(color :format "OFF: %v (%{sample%})\n" :value "blue")
			(color :format "DISABLED: %v (%{sample%})\n"
			       :value "limegreen")))
  :group 'ibus-appearance)

(defcustom ibus-isearch-cursor-type
  0
  "This option specifies the cursor shape which is applied when
isearch-mode is active. If an integer 0, this option is not active so
that the cursor shape is not changed.
See `cursor-type'."
  :type '(choice (const :tag "default (0)" 0)
		 (const :tag "use frame parameter" t)
		 (const :tag "don't display" nil)
		 (const :tag "filled box" box)
		 (const :tag "hollow box" hollow)
		 (const :tag "vertical bar" bar)
		 (cons :tag "vertical bar (specify width)"
		       (const :format "" bar)
		       (integer :tag "width" :value 1))
		 (const :tag "horizontal bar" hbar)
		 (cons :tag "horizontal bar (specify height)"
		       (const :format "" hbar)
		       (integer :tag "height" :value 1)))
  :group 'ibus-appearance)

(defcustom ibus-cursor-type-for-candidate
  0
  "This option specifies the cursor shape which is applied when the
preediting area shows conversion candidates. If an integer 0, this
option is not active so that the cursor shape is not changed.
See `cursor-type'."
  :type '(choice (const :tag "default (0)" 0)
		 (const :tag "use frame parameter" t)
		 (const :tag "don't display" nil)
		 (const :tag "filled box" box)
		 (const :tag "hollow box" hollow)
		 (const :tag "vertical bar" bar)
		 (cons :tag "vertical bar (specify width)"
		       (const :format "" bar)
		       (integer :tag "width" :value 1))
		 (const :tag "horizontal bar" hbar)
		 (cons :tag "horizontal bar (specify height)"
		       (const :format "" hbar)
		       (integer :tag "height" :value 1)))
  :group 'ibus-appearance)

(defcustom ibus-put-cursor-on-candidate
  nil
  "When the preediting area shows conversion candidates, the cursor
is put on the selected segment if this option is non-nil. Otherwise,
the cursor is put to the tail of the preediting area."
  :type 'boolean
  :group 'ibus-appearance)

;; Advanced settings
(defgroup ibus-expert nil
  "Advanced settings"
  :group 'ibus)

(defcustom ibus-focus-update-interval 0.3
  "The window focus is checked with this cycle measured in seconds.
When iBus is off or input focus is in the other application, the slower
time cycle given by `ibus-focus-update-interval-long' is used instead."
  :type 'number
  :group 'ibus-expert)

(defcustom ibus-focus-update-interval-long 1.0
  "This value might be used as a slow time cycle for the observation
of input focus instead of `ibus-focus-update-interval'.

See `ibus-focus-update-interval' for details."
  :type 'number
  :group 'ibus-expert)

(defcustom ibus-ja-onbiki-x-keysym "F24"
  "When Japanese prolonged sound mark (onbiki) key is used, this
option specifies the substitute KeySym name used in X window system for
the key. This program sets the substitute KeySym for backslash key to
distinguish it from yen-mark key."
  :set 'ibus-customize-key
  :type 'string
  :group 'ibus-expert)

(defcustom ibus-ja-onbiki-key-symbol 'f24
  "When Japanese prolonged sound mark (onbiki) key is used, this
option specifies the event corresponding to the substitute KeySym given
in `ibus-ja-onbiki-x-keysym' as a symbol. This program sets the
substitute KeySym for backslash key to distinguish it from yen-mark key."
  :set 'ibus-customize-key
  :type '(choice (symbol)
		 (const :tag "none" nil))
  :group 'ibus-expert)

(defcustom ibus-agent-timeout 3.0
  "Specify the maximum waiting time for data reception from iBus.
A floating point number means the number of seconds, otherwise an integer
the milliseconds."
  :type 'number
  :group 'ibus-expert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ibus-agent-command "/usr/share/pyshared/ibus-mode/ibus-mode-agent.py")
(defvar ibus-python-command nil)

(defvar ibus-debug nil)
(defvar ibus-log-buffer "*ibus-mode log*")

(defvar ibus-meta-key-exists
  (string< "" (shell-command-to-string "xmodmap -pke | grep '= Meta'"))
  "t is set in this variable if there is mata modifier key in the
keyboard. When automatic detection doesn't go well, please set the
value manually before ibus.el is loaded.")

(defvar ibus-agent-buffer-name " *iBus*")

(defvar ibus-incompatible-major-modes
  '(ebrowse-tree-mode w3m-mode)
  "List of symbols specifying major modes that keymaps of ibus-mode are
deactivated.")

(defvar ibus-preedit-incompatible-commands
  '(undo undo-only redo undo-tree-undo undo-tree-redo)
  "List of symbols specifying commands which are disabled when preediting.")

(defvar ibus-inherit-im-functions
  '(read-from-minibuffer read-string read-no-blanks-input completing-read)
  "List of symbols specifying functions which inherit input method.
If the function takes the argument INHERIT-INPUT-METHOD, input method
is inherited only when it's non-nil. Otherwise, input method is
unconditionally inherited.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ibus-modifier-alist
  (mapcar (lambda (pair)
	    (cons (car pair)
		  (lsh 1 (cdr pair))))
	  `((shift . 0)
	    (control . 2)
	    (,(if ibus-meta-key-exists 'alt 'meta) . 3)
	    (super . 26)
	    (hyper . 27)
	    ,@(if ibus-meta-key-exists '((meta . 28))))))

(defvar ibus-alt-modifier-alist
  '(
    (hiragana-katakana . romaji)
    (zenkaku-hankaku . kanji)
    (henkan . mode-switch)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key code table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar ibus-keyval-alist
  '(
    ;; *** Function keys ***********************************************
    (backspace . ?\xff08)
    (tab . ?\xff09)
    (linefeed . ?\xff0a)
    (clear . ?\xff0b)
    (return . ?\xff0d)
    (pause . ?\xff13)
;    (scroll-lock . ?\xff14)
;    (sys-req . ?\xff15)
    (escape . ?\xff1b)
    (delete . ?\xffff)
    ;; *** International & multi-key character composition *************
;    (multi-key . ?\xff20)
;    (codeinput . ?\xff37)
;    (singlecandidate . ?\xff3c)
;    (multiplecandidate . ?\xff3d)
;    (previouscandidate . ?\xff3e)
    ;; Japanese keyboard support ***************************************
    (kanji . ?\xff21)
    (muhenkan . ?\xff22)
;    (henkan-mode . ?\xff23)
    (henkan . ?\xff23)
    (romaji . ?\xff24)
    (hiragana . ?\xff25)
    (katakana . ?\xff26)
    (hiragana-katakana . ?\xff27)
    (zenkaku . ?\xff28)
    (hankaku . ?\xff29)
    (zenkaku-hankaku . ?\xff2a)
    (touroku . ?\xff2b)
    (massyo . ?\xff2c)
    (kana-lock . ?\xff2d)
    (kana-shift . ?\xff2e)
    (eisu-shift . ?\xff2f)
    (eisu-toggle . ?\xff30)
;    (kanji-bangou . ?\xff37)
;    (zen-koho . ?\xff3d)
;    (mae-koho . ?\xff3e)
    ;; *** Cursor control & motion *************************************
    (home . ?\xff50)
    (left . ?\xff51)
    (up . ?\xff52)
    (right . ?\xff53)
    (down . ?\xff54)
    (prior . ?\xff55)
;    (page-up . ?\xff55)
    (next . ?\xff56)
;    (page-down . ?\xff56)
    (end . ?\xff57)
    (begin . ?\xff58)
    ;; *** Misc Functions **********************************************
    (select . ?\xff60)
    (print . ?\xff61)
    (execute . ?\xff62)
    (insert . ?\xff63)
    (undo . ?\xff65)
    (redo . ?\xff66)
    (menu . ?\xff67)
    (find . ?\xff68)
    (cancel . ?\xff69)
    (help . ?\xff6a)
    (break . ?\xff6b)
    (mode-switch . ?\xff7e) ; This key cannot be recognized to Emacs.
;    (num-lock . ?\xff7f)
    ;; *** Keypad ******************************************************
    (kp-space . ?\xff80)
    (kp-tab . ?\xff89)
    (kp-enter . ?\xff8d)
    (kp-f1 . ?\xff91)
    (kp-f2 . ?\xff92)
    (kp-f3 . ?\xff93)
    (kp-f4 . ?\xff94)
    (kp-home . ?\xff95)
    (kp-left . ?\xff96)
    (kp-up . ?\xff97)
    (kp-right . ?\xff98)
    (kp-down . ?\xff99)
    (kp-prior . ?\xff9a)
;    (kp-page-up . ?\xff9a)
    (kp-next . ?\xff9b)
;    (kp-page-down . ?\xff9b)
    (kp-end . ?\xff9c)
    (kp-begin . ?\xff9d)
    (kp-insert . ?\xff9e)
    (kp-delete . ?\xff9f)
    (kp-equal . ?\xffbd)
    (kp-multiply . ?\xffaa)
    (kp-add . ?\xffab)
    (kp-separator . ?\xffac)
    (kp-subtract . ?\xffad)
    (kp-decimal . ?\xffae)
    (kp-divide . ?\xffaf)
    (kp-0 . ?\xffb0)
    (kp-1 . ?\xffb1)
    (kp-2 . ?\xffb2)
    (kp-3 . ?\xffb3)
    (kp-4 . ?\xffb4)
    (kp-5 . ?\xffb5)
    (kp-6 . ?\xffb6)
    (kp-7 . ?\xffb7)
    (kp-8 . ?\xffb8)
    (kp-9 . ?\xffb9)
    ;; *** Auxilliary functions ****************************************
    (f1 . ?\xffbe)
    (f2 . ?\xffbf)
    (f3 . ?\xffc0)
    (f4 . ?\xffc1)
    (f5 . ?\xffc2)
    (f6 . ?\xffc3)
    (f7 . ?\xffc4)
    (f8 . ?\xffc5)
    (f9 . ?\xffc6)
    (f10 . ?\xffc7)
    (f11 . ?\xffc8)
    (f12 . ?\xffc9)
    (f13 . ?\xffca)
    (f14 . ?\xffcb)
    (f15 . ?\xffcc)
    (f16 . ?\xffcd)
    (f17 . ?\xffce)
    (f18 . ?\xffcf)
    (f19 . ?\xffd0)
    (f20 . ?\xffd1)
    (f21 . ?\xffd2)
    (f22 . ?\xffd3)
    (f23 . ?\xffd4)
    (f24 . ?\xffd5)
    (f25 . ?\xffd6)
    (f26 . ?\xffd7)
    (f27 . ?\xffd8)
    (f28 . ?\xffd9)
    (f29 . ?\xffda)
    (f30 . ?\xffdb)
    (f31 . ?\xffdc)
    (f32 . ?\xffdd)
    (f33 . ?\xffde)
    (f34 . ?\xffdf)
    (f35 . ?\xffe0)
    ;; *** Modifier keys ***********************************************
;    (shift-l . ?\xffe1)
;    (shift-r . ?\xffe2)
;    (control-l . ?\xffe3)
;    (control-r . ?\xffe4)
;    (caps-lock . ?\xffe5)
    (capslock . ?\xffe5)
;    (shift-lock . ?\xffe6)
;    (meta-l . ?\xffe7)
;    (meta-r . ?\xffe8)
;    (alt-l . ?\xffe9)
;    (alt-r . ?\xffea)
;    (super-l . ?\xffeb)
;    (super-r . ?\xffec)
;    (hyper-l . ?\xffed)
;    (hyper-r . ?\xffee)
    ;; *** ISO 9995 function and modifier keys *************************
;    (iso-lock . ?\xfe01)
;    (iso-level2-latch . ?\xfe02)
;    (iso-level3-shift . ?\xfe03)
;    (iso-level3-latch . ?\xfe04)
;    (iso-level3-lock . ?\xfe05)
;    (iso-group-shift . ?\xff7e)
;    (iso-group-latch . ?\xfe06)
;    (iso-group-lock . ?\xfe07)
;    (iso-next-group . ?\xfe08)
;    (iso-next-group-lock . ?\xfe09)
;    (iso-prev-group . ?\xfe0a)
;    (iso-prev-group-lock . ?\xfe0b)
;    (iso-first-group . ?\xfe0c)
;    (iso-first-group-lock . ?\xfe0d)
;    (iso-last-group . ?\xfe0e)
;    (iso-last-group-lock . ?\xfe0f)
;    (iso-left-tab . ?\xfe20)
    (iso-lefttab . ?\xfe20)
    (iso-move-line-up . ?\xfe21)
    (iso-move-line-down . ?\xfe22)
    (iso-partial-line-up . ?\xfe23)
    (iso-partial-line-down . ?\xfe24)
    (iso-partial-space-left . ?\xfe25)
    (iso-partial-space-right . ?\xfe26)
    (iso-set-margin-left . ?\xfe27)
    (iso-set-margin-right . ?\xfe28)
    (iso-release-margin-left . ?\xfe29)
    (iso-release-margin-right . ?\xfe2a)
    (iso-release-both-margins . ?\xfe2b)
    (iso-fast-cursor-left . ?\xfe2c)
    (iso-fast-cursor-right . ?\xfe2d)
    (iso-fast-cursor-up . ?\xfe2e)
    (iso-fast-cursor-down . ?\xfe2f)
    (iso-continuous-underline . ?\xfe30)
    (iso-discontinuous-underline . ?\xfe31)
    (iso-emphasize . ?\xfe32)
    (iso-center-object . ?\xfe33)
    (iso-enter . ?\xfe34)
    ;; *** Lispy accent keys *******************************************
    (dead-grave . ?\xfe50)
    (dead-acute . ?\xfe51)
    (dead-circumflex . ?\xfe52)
    (dead-tilde . ?\xfe53)
    (dead-macron . ?\xfe54)
    (dead-breve . ?\xfe55)
    (dead-abovedot . ?\xfe56)
    (dead-diaeresis . ?\xfe57)
    (dead-abovering . ?\xfe58)
    (dead-doubleacute . ?\xfe59)
    (dead-caron . ?\xfe5a)
    (dead-cedilla . ?\xfe5b)
    (dead-ogonek . ?\xfe5c)
    (dead-iota . ?\xfe5d)
    (dead-voiced-sound . ?\xfe5e)
    (dead-semivoiced-sound . ?\xfe5f)
    (dead-belowdot . ?\xfe60)
    (dead-hook . ?\xfe61)
    (dead-horn . ?\xfe62)
    ;; *** Katakana ****************************************************
    (overline . ?\x47e)
    (kana-fullstop . ?\x4a1)
    (kana-openingbracket . ?\x4a2)
    (kana-closingbracket . ?\x4a3)
    (kana-comma . ?\x4a4)
    (kana-conjunctive . ?\x4a5)
;    (kana-middledot . ?\x4a5)
    (kana-WO . ?\x4a6)
    (kana-a . ?\x4a7)
    (kana-i . ?\x4a8)
    (kana-u . ?\x4a9)
    (kana-e . ?\x4aa)
    (kana-o . ?\x4ab)
    (kana-ya . ?\x4ac)
    (kana-yu . ?\x4ad)
    (kana-yo . ?\x4ae)
    (kana-tsu . ?\x4af)
;    (kana-tu . ?\x4af)
    (prolongedsound . ?\x4b0)
    (kana-A . ?\x4b1)
    (kana-I . ?\x4b2)
    (kana-U . ?\x4b3)
    (kana-E . ?\x4b4)
    (kana-O . ?\x4b5)
    (kana-KA . ?\x4b6)
    (kana-KI . ?\x4b7)
    (kana-KU . ?\x4b8)
    (kana-KE . ?\x4b9)
    (kana-KO . ?\x4ba)
    (kana-SA . ?\x4bb)
    (kana-SHI . ?\x4bc)
    (kana-SU . ?\x4bd)
    (kana-SE . ?\x4be)
    (kana-SO . ?\x4bf)
    (kana-TA . ?\x4c0)
    (kana-CHI . ?\x4c1)
;    (kana-TI . ?\x4c1)
    (kana-TSU . ?\x4c2)
;    (kana-TU . ?\x4c2)
    (kana-TE . ?\x4c3)
    (kana-TO . ?\x4c4)
    (kana-NA . ?\x4c5)
    (kana-NI . ?\x4c6)
    (kana-NU . ?\x4c7)
    (kana-NE . ?\x4c8)
    (kana-NO . ?\x4c9)
    (kana-HA . ?\x4ca)
    (kana-HI . ?\x4cb)
    (kana-FU . ?\x4cc)
;    (kana-HU . ?\x4cc)
    (kana-HE . ?\x4cd)
    (kana-HO . ?\x4ce)
    (kana-MA . ?\x4cf)
    (kana-MI . ?\x4d0)
    (kana-MU . ?\x4d1)
    (kana-ME . ?\x4d2)
    (kana-MO . ?\x4d3)
    (kana-YA . ?\x4d4)
    (kana-YU . ?\x4d5)
    (kana-YO . ?\x4d6)
    (kana-RA . ?\x4d7)
    (kana-RI . ?\x4d8)
    (kana-RU . ?\x4d9)
    (kana-RE . ?\x4da)
    (kana-RO . ?\x4db)
    (kana-WA . ?\x4dc)
    (kana-N . ?\x4dd)
    (voicedsound . ?\x4de)
    (semivoicedsound . ?\x4df)
;    (kana-switch . ?\xFF7E)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode management
(defcustom ibus-mode nil
  "Toggle ibus-mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `ibus-mode'."
  :set 'custom-set-minor-mode
  :initialize 'custom-initialize-default
  :version "22.1"
  :type 'boolean
  :group 'ibus
  :require 'ibus)

;; Hook variables
(defvar ibus-set-commit-string-hook nil)
(defvar ibus-commit-string-hook nil)
(defvar ibus-preedit-show-hook nil)

;; Manage key bindings
(defvar ibus-mode-map nil)
(defvar ibus-mode-preedit-map nil)
(defvar ibus-mode-common-map nil)
(defvar ibus-mode-ja-onbiki-map nil)
(defvar ibus-mode-minimum-map nil)
(defvar ibus-mode-map-alist nil)
(defvar ibus-mode-map-disabled nil)
(make-variable-buffer-local 'ibus-mode-map-disabled)
(defvar ibus-mode-map-prev-disabled nil)
(make-variable-buffer-local 'ibus-mode-map-prev-disabled)
(put 'ibus-mode-map-prev-disabled 'permanent-local t)
(defvar ibus-ja-onbiki-prev-x-keysym nil)
(defvar ibus-keyboard-layout nil)
(defvar ibus-keymap-overlay nil)

;; Communication & buffer editing
(defvar ibus-agent-process nil)
(defvar ibus-agent-process-alist nil)
(defvar ibus-callback-queue nil)
(defvar ibus-selected-display nil)
(defvar ibus-last-command-event nil)
(defvar ibus-current-buffer nil)
(defvar ibus-selected-frame nil)
(defvar ibus-frame-focus nil)
(defvar ibus-focus-update-timer nil)
(defvar ibus-string-insertion-failed nil)
(defvar ibus-last-rejected-event nil)
(defvar ibus-last-command nil)

;; IMContexts
(defvar ibus-buffer-group nil)
(make-variable-buffer-local 'ibus-buffer-group)
(put 'ibus-buffer-group 'permanent-local t)
;; Memo:
;;  Each element of `ibus-buffer-group-alist' is a list:
;;  (GROUP IMCONTEXT-ID-ALIST IMCONTEXT-STATUS-ALIST BUFFER-LIST)
;;  GROUP is group identifier which is an object comparable by `eq'
;;  IMCONTEXT-ID-ALIST is an alist of number such as 12
;;  IMCONTEXT-STATUS-ALIST is an alist of string or nil, where string
;;  indicates input method engine
;;  BUFFER-LIST is a list of buffers which belong to this group
;; Example:
;;  ((#<buffer text.txt>
;;          ((":1.0" . 1)
;;           (":0.0" . 20))
;;          ((":1.0" . "anthy")
;;           (":0.0" . nil))
;;          (#<buffer text.txt>))
;;   (#<buffer *scratch*>
;;          ((":1.0" . 2)
;;           (":0.0" . 19))
;;          ((":1.0" . nil)
;;           (":0.0" . "skk"))
;;          (#<buffer *scratch*>)))
(defvar ibus-buffer-group-alist nil)
(defvar ibus-imcontext-id nil)
(defvar ibus-imcontext-status nil)
(defvar ibus-preediting-p nil)
(defvar ibus-preedit-point (make-marker))
(defvar ibus-preedit-update nil)
(defvar ibus-preedit-shown nil)
(defvar ibus-preedit-text "")
(defvar ibus-preedit-prev-text "")
(defvar ibus-preedit-curpos 0)
(defvar ibus-preedit-prev-curpos 0)
(defvar ibus-preedit-attributes nil)
(defvar ibus-preedit-prev-attributes nil)
(defvar ibus-preedit-default-attr nil)
(defvar ibus-preedit-overlays nil)
(defvar ibus-auxiliary-text "")
(defvar ibus-auxiliary-shown nil)
(defvar ibus-saved-frame-coordinates '(0 . 0))
(defvar ibus-surrounding-text-modified nil)
(defvar ibus-cursor-type-saved 0)
(make-variable-buffer-local 'ibus-cursor-type-saved)

;; Minibuffer
(defvar ibus-parent-buffer-group nil)
(defvar ibus-force-inherit-im nil)
(defvar ibus-isearch-buffer-group nil)
(defvar ibus-isearch-minibuffer nil)

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
	  (insert log-str ?\n)
	  (if window (recenter -1))
	  log-str)))))

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
;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibus-decode-event (event)
  ;; Convert Emacs event to ibus keyval
  (let ((modmask (apply 'logior
			(mapcar (lambda (mod)
				  (cdr (assq mod ibus-modifier-alist)))
				(event-modifiers event))))
	(keyval (event-basic-type event)))
    (if (numberp keyval)
	(if (and (not (zerop (logand modmask 1))) ; 1 = 2^0 => Shift keys
		 (>= keyval ?a)
		 (<= keyval ?z))
	    (setq keyval (- keyval 32)))
      (if (not (zerop (logand modmask 8))) ; 8 = 2^3 => Alt keys
	  (setq keyval
		(or (cdr (assq keyval ibus-alt-modifier-alist))
		    keyval)))
      (setq keyval (or (and ibus-use-ja-onbiki-key
			    ibus-ja-onbiki-key-symbol
			    (eq keyval ibus-ja-onbiki-key-symbol)
			    ?\xa5) ; ¥
		       (cdr (assq keyval ibus-keyval-alist))
		       keyval)))
    (cons keyval modmask)))

(defun ibus-encode-event (keyval modmask)
  ;; Convert ibus keyval to Emacs event
  (let ((bas (or (car (rassq keyval ibus-keyval-alist))
		 (if (< keyval 128) keyval)))
	(mods nil)
	mask1 mod1)
    (if (and ibus-use-ja-onbiki-key
	     ibus-ja-onbiki-key-symbol
	     (eq bas ?\xa5) ; ¥
	     (not ibus-mode-map-prev-disabled))
	(setq bas ibus-ja-onbiki-key-symbol))
    (if (logand modmask 8) ; 8 = 2^3 => Alt keys
	(setq bas (or (car (rassq bas ibus-alt-modifier-alist))
		      bas)))
    (when bas
      (dotimes (i 28)
	(if (and (not (zerop (setq mask1 (logand modmask (lsh 1 i)))))
		 (setq mod1 (rassq mask1 ibus-modifier-alist)))
	    (push (car mod1) mods)))
      (event-convert-list (nconc mods (list bas))))))

(defun ibus-null-command ()
  (interactive)
  (ibus-log "dummy event")
  (when (interactive-p)
    (setq this-command last-command)
    (setq unread-command-events
	  (delq 'ibus-dummy-event unread-command-events))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer-undo-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibus-insert-and-modify-undo-list (str)
  (let* ((prev-list (if (car-safe buffer-undo-list)
			buffer-undo-list
		      (cdr-safe buffer-undo-list)))
	 (prev (car-safe prev-list))
	 (consp-prev (and (consp prev)
			  (integerp (car prev))
			  (integerp (cdr prev))))
	 (consecutivep (and consp-prev
			   (= (cdr prev) (point))
			   (not (= (preceding-char) ?\n))
			   (<= (+ (string-width (buffer-substring-no-properties
						 (car prev) (point)))
				  (string-width str))
			       20)))) ; max 20 columns
    (ibus-log-undo-list "previous undo list")
    (when (and consp-prev
	       (integerp (car (cdr prev-list))))
      (ibus-log-undo-list "get rid of point setting entry")
      (setcdr prev-list (cdr (cdr prev-list))))
    (insert-and-inherit str)
    (ibus-log-undo-list "insert string: %S" str)
    (when (integerp (car (cdr-safe buffer-undo-list)))
      (ibus-log-undo-list "get rid of point setting entry")
      (setcdr buffer-undo-list (cdr (cdr buffer-undo-list))))
    (if (and consecutivep
	     (eq (cdr (cdr buffer-undo-list)) prev-list))
	(when (eq ibus-last-command 'self-insert-command)
	  (ibus-log-undo-list "unify consecutive insertion entries")
	  (setcar (car buffer-undo-list) (car (car prev-list)))
	  (setcdr buffer-undo-list (cdr prev-list)))
      (when (and (> (string-width str) 20)
		 (listp buffer-undo-list)) ; Undo enabled?
	(let ((beg (car (car buffer-undo-list)))
	      (end (cdr (car buffer-undo-list)))
	      (new-list (cdr buffer-undo-list)))
	  (ibus-log-undo-list "divide long insertion entry")
	  (while (let ((len (length (truncate-string-to-width str 20))))
		   (setq new-list (cons nil (cons (cons beg (+ beg len)) new-list))
			 beg (+ beg len)
			 str (substring-no-properties str len))
		   (> (string-width str) 20)))
	  (setq buffer-undo-list (cons (cons beg end) new-list)))))))

;; Advices for commands which conflict with preediting

(defun ibus-defadvice-disable-for-preedit ()
  (mapc (lambda (command)
	  (eval
	   `(defadvice ,command
	      (around ,(intern (concat "ibus-inhibit-" (symbol-name command))) ())
	      (if ibus-preediting-p
		  (error "iBus: `%s' cannot be used while preediting!" ',command)
		ad-do-it))))
	ibus-preedit-incompatible-commands))

(defun ibus-activate-advices-disable-for-preedit (enable)
  (with-no-warnings
    (if enable
	(ad-enable-regexp "^ibus-inhibit-")
      (ad-disable-regexp "^ibus-inhibit-"))
    (ad-activate-regexp "^ibus-inhibit-")))

;; Advices for yasnippet (version < 0.6)

(mapc (lambda (command)
	(eval
	 `(defadvice ,command
	    (around ,(intern (concat "ibus-inhibit-" (symbol-name command))) ())
	    (unless ibus-preediting-p
	      ad-do-it))))
      '(yas/field-undo-before-hook
	yas/check-cleanup-snippet
	yas/field-undo-after-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibus-set-mode-map-alist ()
  (setq ibus-mode-map-alist
	(list (cons 'ibus-mode ibus-mode-map)
	      (cons 'ibus-preediting-p ibus-mode-preedit-map))))

(defun ibus-set-keymap-parent ()
  (set-keymap-parent ibus-mode-map
		     (cond
		      ((or (not ibus-frame-focus)
			   ibus-mode-map-prev-disabled)
		       (ibus-log "use empty keymap")
		       nil)
		      (ibus-imcontext-status
		       (ibus-log "use common keymap")
		       ibus-mode-common-map)
		      (t
		       (ibus-log "use minimum keymap")
		       ibus-mode-minimum-map))))

(defun ibus-enable-ja-onbiki-key (&optional keysym)
  (unless keysym (setq keysym ibus-ja-onbiki-x-keysym))
  (when keysym
    (ibus-log "enable Kana-RO key: %s" keysym)
    (shell-command-to-string
     (concat "xmodmap -pke | sed -n 's/\\(\\(=\\) backslash bar\\| backslash bar$\\)/\\2 "
	     keysym " bar/gp' | xmodmap -"))
    (setq ibus-ja-onbiki-prev-x-keysym keysym)))

(defun ibus-disable-ja-onbiki-key (&optional keysym)
  (unless keysym (setq keysym ibus-ja-onbiki-prev-x-keysym))
  (when keysym
    (ibus-log "disable Kana-RO key: %s" keysym)
    (shell-command-to-string
     (concat "xmodmap -pke | sed -n 's/\\(\\(=\\) " keysym " bar\\| " keysym
	     " bar$\\)/\\2 backslash bar/gp' | xmodmap -"))
    (setq ibus-ja-onbiki-prev-x-keysym nil)))

(defun ibus-get-keyboard-layout ()
  (let ((xkb-rules (x-window-property "_XKB_RULES_NAMES" nil "STRING" 0 nil nil)))
    (if xkb-rules
	(intern (cadr (split-string xkb-rules "\0" t))))))

(defun ibus-update-ja-onbiki-key (&optional inhibit delayed)
  (when (eq ibus-keyboard-layout 'jp106)
    (if (and (not inhibit)
	     ibus-imcontext-status
	     ibus-use-ja-onbiki-key
	     ibus-ja-onbiki-x-keysym
	     ibus-frame-focus
	     (not ibus-mode-map-prev-disabled))
	(if delayed
	    (let ((cycle ibus-focus-update-interval))
	      (run-at-time (+ cycle 0.1) nil 'ibus-enable-ja-onbiki-key))
	  (ibus-enable-ja-onbiki-key))
      (ibus-disable-ja-onbiki-key))))

(defun ibus-switch-keymap (enable)
  (if (and (not enable)
	   ibus-preediting-p)
      (ibus-abort-preedit))
  (setq ibus-mode-map-prev-disabled (not enable))
  (ibus-update-cursor-color)
  (ibus-set-keymap-parent)
  (if (and ibus-imcontext-status
	   ibus-use-ja-onbiki-key
	   ibus-ja-onbiki-x-keysym
	   ibus-frame-focus)
      (ibus-update-ja-onbiki-key)))

(defun ibus-enable-keymap ()
  (interactive)
  (if ibus-mode
      (ibus-switch-keymap t))
  (setq ibus-mode-map-disabled nil))

(defun ibus-disable-keymap ()
  (interactive)
  (if ibus-mode
      (ibus-switch-keymap nil))
  (setq ibus-mode-map-disabled t))

(defun ibus-check-major-mode ()
  (if (memq major-mode ibus-incompatible-major-modes)
      (ibus-disable-keymap)))

(defun ibus-make-keymap-internal (keys &optional parent &rest ranges)
  (let ((map (if ranges (make-keymap) (make-sparse-keymap))))
    (if parent (set-keymap-parent map parent))
    (while ranges
      (let ((i (caar ranges))
	    (max (cdar ranges)))
	(while (<= i max)
	  (define-key map (char-to-string i) 'ibus-handle-event)
	  (setq i (1+ i))))
      (setq ranges (cdr ranges)))
    (while keys
      (let* ((key (reverse (car keys)))
	     (bas (car key))
	     (mods (cdr key)))
	(if (stringp bas)
	    (setq bas (string-to-char bas)))
	(when (memq 'alt mods)
	  (unless ibus-meta-key-exists
	    (setq mods (cons 'meta (delq 'alt mods))))
	  (setq bas (or (car (rassq bas ibus-alt-modifier-alist))
			bas)))
	(define-key map (vector (nconc mods (list bas))) 'ibus-handle-event)
	(setq keys (cdr keys))))
    map))

(defun ibus-combine-modifiers (base modifiers)
  (if modifiers
      (apply 'nconc
	     (mapcar
	      (lambda (k) (list (cons (car modifiers) k) k))
	      (ibus-combine-modifiers base (cdr modifiers))))
    (list (list base))))

(defun ibus-make-minimum-map ()
  (ibus-make-keymap-internal ibus-common-function-key-list))

(defun ibus-make-ja-onbiki-map ()
  (ibus-make-keymap-internal (if (and ibus-use-ja-onbiki-key
				      ibus-ja-onbiki-key-symbol)
				 (ibus-combine-modifiers
				  ibus-ja-onbiki-key-symbol
				  '(meta control hyper super alt)))
			     ibus-mode-minimum-map))

(defun ibus-make-common-map ()
  (ibus-make-keymap-internal nil
			     ibus-mode-ja-onbiki-map
			     '(32 . 126)))

(defun ibus-make-preedit-map ()
  (ibus-make-keymap-internal ibus-preedit-function-key-list
			     nil
			     '(0 . 26) '(28 . 31)))

(defun ibus-update-key-bindings (&optional symbol)
  (when (and ibus-frame-focus
	     (not ibus-mode-map-prev-disabled)
	     (or (null symbol)
		 (and (eq symbol 'ibus-use-ja-onbiki-key)
		      ibus-imcontext-status
		      ibus-ja-onbiki-x-keysym)
		 (and (eq symbol 'ibus-ja-onbiki-x-keysym)
		      ibus-imcontext-status
		      ibus-use-ja-onbiki-key)))
    (when (memq symbol '(nil ibus-ja-onbiki-x-keysym))
      (ibus-update-ja-onbiki-key t))
    (ibus-update-ja-onbiki-key))
  (when (null symbol)
    (ibus-log "update ibus-mode-minimum-map")
    (if (keymapp ibus-mode-minimum-map)
	(setcdr ibus-mode-minimum-map (cdr (ibus-make-minimum-map)))
      (setq ibus-mode-minimum-map (ibus-make-minimum-map))))
  (when (memq symbol '(nil ibus-use-ja-onbiki-key ibus-ja-onbiki-key-symbol))
    (ibus-log "update ibus-mode-ja-onbiki-map")
    (if (keymapp ibus-mode-ja-onbiki-map)
	(setcdr ibus-mode-ja-onbiki-map (cdr (ibus-make-ja-onbiki-map)))
      (setq ibus-mode-ja-onbiki-map (ibus-make-ja-onbiki-map))))
  (when (memq symbol '(nil ibus-common-function-key-list))
    (ibus-log "update ibus-mode-common-map")
    (if (keymapp ibus-mode-common-map)
	(setcdr ibus-mode-common-map (cdr (ibus-make-common-map)))
      (setq ibus-mode-common-map (ibus-make-common-map))))
  (when (null symbol)
    (ibus-log "update ibus-mode-map")
    (unless (keymapp ibus-mode-map)
      (setq ibus-mode-map (make-sparse-keymap)))
    (define-key ibus-mode-map [ibus-receive-event] 'ibus-exec-callback)
    (define-key ibus-mode-map [ibus-dummy-event] 'ibus-null-command)
    (ibus-set-keymap-parent))
  (when (memq symbol '(nil ibus-preedit-function-key-list))
    (ibus-log "update ibus-mode-preedit-map")
    (if (keymapp ibus-mode-preedit-map)
	(setcdr ibus-mode-preedit-map (cdr (ibus-make-preedit-map)))
      (setq ibus-mode-preedit-map (ibus-make-preedit-map)))))

(defun ibus-define-key (symbol keys handle)
  ;; If keys is given as an array, it doesn't indicate key sequence,
  ;; but multiple definitions of single keystroke.
  (let ((keys-list (if (arrayp keys)
		       (listify-key-sequence keys)
		     (list keys))))
    (while keys-list
      (let ((key (car keys-list)))
	(if (listp key)
	    (let* ((n (1- (length key)))
		   (bas (nth n key)))
	      ;; If the key event is specified by a list and the last
	      ;; element is given as a string, the code number for the first
	      ;; character of the string is used for an event basic type.
	      (when (stringp bas)
		(setq key (copy-sequence key))
		(setcar (nthcdr n key) (string-to-char bas)))
	      (setq key (event-convert-list key))))
	;; In Emacs 22, the function `event-modifiers' cannot return the
	;; correct value until the symbol is parsed.
	(key-binding (vector key))
	;; It is necessary to call a function `event-basic-type' after
	;; `event-modifiers' because `event-basic-type' uses the symbol
	;; property `event-symbol-elements' added by `event-modifiers'
	;; when event is given as a symbol.
	(let ((modifiers (event-modifiers key))
	      (keyval (event-basic-type key)))
	  (if (integerp keyval)
	      (setq keyval (char-to-string keyval)
		    modifiers (reverse modifiers)))
	  (setq key (append modifiers (list keyval))))
	(if handle
	    (add-to-list symbol key)
	  (set symbol (delete key (symbol-value symbol)))))
      (setq keys-list (cdr keys-list)))
    (symbol-value symbol))) ; Return value

(defun ibus-define-common-key (key handle)
  "Specify which key events iBus anytime takes over. If HANDLE
is non-nil, iBus handles the key events given by KEY. When KEY is
given as an array, it doesn't indicate key sequence, but multiple
definitions of single keystroke.
 It is necessary to call a function `ibus-update-key-bindings' or
restart ibus-mode so that this settings may become effective."
  (ibus-define-key 'ibus-common-function-key-list key handle))

(defun ibus-define-preedit-key (key handle)
  "Specify which key events iBus takes over when preediting. If
HANDLE is non-nil, iBus handles the key events given by KEY. When
KEY is given as an array, it doesn't indicate key sequence, but
multiple definitions of single keystroke.
 It is necessary to call a function `ibus-update-key-bindings' or
restart ibus-mode so that this settings may become effective."
  (ibus-define-key 'ibus-preedit-function-key-list key handle))

;; Advice for `describe-key'
(defadvice describe-key
  (around ibus-describe-key ())
  (cond
   ((not ibus-mode)
    ad-do-it)
   ((null ibus-mode-map-alist)
    ;; Translate Jananese kana RO key
    (if (commandp (lookup-key (ibus-make-keymap-internal
			       (ibus-combine-modifiers
				ibus-ja-onbiki-key-symbol
				'(meta control hyper super alt)))
			      key))
	(let ((mods (event-modifiers (aref key 0))))
	  (setq key (vector
		     (event-convert-list
		      (delq 'shift
			    (append mods (list (if (memq 'shift mods) 95 92)))))))))
    ad-do-it)
   (t
    ;; Set modified flag of *Help* buffer in order to detect
    ;; whether *Help* is updated or not.
    (with-current-buffer (help-buffer)
      (set-buffer-modified-p t))
    (if (eq (key-binding key) 'ibus-handle-event)
	;; Invoke `describe-key' without ibus-mode's keymaps
	(let ((ibus-mode-map-alist nil))
	  (if ibus-keymap-overlay
	      (overlay-put ibus-keymap-overlay 'keymap nil))
	  (setq unread-command-events
		(nconc (listify-key-sequence key) unread-command-events))
	  (call-interactively 'describe-key)
	  (if ibus-keymap-overlay
	      (overlay-put ibus-keymap-overlay 'keymap ibus-mode-preedit-map)))
      ad-do-it)
    ;; Add descriptions to *Help* buffer, if any
    (with-current-buffer (help-buffer)
      (let* ((raw (vector (aref (this-single-command-raw-keys) 0)))
	     (format (format "iBus: ibus-mode handles %s when %%s.\n"
			     (key-description raw)))
	     (preedit (lookup-key ibus-mode-preedit-map raw))
	     (common (lookup-key ibus-mode-common-map raw))
	     (minimum (lookup-key ibus-mode-minimum-map raw))
	     (inhibit-read-only t))
	(when (or preedit common minimum)
	  ;; Popup *Help* buffer if it was't updated
	  (if (or (= (buffer-size) 0)
		  (buffer-modified-p))
	      (with-output-to-temp-buffer (help-buffer)
		(princ (current-message))))
	  ;; Insert above [BACK] button
	  (goto-char (point-max))
	  (beginning-of-line 0)
	  ;; When *Help* is opened for the first time, [BACK] button doesn't appear
	  (unless (get-text-property (point) 'button)
	    (goto-char (point-max))
	    (insert "\n"))
	  (if (or preedit common)
	      (insert (format format "preediting")))
	  (if common
	      (insert (format format "iBus is active")))
	  (if minimum
	      (insert (format format "iBus is not active")))
	  (insert "\n")))))))

(defun ibus-activate-advice-describe-key (enable)
  (if enable
      (ad-enable-advice 'describe-key 'around 'ibus-describe-key)
    (ad-disable-advice 'describe-key 'around 'ibus-describe-key))
  (ad-activate 'describe-key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Displaying
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibus-get-x-display ()
  ;; Don't use (frame-parameter nil 'display) because
  ;; this expression returns nil if frame is on text-only terminal.
  (let ((env (getenv "DISPLAY")))
    (and env (let* ((display (substring env (string-match ":[0-9]+" env)))
		    (screen (and (not (string-match "\\.[0-9]+$" display)) ".0")))
	       (concat display screen)))))

(defun ibus-mode-line-string ()
  (let ((status (cdr (assoc ibus-selected-display
			    (nth 2 (assq ibus-buffer-group
					 ibus-buffer-group-alist))))))
    (concat " iBus" (and status (format "[%s]" status)))))

;; Cursor color

(defun ibus-set-cursor-color (&optional single-frame)
  (let ((color (cond
		;;    |      `ibus-cursor-color'     |   ON   |   OFF  |Disabled|
		;;    +------------------------------+--------+--------+--------+
		;;    |              nil             |  none  |  none  |  none  |
		((or (null ibus-cursor-color)
		     (null ibus-imcontext-id))
		 nil)
		;;    |           "color1"           | color1 |  none  |  none  |
		((stringp ibus-cursor-color)
		 (if ibus-imcontext-status
		     ibus-cursor-color))
		;;    |    ("color1" . "color2")     | color1 | color2 | color2 |
		((consp ibus-cursor-color)
		 (let ((tail (cdr ibus-cursor-color)))
		   (cond
		    ((stringp tail)
		     (if ibus-imcontext-status
			 (car ibus-cursor-color)
		       tail))
		    ;;|     ("color1" "color2")      | color1 | color2 |  none  |
		    ;;| ("color1" "color2" "color3") | color1 | color2 | color3 |
		    ((consp tail)
		     (if ibus-mode-map-prev-disabled
			 (cadr tail)
		       (if ibus-imcontext-status
			   (car ibus-cursor-color)
			 (car tail)))))))))
	(ac-fuzzy (with-no-warnings
		    ;; Fuzzy state of auto-complete-mode
		    (and (featurep 'auto-complete)
			 (bound-and-true-p ac-fuzzy-enable)
			 ac-fuzzy-cursor-color)))
	(viper (with-no-warnings
		 (and (featurep 'viper)
		      viper-mode
		      (eq viper-current-state 'insert-state))))
	(orig-frame (selected-frame)))
    (ibus-log "set cursor color: %S" color)
    (condition-case err
	(while (progn
		 (unless single-frame
		   (select-frame (next-frame nil t)))
		 (when (or (and (eq window-system 'x)
				(not ac-fuzzy)
				(eq (window-buffer (frame-selected-window))
				    ibus-current-buffer))
			   (not ibus-mode))
		   (unless color
		     (setq color (frame-parameter nil 'foreground-color)))
		   (if viper
		       (with-no-warnings
			 (setq viper-insert-state-cursor-color color)
			 (viper-set-cursor-color-according-to-state))
		     (set-cursor-color color)))
		 (not (eq (selected-frame) orig-frame))))
      (error
	(select-frame orig-frame)
	(ibus-message "Failed to set cursor color %S" err)))))

(defun ibus-update-cursor-color (&optional single-frame)
  (if (and ibus-cursor-color
	   (eq (selected-frame) ibus-selected-frame))
      (ibus-set-cursor-color single-frame)))

(defun ibus-after-make-frame-function (frame)
  (save-current-buffer
    (let ((old-frame (selected-frame)))
      (unwind-protect
	  (progn
	    (select-frame frame)
	    (set-buffer (window-buffer (frame-selected-window)))
	    (let ((ibus-selected-frame frame))
	      (ibus-update-cursor-color t)))
	(select-frame old-frame)))))

;; Cursor location

(defun ibus-frame-top-left-coordinates (&optional frame)
  "Return the pixel coordinates of FRAME as a cons cell (LEFT . TOP),
which are relative to top left corner of screen.

If FRAME is omitted, use selected-frame.

Users can also get the frame coordinates by referring the variable
`ibus-saved-frame-coordinates' just after calling this function."
  ;; Note: This function was imported from pos-tip.el ver. 0.0.3
  (with-current-buffer (get-buffer-create " *xwininfo*")
    (let ((case-fold-search nil))
      (buffer-disable-undo)
      (erase-buffer)
      (call-process shell-file-name nil t nil shell-command-switch
		    (concat "xwininfo -id " (frame-parameter frame 'window-id)))
      (goto-char (point-min))
      (search-forward "\n  Absolute")
      (setq ibus-saved-frame-coordinates
	    (cons (progn (string-to-number (buffer-substring-no-properties
					    (search-forward "X: ")
					    (line-end-position))))
		  (progn (string-to-number (buffer-substring-no-properties
					    (search-forward "Y: ")
					    (line-end-position)))))))))

(defun ibus-compute-pixel-position
  (&optional pos window frame-coordinates)
  "Return the absolute pixel coordinates of POS in WINDOW as a list like
\(X Y W H), here W and H are the pixel width and height of object at POS.

Omitting POS and WINDOW means use current position and selected window,
respectively.

If FRAME-COORDINATES is omitted, automatically obtain the absolute
coordinates of the top left corner of frame which WINDOW is on. Here,
`top left corner of frame' represents the origin of `window-pixel-edges'
and its coordinates are essential for calculating the return value. If
non-nil, specifies the frame location as a cons cell like (LEFT . TOP).
This option makes the calculations slightly faster, but can be used only
when it's clear that frame is in the specified position. Users can get
the previous values of frame coordinates by referring the variable
`ibus-saved-frame-coordinates'."
  (unless frame-coordinates
    (ibus-frame-top-left-coordinates
     (window-frame (or window (selected-window)))))
  (let* ((x-y (or (pos-visible-in-window-p (or pos (window-point window)) window t)
		  '(0 0)))
	 ;; `posn-object-width-height' returns an incorrect value
	 ;; when the header line is displayed (Emacs bug #4426).
	 (w-h (if header-line-format
		  (cons (frame-char-width) (frame-char-height))
		(posn-object-width-height
		 (posn-at-x-y (max (car x-y) 0) (cadr x-y)))))
	 (ax (+ (car ibus-saved-frame-coordinates)
		(car (window-inside-pixel-edges))
		(car x-y)))
	 (ay (+ (cdr ibus-saved-frame-coordinates)
		(cadr (window-pixel-edges))
		(cadr x-y))))
    (list ax ay (car w-h) (cdr w-h))))

;;; TODO: FIXME: Does anyone know how to get the actual character height
;;;              even if the header line is displayed?

(defun ibus-set-cursor-location ()
  (let* ((rect (ibus-compute-pixel-position
		(+ ibus-preedit-point ibus-preedit-curpos) nil
		ibus-saved-frame-coordinates)))
    (ibus-log "cursor position (x y w h): %s" rect)
    ;; Finite value of width seems to locate candidate window in incorrect position
    (ibus-agent-send "set_cursor_location(%d, %d, %d, 0, %d)" ibus-imcontext-id
		     (car rect) (cadr rect) (nth 3 rect)))) ; Send only

;; Frame input focuses

(defun ibus-get-active-window-id ()
  "Return the number of the window-system window which is foreground,
i.e. input focus is in this window."
  (let ((x-active-window (x-window-property "_NET_ACTIVE_WINDOW" nil "WINDOW" 0 nil t)))
    (if x-active-window
	;; It's possible that `x-active-window' take the value of 0. Why?
	(condition-case err
	    (+ (ash (car x-active-window) 16)  (cdr x-active-window))
	  (wrong-type-argument -1)))))

(defun ibus-change-x-display ()
  (let ((display (ibus-get-x-display)))
    (ibus-log "change display from %s to %s" ibus-selected-display display)
    (setq ibus-agent-process (cdr (assoc display ibus-agent-process-alist)))
    (if ibus-agent-process
	(setq ibus-selected-display display)
      (ibus-agent-start)
      (if (and ibus-agent-process
;	       (equal display ":0.0") ; debug code
	       (memq (process-status ibus-agent-process)
		     '(open run)))
	  (setq ibus-agent-process-alist (cons (cons display ibus-agent-process)
					       ibus-agent-process-alist)
		ibus-selected-display display)
	(ibus-mode-quit)
	(error "Unable to launch agent for display %S. Turned off ibus-mode" display)))))

(defun ibus-change-focus (focus-in)
  (ibus-agent-send (if focus-in "focus_in(%d)" "focus_out(%d)")
		   ibus-imcontext-id))

(defun ibus-check-frame-focus (&optional focus-in)
  (let* (active-win
	 redirect
	 (focused-p
	  (and (eq window-system 'x)
	       (setq active-win (ibus-get-active-window-id))
	       (or (eq active-win
		       (string-to-number
			(frame-parameter nil 'outer-window-id)))
		   (eq active-win
		       (and (setq redirect
				  (car (delq nil
					     (mapcar (lambda (frame)
						       (and (frame-focus frame)
							    frame))
						     (frame-list)))))
			    (string-to-number
			     (frame-parameter redirect 'outer-window-id)))))))
	 (new-focus (or (not ibus-frame-focus) focus-in)))
    (when (eq focused-p new-focus)
      (when (and (numberp ibus-imcontext-id)
		 (eq (current-buffer) ibus-current-buffer))
	(setq ibus-frame-focus new-focus)
	(ibus-log "change focus: %S" (and ibus-frame-focus (current-buffer)))
	(ibus-log "ibus-current-buffer: %S" ibus-current-buffer)
	(if ibus-frame-focus
	    (setq ibus-keyboard-layout (ibus-get-keyboard-layout)))
	(when (and ibus-use-ja-onbiki-key
		   ibus-ja-onbiki-x-keysym)
	  (ibus-update-ja-onbiki-key nil t))
	(ibus-set-keymap-parent)
	(ibus-change-focus ibus-frame-focus) ; Send only
	(when ibus-frame-focus
	  (ibus-frame-top-left-coordinates)))
      (unless (or focus-in
		  (memq 'ibus-receive-event unread-command-events))
	;; Set dummy event as a trigger of `post-command-hook'
	(setq unread-command-events
	      (cons 'ibus-dummy-event
		    (delq 'ibus-dummy-event unread-command-events)))))))

(defun ibus-cancel-focus-update-timer ()
  (when ibus-focus-update-timer
    (cancel-timer ibus-focus-update-timer)
    (setq ibus-focus-update-timer nil)))

(defun ibus-start-focus-observation ()
  (let ((cycle (if (or ibus-mode-map-prev-disabled
		       (not ibus-imcontext-status)
		       (not ibus-frame-focus))
		   ibus-focus-update-interval-long
		 ibus-focus-update-interval)))
    (ibus-cancel-focus-update-timer)
    (setq ibus-focus-update-timer
	  (run-at-time cycle cycle 'ibus-check-frame-focus))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preediting area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibus-reset-imcontext ()
  (ibus-enable ibus-imcontext-status))

(defun ibus-remove-preedit (&optional abort)
  (remove-hook 'before-change-functions 'ibus-before-change-function)
  (unless (or (string= ibus-preedit-prev-text "")
	      abort)
    (let ((pos ibus-preedit-point)
	  (inhibit-read-only t)
	  (inhibit-modification-hooks t))
      (condition-case err
	  (progn
	    (if (consp buffer-undo-list)
		;; `buffer-undo-list' contains undo information
		(progn
		  (let ((undo-in-progress t))
		    (setq buffer-undo-list (primitive-undo 2 buffer-undo-list)))
		  ;; Restore modification flag of yasnippet field at cursor position
		  (if (boundp 'yas/active-field-overlay)
		      ;; yasnippet version >= 0.6
		      (when (memq yas/active-field-overlay (overlays-at pos))
			(dont-compile ; To avoid byte-compile error
			  (setf (yas/field-modified-p
				 (overlay-get yas/active-field-overlay 'yas/field))
				nil)))
		    ;; yasnippet version < 0.6
		    (mapc (lambda (overlay)
			    (when (overlay-get overlay 'yas/modified?)
			      (overlay-put
			       overlay 'yas/modified?
			       (overlay-get overlay 'ibus-saved-yas/modified?))))
			  (overlays-at pos))))
	      ;; Undo disabled or `buffer-undo-list' is empty
	      (let (buffer-undo-list)
		(delete-region pos (+ pos (length ibus-preedit-prev-text)))))
	    (undo-boundary)
	    (goto-char pos)
	    ;; Invoke function bound to `point-entered' text property
	    (let ((func (get-text-property pos 'point-entered)))
	      (when func
		(funcall func))))
	(error
	 (ibus-message "Failed to delete preediting text %S" err)))))
  (mapc 'delete-overlay ibus-preedit-overlays)
  (when (local-variable-p 'ibus-cursor-type-saved)
    (if (eq ibus-cursor-type-saved 1)
	(kill-local-variable 'cursor-type)
      (setq cursor-type ibus-cursor-type-saved))
    (kill-local-variable 'ibus-cursor-type-saved))
  (setq ibus-preedit-prev-text ""
	ibus-preedit-prev-curpos 0
	ibus-preedit-prev-attributes nil
	ibus-preedit-overlays nil)
  (set-marker ibus-preedit-point nil)
  (when ibus-keymap-overlay
    (delete-overlay ibus-keymap-overlay)
    (setq ibus-keymap-overlay nil))
  (setq ibus-preediting-p nil))

(defun ibus-cleanup-preedit (&optional abort)
  (ibus-remove-preedit abort)
  (ibus-log "cleanup preedit")
  (setq ibus-preedit-update nil
	ibus-preedit-shown nil
	ibus-preedit-text ""
	ibus-preedit-curpos 0
	ibus-preedit-attributes nil
	ibus-committed-text ""))

(defun ibus-show-preedit (&optional resume)
  (setq ibus-preedit-update nil)
  (if resume (setq ibus-surrounding-text-modified t))
  (let* ((text ibus-preedit-text)
	 (attrs ibus-preedit-attributes)
	 (empty (or (not ibus-preedit-shown)
		    (string= text ""))))
    (cond
     ;; isearch-mode
     ((and ibus-isearch-minibuffer
	   ibus-surrounding-text-modified
	   (not empty))
      (ibus-log "preediting text not shown")
      (add-to-list 'unread-command-events 'ibus-resume-preedit))
     ;; IMContext is empty or invisible
     (empty
      (ibus-cleanup-preedit))
     ;; IMContext contains preedit string
     (resume
      (setq ibus-preedit-update t))
     ((not (and ibus-preediting-p
		(string= text ibus-preedit-prev-text)
		(= ibus-preedit-curpos ibus-preedit-prev-curpos)
		(equal attrs ibus-preedit-prev-attributes)))
      (if ibus-preediting-p
	  (ibus-remove-preedit)
	(if (eq window-system 'x)
	    (ibus-frame-top-left-coordinates))
	(unless ibus-surrounding-text-modified
	  (ibus-log "cleanup base attribute")
	  (setq ibus-preedit-default-attr nil)))
      ;; Put String
      (setq ibus-preediting-p (current-buffer))
      (setq ibus-keymap-overlay (make-overlay (point-min) (1+ (point-max)) nil nil t))
      (overlay-put ibus-keymap-overlay 'keymap ibus-mode-preedit-map)
      (overlay-put ibus-keymap-overlay 'priority 100) ; override yasnippet's keymap
      (set-marker ibus-preedit-point (point))
;      (ibus-log "current cursor position: %d" ibus-preedit-point)
      (mapc (lambda (overlay)
	      (when (overlay-get overlay 'yas/modified?)
		(overlay-put overlay 'ibus-saved-yas/modified? t)))
	    (overlays-at ibus-preedit-point))
      (undo-boundary)
      (condition-case err
	  (insert-and-inherit text)
	(text-read-only
	 (ibus-message "Failed to insert preediting text %S" err)
	 (ibus-cleanup-preedit)
	 (let ((ibus-string-insertion-failed nil))
	   (ibus-reset-imcontext))
	 (setq text ""
	       ibus-string-insertion-failed t)))
      (undo-boundary)
      (unless (string= text "")
	(setq ibus-preedit-prev-text text
	      ibus-preedit-prev-curpos ibus-preedit-curpos
	      ibus-preedit-prev-attributes attrs)
	;; Set attributes
;	(ibus-log "attributes: %s" attrs)
	(let ((max (length text))
	      flat-attr)
	  (setq ibus-preedit-overlays nil)
	  (when ibus-auxiliary-shown
	    (let ((overlay (make-overlay ibus-preedit-point
					 (+ ibus-preedit-point max))))
	      (overlay-put overlay 'after-string
			   (propertize ibus-auxiliary-text 'face 'tooltip))
	      (push overlay ibus-preedit-overlays)))
	  (while attrs
	    (let* ((type (pop attrs))
		   (value (pop attrs))
		   (beg (pop attrs))
		   (end (pop attrs))
		   face priority)
;	      (ibus-log "type: %s  val: %s  beg: %d  end: %d" type value begin end)
	      (if (cond ((eq type 'foreground)
			 (setq face (list :foreground (format "#%06x" value))
			       priority 50))
			((eq type 'background)
			 (setq face (list :background (format "#%06x" value))
			       priority 50))
			((eq type 'underline)
			 (setq face (list :underline (> value 0))
			       priority 100)))
		  (let ((overlay (make-overlay (+ ibus-preedit-point beg)
					       (+ ibus-preedit-point end))))
		    (overlay-put overlay 'face face)
		    (overlay-put overlay 'priority priority)
		    (push overlay ibus-preedit-overlays)
		    (setq flat-attr (if (and (listp flat-attr)
					     (eq beg 0) (eq end max))
					(cons face flat-attr)
				      t)))
		(ibus-message "Unable to set attribute %S %S." type value))))
	  ;; This modification hook must be registered as a global hook because
	  ;; local hooks might be reset when major mode is changed.
	  (add-hook 'before-change-functions 'ibus-before-change-function)
	  (setq flat-attr (or flat-attr
			      'none)
		ibus-preedit-default-attr (or ibus-preedit-default-attr
					      flat-attr))
	  (ibus-log "default attr: %S" ibus-preedit-default-attr)
	  (ibus-log "current attr: %S" flat-attr)
	  (if (or (eq flat-attr t)
		  (not (equal flat-attr ibus-preedit-default-attr))
		  ibus-auxiliary-shown)
	      ;; When conversion candidate is shown
	      (progn
		(unless (or (eq ibus-cursor-type-for-candidate 0)
			    (local-variable-p 'ibus-cursor-type-saved))
		  (setq ibus-cursor-type-saved
			(or (and (local-variable-p 'cursor-type) cursor-type)
			    1))) ; 1 means that global value has been used
		(if ibus-put-cursor-on-candidate
		    (goto-char (+ ibus-preedit-point ibus-preedit-curpos))))
	    ;; When the string is preedited or prediction window is shown
	    (goto-char (+ ibus-preedit-point ibus-preedit-curpos)))
	  (ibus-set-cursor-location))
	(run-hooks 'ibus-preedit-show-hook))
      ))))

(defun ibus-do-update-preedit ()
  (when ibus-preedit-update
    (ibus-log "preedit-update  win-buf: %S  cur-buf: %S  cmd-buf: %S  text: %S" (window-buffer) (current-buffer) ibus-current-buffer ibus-preedit-text)
    (ibus-show-preedit)))

(defun ibus-abort-preedit ()
  (when ibus-preediting-p
    (save-excursion
      (ibus-cleanup-preedit (not ibus-clear-preedit-when-unexpected-event)))
    (ibus-reset-imcontext)))

(defun ibus-before-change-function (&optional beg end)
  (when (eq (current-buffer) ibus-current-buffer)
    (ibus-log "buffer will be modified (beg:%s  end:%s)" beg end)
    (ibus-log "cursor positon: %s" (point))
    (unless (and (memq major-mode '(erc-mode
				    rcirc-mode
				    circe-server-mode
				    circe-channel-mode))
		 (memq this-command '(nil ibus-handle-event)))
      (ibus-abort-preedit))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start/stop agent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibus-agent-kill ()
  (let ((proc ibus-agent-process))
    (condition-case err
	(when (processp proc)
	  (set-process-sentinel proc nil)
	  (let ((buffer (process-buffer proc)))
	    (when (buffer-live-p buffer)
	      (with-current-buffer buffer
		(remove-hook 'after-change-functions
			     'ibus-agent-receive-passively t))
	      (kill-buffer buffer)))
	  (delete-process proc)
	  (ibus-log "process: %s  status: %s" proc (process-status proc))
	  )
      (error (ibus-message "%S: %S" (car err) (cdr err))))
    (setq ibus-agent-process nil)))

(defun ibus-agent-process-sentinel (proc stat)
  (ibus-message "process: %s  status: %s" proc (substring stat 0 -1))
  (ibus-mode-quit)
  (if (ibus-mode-on) ; Try to restart
      ;; Succeeded
      (progn
	(ibus-message "ibus-mode restarted")
	(ibus-check-current-buffer))
    ;; Failed
    (ibus-message "Agent was unexpectedly stopped. Turned off ibus-mode.")))

(defun ibus-agent-start-internal ()
  (let* ((display (ibus-get-x-display))
	 (buffer (progn
		   (string-match "\\(\\**\\)$" ibus-agent-buffer-name)
		   (replace-match (concat "(" display ")\\1")
				  t nil ibus-agent-buffer-name))))
    (condition-case err
	(if ibus-python-command
	    (start-process "ibus-agent" buffer
			   ibus-python-command ibus-agent-command)
	  (start-process "ibus-agent" buffer
			 ibus-agent-command))
      (error
       (ibus-message "%S: %S" (car err) (cdr err))
       nil))))

(defun ibus-agent-start ()
  (if (and (processp ibus-agent-process)
	   (not (memq (process-status ibus-agent-process) '(open run))))
      (ibus-agent-kill))
  (unless (and (processp ibus-agent-process)
	       (memq (process-status ibus-agent-process) '(open run)))
    (let ((proc (ibus-agent-start-internal)))
      (setq ibus-agent-process proc)
      (when (processp proc)
	(ibus-log "process: %s  status: %s" proc (process-status proc))
	;; `process-kill-without-query' is an obsolete function (as of Emacs 22.1)
;	(process-kill-without-query proc)
	(set-process-query-on-exit-flag proc nil)
	(set-process-coding-system proc 'utf-8 'utf-8)
	(set-process-sentinel proc 'ibus-agent-process-sentinel)
	(with-current-buffer (process-buffer proc)
	  (ibus-log "temp buffer: %S" (current-buffer))
	  (unless ibus-debug (buffer-disable-undo))
	  (erase-buffer)
	  ;; `make-local-hook' is an obsolete function (as of Emacs 21.1)
;	  (make-local-hook 'after-change-functions)
	  (add-hook 'after-change-functions
		    'ibus-agent-receive-passively nil t))))))

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
	    (ibus-process-signals repl passive)
	  (ibus-message "Couldn't receive data from agent."))))))

(defun ibus-agent-send-receive (string &rest objects)
  (and (apply 'ibus-agent-send string objects)
       (ibus-agent-receive)))

(defun ibus-agent-receive-passively (beg &optional end lng)
  (ibus-log "passively receive")
  (ibus-agent-receive t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Execute callbacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibus-exec-callback-1 (sexplist)
  (ibus-log "buffer: %s" (current-buffer))
  (ibus-log "socket: %s" ibus-agent-process)
  (ibus-log "display: %s" ibus-selected-display)
  (ibus-log "imcontext-id: %s" ibus-imcontext-id)
  (mapc (lambda (sexp)
	  (ibus-log "execute: %S" sexp)
	  (eval sexp))
	sexplist))

(defun ibus-exec-callback ()
  (interactive)
  (when (interactive-p)
    (unless (eq last-command 'ibus-handle-event)
      (setq ibus-string-insertion-failed nil))
    (setq this-command last-command
	  unread-command-events
	  (delq 'ibus-receive-event
		(delq 'ibus-dummy-event unread-command-events))))
  (when (buffer-live-p ibus-current-buffer)
    (with-current-buffer ibus-current-buffer
      (ibus-log "callback queue: %s" (pp-to-string ibus-callback-queue))
      (while ibus-callback-queue
	(let* ((queue (car ibus-callback-queue))
	       (ibus-agent-process (car queue))
	       (ibus-selected-display (car (rassoc ibus-agent-process
						   ibus-agent-process-alist)))
	       (group (assq ibus-buffer-group ibus-buffer-group-alist))
	       (ibus-imcontext-id (cdr (assoc ibus-selected-display
					      (cadr group))))
	       (ibus-imcontext-status (cdr (assoc ibus-selected-display
						  (nth 2 group)))))
	  (setq ibus-callback-queue (cdr ibus-callback-queue))
	  (ibus-exec-callback-1 (cdr queue))))
      (let ((group (assq ibus-buffer-group ibus-buffer-group-alist)))
	(setq ibus-imcontext-id (cdr (assoc ibus-selected-display
					    (cadr group)))
	      ibus-imcontext-status (cdr (assoc ibus-selected-display
						(nth 2 group)))))
      (ibus-do-update-preedit))))

(defun ibus-process-signals (sexplist &optional passive)
  (let (rsexplist)
    (while sexplist
      (let ((sexp (pop sexplist)))
	(if (and (consp sexp)
		 (functionp (car sexp)))
	    (push sexp rsexplist)
	  (ibus-log "ignore: %S" sexp))))
    (when rsexplist
      (ibus-log "this-command: %s" this-command)
      (ibus-log "last-command: %s" last-command)
      (ibus-log "ibus-last-command-event: %s" ibus-last-command-event)
      (ibus-log "before-change-functions: %s" before-change-functions)
      (if passive
	  (let ((queue1 (list (cons (get-buffer-process (current-buffer))
				    (nreverse rsexplist)))))
	    (if ibus-callback-queue
		(nconc ibus-callback-queue queue1)
	      (setq ibus-callback-queue queue1))
	    (setq unread-command-events
		  (cons 'ibus-receive-event
			(delq 'ibus-receive-event
			      (delq 'ibus-dummy-event unread-command-events)))))
	(when (buffer-live-p ibus-current-buffer)
	  (with-current-buffer ibus-current-buffer
	    (ibus-exec-callback-1 (nreverse rsexplist))
	    (ibus-do-update-preedit)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Callbacks for preediting & commit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibus-*table--cell-insert (string)
  (with-no-warnings
    (table--finish-delayed-tasks)
    (table-recognize-cell 'force)
    (dont-compile ; To avoid byte-compile error
      (table-with-cache-buffer
       (insert-and-inherit string)
       (table--untabify (point-min) (point-max))
       (table--fill-region (point-min) (point-max))
       (setq table-inhibit-auto-fill-paragraph t)))
    (table--finish-delayed-tasks)))

(defun ibus-commit-text-cb (ic text)
  (cond
   ((not (= ic ibus-imcontext-id))
    (ibus-message "IMContext ID (%s) is mismatched." id))
   (isearch-mode
    (isearch-process-search-string text text))
   (buffer-read-only
    (ibus-message "Buffer is read-only: %S" (current-buffer)))
   ((not ibus-string-insertion-failed)
    (ibus-remove-preedit)
    (condition-case err
	(progn
	  (cond
	   ;; ansi-term
	   ((and (eq major-mode 'term-mode)
		 (get-buffer-process (current-buffer)))
	    (with-no-warnings
	      (term-send-raw-string text)))
	   ;; table-mode
	   ((and (featurep 'table)
		 (with-no-warnings table-mode-indicator))
	    (ibus-*table--cell-insert text))
	   ;; Normal commit
	   (ibus-undo-by-committed-string
	    (insert-and-inherit text))
	   ;; Normal commit (Undoing will be performed every 20 characters)
	   (t
	    (ibus-insert-and-modify-undo-list text)))
	  (setq ibus-last-command 'self-insert-command)
	  (run-hooks 'ibus-commit-string-hook))
      (text-read-only
       (ibus-message "Failed to commit string %S" err)
       (setq ibus-string-insertion-failed t)))
    (ibus-show-preedit t))))

(defun ibus-hide-preedit-text-cb (ic)
  (setq ibus-preedit-shown nil))

(defun ibus-show-preedit-text-cb (ic)
  (setq ibus-preedit-shown t))

(defun ibus-update-preedit-text-cb (ic text cursor-pos visible &rest attributes)
  (setq ibus-preedit-text text
	ibus-preedit-curpos cursor-pos
	ibus-preedit-shown visible
	ibus-preedit-attributes attributes
	ibus-preedit-update t))

(defun ibus-hide-auxiliary-text-cb (ic)
  (setq ibus-auxiliary-shown nil))

(defun ibus-show-auxiliary-text-cb (ic)
  (setq ibus-auxiliary-shown t))

(defun ibus-update-auxiliary-text-cb (ic text visible)
  (setq ibus-auxiliary-text text
	ibus-auxiliary-shown visible))

(defun ibus-hide-lookup-table-cb (ic)
  (message nil))

(defun ibus-show-lookup-table-cb (ic current-table &optional selection)
  (message (let ((i 0))
	     (mapconcat (lambda (candidate)
			  (format (if (eq i selection) "%s.[%s]" "%s. %s ")
				  (setq i (1+ i)) candidate))
			current-table
			" "))))

(defun ibus-*table--cell-delete-region (beg end)
  (push-mark beg t t)
  (goto-char end)
  (call-interactively '*table--cell-delete-region))

(defun ibus-delete-surrounding-text-cb (ic offset length)
  (cond
   ((not (= ic ibus-imcontext-id))
    (ibus-message "IMContext ID (%s) is mismatched." ic))
   (buffer-read-only
    (ibus-message "Buffer is read-only: %S" (current-buffer)))
   ((not ibus-string-insertion-failed)
    (ibus-log "delete surrounding text")
    (ibus-remove-preedit)
    (let* ((pos (point))
	   (beg (+ pos offset))
	   (end (+ beg length))
	   (retval t))
      (condition-case err
	  (cond
	   ((and (featurep 'table)
		 (with-no-warnings table-mode-indicator))
	    (ibus-*table--cell-delete-region beg end))
	   (t
	    (delete-region beg end)))
	(text-read-only
	 (ibus-message "Failed to delete surrounding text %S" err)
	 (setq retval nil
	       ibus-string-insertion-failed t)))
      (ibus-show-preedit t)))))

(defun ibus-forward-key-event-cb (ic keyval modmask &optional pressed)
  (let ((event (ibus-encode-event keyval modmask)))
    (if event
	(if pressed
	    (setq unread-command-events (cons event unread-command-events)))
      (if (not (= ic ibus-imcontext-id))
	  (ibus-message "IMContext ID (%s) is mismatched." ic)
	(ibus-agent-send-key-event keyval modmask pressed)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process key events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibus-agent-send-key-event (keyval modmask pressed)
  (when (ibus-agent-send "process_key_event(%d, %d, %d, %d, %s)"
			 ibus-imcontext-id keyval 0 modmask
			 (if pressed "True" "False"))
    (if pressed (sit-for 0.001 t))
    (ibus-agent-receive)))

(defun ibus-process-key-event-cb (ic handled)
  (if (or handled
	  (null ibus-last-command-event))
      ;; If key event is handled
      (when ibus-last-command-event
	(setq ibus-last-command-event nil))
    ;; If key event is ignored
    (let* ((vec (vector ibus-last-command-event))
	   (event (or (and (boundp 'local-function-key-map)
			   (lookup-key local-function-key-map vec))
		      (lookup-key function-key-map vec)))
	   keybind)
      (setq event (or (and (arrayp event)
			   (aref event 0))
		      ibus-last-command-event))
      (let ((ibus-mode-map-alist nil)) ; Disable keymap temporarily
	(if ibus-keymap-overlay
	    (overlay-put ibus-keymap-overlay 'keymap nil))
	(setq keybind (key-binding (vector event)))
	(if (and (null keybind)
		 (integerp event)
		 (memq 'shift (event-modifiers event)))
	    ;; Reset the 25th bit corresponding to the shift key
	    (setq event (logand event (lognot ?\x2000000))
		  keybind (key-binding (vector event))))
	(if ibus-keymap-overlay
	    (overlay-put ibus-keymap-overlay 'keymap ibus-mode-preedit-map)))
      (ibus-log "event: --> %s --> %s" ibus-last-command-event event)
      (if (or (eq ibus-last-command-event ibus-last-rejected-event)
	      (eq keybind this-command)
	      isearch-mode)
	  (progn
	    (ibus-message "%s is undefined"
			  (single-key-description ibus-last-command-event))
	    (if isearch-mode
		(isearch-done)))
	(if (memq keybind '(self-insert-command
			    *table--cell-self-insert-command))
	    ;; Self-insert command
	    (progn
	      (ibus-do-update-preedit)
	      (ibus-log "execute command: %s" keybind)
	      (setq ibus-last-rejected-event ibus-last-command-event
		    ibus-last-command-event nil
		    last-command-event event
		    last-command ibus-last-command
		    this-command keybind)
	      (unwind-protect
		  (if (and (eq keybind 'self-insert-command)
			   (eq ibus-last-command 'self-insert-command))
		      (ibus-insert-and-modify-undo-list (char-to-string event))
		    (command-execute keybind)
		    (if (eq keybind '*table--cell-self-insert-command)
			(with-no-warnings
			  (table--finish-delayed-tasks))))
		(setq ibus-last-rejected-event nil)))
	  ;; The other commands
	  (ibus-log "event rejected: %s" ibus-last-command-event)
	  (if ibus-keymap-overlay
	      (overlay-put ibus-keymap-overlay 'keymap nil))
	  (setq ibus-mode-map-alist nil
		this-command ibus-last-command
		unread-command-events
		(cons ibus-last-command-event unread-command-events))
	  (remove-hook 'post-command-hook 'ibus-check-current-buffer)
	  (add-hook 'pre-command-hook 'ibus-fallback-pre-function)))))
  (setq ibus-last-rejected-event ibus-last-command-event
	ibus-last-command-event nil))

(defun ibus-fallback-pre-function ()
  (remove-hook 'pre-command-hook 'ibus-fallback-pre-function)
  (add-hook 'post-command-hook 'ibus-check-current-buffer)
  (add-hook 'post-command-hook 'ibus-fallback-post-function))

(defun ibus-fallback-post-function ()
  (remove-hook 'post-command-hook 'ibus-fallback-post-function)
  (if ibus-keymap-overlay
      (overlay-put ibus-keymap-overlay 'keymap ibus-mode-preedit-map))
  (ibus-set-mode-map-alist))

(defun ibus-wait-following-key-event (prev-event keyval modmask)
  (let ((event (read-event nil nil ibus-simultaneous-pressing-time)))
    (ibus-agent-send-key-event keyval modmask t)
    (when (and event
	       (not ibus-string-insertion-failed))
      (if (or (eq event prev-event)
	      (not (eq (key-binding (vector event)) 'ibus-handle-event)))
	  (setq unread-command-events (cons event unread-command-events))
	(let ((ibus-simultaneous-pressing-time nil))
	  (undo-boundary)
	  (setq this-command 'ibus-handle-event)
	  (ibus-process-key-event event))))))

(defun ibus-process-key-event (event)
  (let* ((pair (ibus-decode-event event))
	 (keyval (car pair))
	 (modmask (cdr pair)))
    (when (numberp keyval)
      (unless ibus-frame-focus (ibus-check-frame-focus t))
      (ibus-check-current-buffer))
    (ibus-log "event: %s  keyval: %s  modmask: %s" event keyval modmask)
    (when (eq keyval ?\xa5) ; ¥
      (setq event (event-convert-list
		   (append (event-modifiers event) (list ?\\))))
      (ibus-log "event: --> %s" event)
      (unless (eq (key-binding (vector event)) 'ibus-handle-event)
	(setq keyval nil
	      unread-command-events (cons event unread-command-events))))
    (when keyval
      (setq ibus-last-command-event event
	    ibus-surrounding-text-modified nil)
      (if (and (numberp ibus-imcontext-id)
	       (numberp keyval))
	  ;; Send a key event to agent
	  (progn
	    (setq ibus-string-insertion-failed nil)
	    (if (and ibus-simultaneous-pressing-time
		     ibus-imcontext-status)
		;; Thumb shift typing method
		(ibus-wait-following-key-event event keyval modmask)
	      (ibus-agent-send-key-event keyval modmask t))
;	    (unless ibus-string-insertion-failed
;	      (ibus-agent-send-key-event keyval modmask nil))
	    )
	;; IMContext is not registered or key event is not recognized
	(ibus-process-key-event-cb ibus-imcontext-id nil))))
  ;; Repair post-command-hook
  (unless (memq 'ibus-fallback-pre-function
		(default-value 'pre-command-hook))
    (when (and (local-variable-p 'post-command-hook)
	       (not (memq t post-command-hook)))
      (if post-command-hook
	  (add-to-list 'post-command-hook t t)
	(kill-local-variable 'post-command-hook)))
    (unless (memq 'ibus-check-current-buffer
		  (default-value 'post-command-hook))
      (if (y-or-n-p "iBus: `post-command-hook' was reset for some reasons. Try to repair this? ")
	  (add-hook 'post-command-hook 'ibus-check-current-buffer)
	(ibus-mode-off)))))

(defun ibus-handle-event (&optional arg)
  (interactive "*p")
  (unless (eq last-command 'ibus-handle-event)
    (setq ibus-last-command last-command))
  (ibus-process-key-event last-command-event))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manage IMContexts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibus-create-imcontext ()
  (let ((group (assq ibus-buffer-group ibus-buffer-group-alist)))
    (if group
	(setcar (nthcdr 3 group)
		(cons (current-buffer)
		      (delq (current-buffer) (nth 3 group))))
      (setq group (list ibus-buffer-group
			nil nil
			(list (current-buffer)))
	    ibus-buffer-group-alist (cons group ibus-buffer-group-alist)))
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
	(ibus-mode-quit)
	(error "Couldn't create imcontext. Turned off ibus-mode."))
      (setcdr group
	      (list (cons (cons ibus-selected-display ibus-imcontext-id)
			  (cadr group))
		    (cons (cons ibus-selected-display ibus-imcontext-status)
			  (nth 2 group))
		    (nth 3 group)))
      (ibus-cleanup-preedit))))

(defun ibus-create-imcontext-cb (ic)
  (setq ibus-imcontext-id ic))

(defun ibus-destroy-imcontext ()
  (if (and (numberp ibus-imcontext-id)
	   ibus-frame-focus)
      (ibus-change-focus nil))
  (let ((group (assq ibus-buffer-group ibus-buffer-group-alist)))
    (when (and group
	       (null (setcar (nthcdr 3 group)
			     ;; Remove current buffer from imcontext group
			     (delq (current-buffer) (nth 3 group)))))
      (mapc (lambda (pair)
	      (let* ((ibus-selected-display (car pair))
		     (ibus-agent-process (cdr (assoc ibus-selected-display
						     ibus-agent-process-alist)))
		     (ibus-imcontext-id (cdr pair)))
		(if (numberp ibus-imcontext-id)
		    (ibus-agent-send "destroy_imcontext(%d)" ibus-imcontext-id))))
	    (cadr group))
      (setq ibus-imcontext-id nil
	    ibus-imcontext-status nil
	    ibus-buffer-group-alist (assq-delete-all ibus-buffer-group
						     ibus-buffer-group-alist))))
  (kill-local-variable 'ibus-buffer-group)
  (if (eq ibus-current-buffer (current-buffer))
      (setq ibus-current-buffer nil)))

(defun ibus-enable (&optional engine-name)
  (interactive)
  (when (and (interactive-p)
	     (null ibus-current-buffer))
    (ibus-check-current-buffer))
  (when (and (processp ibus-agent-process)
	     (numberp ibus-imcontext-id))
    (if engine-name
	(ibus-agent-send-receive "set_engine(%d, %S)" ibus-imcontext-id engine-name)
    (ibus-agent-send-receive "enable(%d)" ibus-imcontext-id))))

(defun ibus-disable ()
  (interactive)
  (when (and (interactive-p)
	     (null ibus-current-buffer))
    (ibus-check-current-buffer))
  (when (and (processp ibus-agent-process)
	     (numberp ibus-imcontext-id))
    (ibus-agent-send-receive "disable(%d)" ibus-imcontext-id))
  (if ibus-imcontext-status
      (ibus-disabled-cb ibus-imcontext-id)))

(defun ibus-status-changed-cb (ic status)
  (if (not (= ic ibus-imcontext-id))
      (ibus-message "IMContext ID (%s) is mismatched." ic)
    (unless (string= ibus-preedit-prev-text "")
      (ibus-commit-text-cb ic ibus-preedit-prev-text)
      (ibus-cleanup-preedit))
    (setq ibus-imcontext-status status)
    (setcdr (assoc ibus-selected-display
		   (nth 2 (assq ibus-buffer-group ibus-buffer-group-alist)))
	    status)
    (ibus-update-cursor-color)
    (ibus-set-keymap-parent)
    (when (and ibus-use-ja-onbiki-key
	       ibus-ja-onbiki-x-keysym)
      (ibus-update-ja-onbiki-key))))

(defun ibus-toggle ()
  (interactive)
  (when (and (interactive-p)
	     (null ibus-current-buffer))
    (ibus-check-current-buffer))
  (if ibus-imcontext-status
      (ibus-disable)
    (ibus-enable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manage buffer switching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibus-buffer-group-identifier ()
  (or (not ibus-mode-local)
      (current-buffer)))

(defun ibus-buffer-group-suitable-p ()
  (eq (eq ibus-buffer-group t)
      (not ibus-mode-local)))

(defun ibus-check-current-buffer ()
;  (ibus-log "check current buffer")
  (catch 'exit
    (ibus-cancel-focus-update-timer)
    (setq ibus-last-rejected-event nil)
    (with-current-buffer (window-buffer)
      (let ((buffer (current-buffer))
	    (visited-p ibus-buffer-group)
	    (non-x-p (not (eq window-system 'x)))
	    (display-unchanged-p (equal (ibus-get-x-display)
					ibus-selected-display)))
	;; Switch IMContext between global and local
	(unless (or non-x-p
		    (not visited-p)
		    (ibus-buffer-group-suitable-p))
	  (setq visited-p nil)
	  (if (eq buffer ibus-current-buffer)
	      (ibus-destroy-imcontext)
	    (let ((ibus-current-buffer buffer))
	      (ibus-destroy-imcontext))))
	;; Change focus if buffer is switched to another one
	(unless (and (eq buffer ibus-current-buffer)
		     (if non-x-p
			 (null ibus-imcontext-id)
		       (and ibus-imcontext-id
			    display-unchanged-p)))
	  ;; Focus out from previous buffer
	  (ibus-log "buffer was changed from %S to %S" ibus-current-buffer buffer)
	  (when (buffer-live-p ibus-current-buffer)
	    (with-current-buffer ibus-current-buffer
	      (when (numberp ibus-imcontext-id)
		(when ibus-frame-focus
		  (ibus-change-focus nil)) ; Send only
		(if ibus-preediting-p
		    ;; Cleenup preedit if focus change become timeout
		    (ibus-abort-preedit)))))
	;; Setup currently selected buffer
	  (unless display-unchanged-p
	    (condition-case err
		(ibus-change-x-display)
	      (error
	       (ibus-message "%s: %s" (car err) (if (cddr err) (cdr err) (cadr err)))
	       (if ibus-mode (ibus-mode-quit))
	       (throw 'exit nil))))
	  (setq ibus-current-buffer buffer)
	  (let* ((group-id (or ibus-buffer-group
			       ibus-parent-buffer-group
			       (ibus-buffer-group-identifier)))
		 (group (assq group-id ibus-buffer-group-alist)))
	    (setq ibus-imcontext-id (cdr (assoc ibus-selected-display
						(cadr group)))
		  ibus-imcontext-status (cdr (assoc ibus-selected-display
						    (nth 2 group))))
	    (unless ibus-buffer-group
	      (setq ibus-buffer-group group-id)
	      (when ibus-parent-buffer-group
		;; Inherit IMContext
		(ibus-log "inherit IMContext (buffer group: %s)" group-id)
		(setcar (nthcdr 3 group)
			(cons buffer (delq buffer (nth 3 group)))))
	      (add-hook 'kill-buffer-hook 'ibus-kill-buffer-function nil t)))
	  ;; Check whether buffer is already registered
	  (unless (or non-x-p
		      (and visited-p ibus-imcontext-id))
	    (ibus-log "new buffer was detected: %S" buffer)
	    (condition-case err
		(ibus-create-imcontext)
	      (error
	       (ibus-message "%s: %s" (car err) (if (cddr err) (cdr err) (cadr err)))
	       (if ibus-mode (ibus-mode-quit))
	       (throw 'exit nil))))
	  ;; `ibus-preedit-text' not empty means
	  ;; continuous preediting of incremental search
	  (when (string= ibus-preedit-text "")
	    ;; Focus in if window is active
	    (setq ibus-frame-focus nil) ; Send only
	    (if (numberp ibus-imcontext-id)
		(ibus-check-frame-focus t)))
	  (ibus-set-keymap-parent)
	  (ibus-update-cursor-color)))
      (setq ibus-parent-buffer-group nil)
      ;; Disable keymap if buffer is read-only, explicitly disabled, or vi-mode.
      (if (eq (and (or buffer-read-only
		       ibus-mode-map-disabled
		       (eq major-mode 'vi-mode)
		       (and (boundp 'vip-current-mode)
			    (eq vip-current-mode 'vi-mode))
		       (and (boundp 'viper-current-state)
			    (eq viper-current-state 'vi-state)))
		   (not (and isearch-mode
			     ibus-use-ja-onbiki-key
			     ibus-ja-onbiki-x-keysym)))
	      (not ibus-mode-map-prev-disabled))
	  (ibus-switch-keymap ibus-mode-map-prev-disabled))
      ;; Set/restore cursor shape
      (when (local-variable-p 'ibus-cursor-type-saved)
	(cond
	 (ibus-preediting-p
	  (setq cursor-type ibus-cursor-type-for-candidate))
	 (isearch-mode
	  (setq cursor-type ibus-isearch-cursor-type))
	 (t
	  (if (eq ibus-cursor-type-saved 1)
	      (kill-local-variable 'cursor-type)
	    (setq cursor-type ibus-cursor-type-saved))
	  (kill-local-variable 'ibus-cursor-type-saved))))
      ;; Check selected frame
      (unless (eq (selected-frame) ibus-selected-frame)
	(if (eq window-system 'x)
	    (ibus-frame-top-left-coordinates))
	(setq ibus-selected-frame (selected-frame))
	(ibus-update-cursor-color)))
    (ibus-start-focus-observation)))

(defun ibus-kill-buffer-function ()
  (ibus-destroy-imcontext))

(defun ibus-exit-minibuffer-function ()
  (if ibus-imcontext-temporary-for-minibuffer
      (ibus-destroy-imcontext)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INHERIT-INPUT-METHOD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibus-defadvice-inherit-imcontext ()
  (mapc (lambda (command)
	  (eval
	   `(defadvice ,command
	      (around ,(intern (concat "ibus-inherit-im-" (symbol-name command))) ())
	      (if (and (with-no-warnings
			 (or (null (assq 'inherit-input-method ad-arg-bindings))
			     inherit-input-method
			     ibus-force-inherit-im))
		       (numberp ibus-imcontext-id))
		  (let ((ibus-force-inherit-im t))
		    (setq ibus-parent-buffer-group ibus-buffer-group)
		    ad-do-it)
		ad-do-it))))
	ibus-inherit-im-functions))

(defun ibus-activate-advices-inherit-im (enable)
  (if enable
      (ad-enable-regexp "^ibus-inherit-im-")
    (ad-disable-regexp "^ibus-inherit-im-"))
  (ad-activate-regexp "^ibus-inherit-im-"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup incremental search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Internal functions

(defun ibus-isearch-start ()
  (if ibus-preediting-p
      (ibus-abort-preedit))
  (unless (or (eq ibus-isearch-cursor-type 0)
	      (local-variable-p 'ibus-cursor-type-saved))
    (setq ibus-cursor-type-saved
	  (or (and (local-variable-p 'cursor-type) cursor-type)
	      1)))) ; 1 means that global value has been used

(defun ibus-isearch-check-preedit ()
  (unless ibus-preediting-p
    (with-current-buffer ibus-isearch-minibuffer
      (exit-minibuffer))))

(defun ibus-isearch-read-string-post-function ()
  (ibus-log "isearch: exit iBus input")
  (remove-hook 'post-command-hook 'ibus-isearch-check-preedit)
  (remove-hook 'minibuffer-exit-hook 'ibus-isearch-read-string-post-function t)
  (ibus-isearch-start)
  (when (numberp ibus-imcontext-id)
    (let ((ibus-frame-focus nil)) ; To avoid focus out
      (ibus-destroy-imcontext))))

(defun ibus-isearch-read-string-pre-function ()
  (ibus-log "isearch: start iBus input")
  (remove-hook 'post-command-hook 'ibus-isearch-read-string-pre-function)
  (add-hook 'post-command-hook 'ibus-isearch-check-preedit t)
  (add-hook 'minibuffer-exit-hook 'ibus-isearch-read-string-post-function nil t)
  (ibus-show-preedit)
  (setq ibus-isearch-minibuffer (current-buffer)))

(defun ibus-isearch-process-search-characters (last-char)
  (let ((overriding-terminal-local-map nil)
	(prompt (isearch-message-prefix))
	(minibuffer-local-map (with-no-warnings isearch-minibuffer-local-map))
	(current-input-method nil)
	(ibus-imcontext-temporary-for-minibuffer nil)
	(ibus-isearch-minibuffer nil)
	(ibus-current-buffer nil)
	str junk-hist)
    (add-hook 'post-command-hook 'ibus-isearch-read-string-pre-function t)
    (if (eq (car unread-command-events) 'ibus-resume-preedit)
	(setq unread-command-events (cdr unread-command-events))
      (setq unread-command-events (cons last-char unread-command-events)))
    (setq str (read-string prompt isearch-string 'junk-hist nil t)
	  isearch-string ""
	  isearch-message "")
    (ibus-log "isearch-string: %S" str)
    (if (and str (> (length str) 0))
	(let ((unread-command-events nil))
	  (isearch-process-search-string str str))
      (isearch-update))
    (if (eq (car unread-command-events) 'ibus-resume-preedit)
	(if (string= ibus-preedit-text "")
	    (setq unread-command-events (cdr unread-command-events))
	  (setq unread-command-events (cons ?a unread-command-events))))))

(defun ibus-isearch-other-control-char ()
  (if (and ibus-use-ja-onbiki-key
	   (eq (event-basic-type last-command-event) ibus-ja-onbiki-key-symbol))
      (setq unread-command-events
	    (if ibus-imcontext-status
		(append (list ?a 'ibus-resume-preedit last-command-event)
			unread-command-events)
	      (cons (event-convert-list
		     (append (event-modifiers last-command-event) (list ?\\)))
		    unread-command-events)))
    (funcall (lookup-key ibus-mode-map (this-command-keys)))
    (if isearch-mode
	(isearch-update))))

;; Advices for `isearch.el'

(defadvice isearch-printing-char
  (around ibus-isearch-printing-char ())
  (if ibus-imcontext-status
      (let ((current-input-method "iBus"))
	ad-do-it)
    ad-do-it))

(defadvice isearch-other-control-char
  (around ibus-isearch-other-control-char ())
  (if (and ibus-mode
	   ;; `lookup-key' returns nil if KEY is undefined.
	   ;; Otherwise, returns a number if KEY is too long
	   ;; (this maybe means the case that KEY is mouse event).
	   (commandp (lookup-key ibus-mode-map (this-command-keys))))
      (ibus-isearch-other-control-char)
    ad-do-it))

(defadvice isearch-message-prefix
  (around ibus-isearch-message-prefix ())
  (if (and ibus-imcontext-status
	   (not nonincremental)
	   (not (eq this-command 'isearch-edit-string)))
      (let ((current-input-method "iBus")
	    (current-input-method-title "iBus"))
	ad-do-it)
    ad-do-it))

;; Advices for `isearch-x.el'

(defadvice isearch-toggle-specified-input-method
  (around ibus-isearch-toggle-specified-input-method ())
  (if ibus-imcontext-status
      (isearch-update)
    ad-do-it))

(defadvice isearch-toggle-input-method
  (around ibus-isearch-toggle-input-method ())
  (if ibus-imcontext-status
      (isearch-update)
    ad-do-it))

(defadvice isearch-process-search-multibyte-characters
  (around ibus-isearch-process-search-characters ())
  (if (and (string= current-input-method "iBus")
	   (eq this-command 'isearch-printing-char))
      (ibus-isearch-process-search-characters last-char)
    ad-do-it))

;; Commands and functions
(defun ibus-enable-isearch ()
  "Make iBus usable with isearch-mode."
  (interactive)
  (add-hook 'isearch-mode-hook 'ibus-isearch-start)
  (ad-enable-regexp "^ibus-isearch-")
  (ad-activate-regexp "^ibus-isearch-"))

(defun ibus-disable-isearch ()
  "Make iBus not usable with isearch-mode."
  (interactive)
  (remove-hook 'isearch-mode-hook 'ibus-isearch-start)
  (ad-disable-regexp "^ibus-isearch-")
  (ad-activate-regexp "^ibus-isearch-"))

(defun ibus-setup-isearch ()
  (if ibus-use-in-isearch-window
      (ibus-enable-isearch)
    (ibus-disable-isearch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup minor mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ibus-cleanup-variables ()
  (mapc (lambda (buffer)
	  (with-current-buffer buffer
	    (kill-local-variable 'ibus-buffer-group)
	    (kill-local-variable 'ibus-mode-map-prev-disabled)))
	(buffer-list))
  (setq-default ibus-buffer-group nil)
  (setq-default ibus-mode-map-prev-disabled nil)
  (setq ibus-current-buffer nil
	ibus-buffer-group-alist nil
	ibus-imcontext-id nil
	ibus-imcontext-status nil
	ibus-callback-queue nil
	ibus-preediting-p nil
	ibus-last-rejected-event nil
	ibus-last-command nil
	ibus-preedit-default-attr nil))

(defun ibus-mode-on ()
  (interactive)
  (if (not (or (eq window-system 'x) ; X frame
	       (getenv "DISPLAY")))  ; non-X frame under X session
      (ibus-mode-quit)
    (let (process-live-p)
      (mapc (lambda (pair)
	      (if (processp (cdr pair))
		  (setq process-live-p t)))
	    ibus-agent-process-alist)
      (if process-live-p (ibus-mode-off))) ; Restart ibus-mode
    (unwind-protect
	(ibus-agent-start)
      (if (not (and ibus-agent-process
		    (memq (process-status ibus-agent-process)
			  '(open run))))
	  ;; Connection failed
	  (ibus-mode-quit)
	;; Connection succeeded
	(setq ibus-selected-display (ibus-get-x-display)
	      ibus-agent-process-alist (list (cons ibus-selected-display
						   ibus-agent-process)))
	;; Turn on minor mode
	(setq-default ibus-mode t)
	(ibus-cleanup-variables)
	(setq ibus-frame-focus nil
	      ibus-selected-frame (selected-frame))
	(ibus-defadvice-disable-for-preedit)
	(ibus-activate-advices-disable-for-preedit t)
	(ibus-defadvice-inherit-imcontext)
	(ibus-activate-advices-inherit-im t)
	(ibus-activate-advice-describe-key t)
	(ibus-setup-isearch)
	;; Initialize key bindings
	(mapc (lambda (buffer)
		(with-current-buffer buffer
		  (if (memq major-mode ibus-incompatible-major-modes)
		      (setq ibus-mode-map-disabled t))))
	      (buffer-list))
	(ibus-update-key-bindings)
	(ibus-set-mode-map-alist)
	(add-to-ordered-list
	 'emulation-mode-map-alists 'ibus-mode-map-alist 50)
	;; Setup hooks
	(add-hook 'minibuffer-exit-hook 'ibus-exit-minibuffer-function)
	(add-hook 'after-change-major-mode-hook 'ibus-check-major-mode)
	(add-hook 'ediff-startup-hook 'ibus-check-current-buffer)
	(add-hook 'post-command-hook 'ibus-check-current-buffer)
	(ibus-log "post-command-hook: %s" post-command-hook)
	(add-hook 'after-make-frame-functions 'ibus-after-make-frame-function)
	(add-hook 'kill-emacs-hook 'ibus-mode-off)
	(ibus-log "ibus-mode ON")
	)))
  ibus-mode)

(defun ibus-mode-quit ()
  (remove-hook 'kill-emacs-hook 'ibus-mode-off)
  (mapc (lambda (buffer)
	  (with-current-buffer buffer
	    (remove-hook 'kill-buffer-hook 'ibus-kill-buffer-function t)))
	(buffer-list))
  (remove-hook 'after-make-frame-functions 'ibus-after-make-frame-function)
  (remove-hook 'post-command-hook 'ibus-check-current-buffer)
  (remove-hook 'ediff-startup-hook 'ibus-check-current-buffer)
  (remove-hook 'after-change-major-mode-hook 'ibus-check-major-mode)
  (remove-hook 'minibuffer-exit-hook 'ibus-exit-minibuffer-function)
  (setq emulation-mode-map-alists
	(delq 'ibus-mode-map-alist emulation-mode-map-alists))
  (ibus-update-ja-onbiki-key t)
  (ibus-activate-advices-disable-for-preedit nil)
  (ibus-activate-advices-inherit-im nil)
  (ibus-activate-advice-describe-key nil)
  (ibus-disable-isearch)
  (ibus-cancel-focus-update-timer)
  (ibus-cleanup-preedit)
  (mapc (lambda (pair)
	  (setq ibus-agent-process (cdr pair))
	  (ibus-agent-kill))
	ibus-agent-process-alist)
  (setq ibus-agent-process-alist nil)
  (setq-default ibus-mode nil)
  (ibus-cleanup-variables)
  (ibus-set-cursor-color)
  (ibus-log "ibus-mode OFF")
  ibus-mode)

(defun ibus-mode-off ()
  (interactive)
  (when (and (numberp ibus-imcontext-id)
	     ibus-frame-focus)
    (condition-case err
	(ibus-change-focus nil)
      (error (ibus-message "%S" err))))
  (setq ibus-frame-focus nil)
  ;; Destroy IMContext
  (mapc (lambda (group)
	  (let ((ibus-imcontext-id (cdar (cadr group))))
	    (condition-case err
		(ibus-destroy-imcontext)
	      (error (ibus-message "%S" err)))))
	ibus-buffer-group-alist)
  ;; Turn off minor mode
  (ibus-mode-quit))

(defun ibus-update-mode ()
  (if ibus-mode
      (ibus-mode-on)
    (ibus-mode-off)))

(defun ibus-mode (&optional arg)
  "Toggle iBus minor mode (ibus-mode).
With optional argument ARG, turn ibus-mode on if ARG is
positive, otherwise turn it off."
  (interactive "P")
  (if (not (or (eq window-system 'x)
	       (getenv "DISPLAY")
	       ibus-mode))
      (prog1 nil
	(ibus-message "ibus-mode needs Emacs to run under X session."))
    (setq-default ibus-mode
		  (if (null arg)
		      (not ibus-mode)
		    (> (prefix-numeric-value arg) 0)))
    (ibus-update-mode)))

;; minor-mode-alist
(unless (assq 'ibus-mode minor-mode-alist)
  (setq minor-mode-alist
	(cons '(ibus-mode (:eval (ibus-mode-line-string))) minor-mode-alist)))

;; minor-mode-map-alist
;;  ibus-mode doesn't use `minor-mode-map-alist' but
;;  `emulation-mode-map-alists', and it is not set yet because
;;  `ibus-mode-map' will be generated dynamically.

;; mode-line-mode-menu
(define-key mode-line-mode-menu [ibus-mode]
  `(menu-item ,(purecopy "Intelligent Input Bus (iBus)") ibus-mode
	      :help "Support the input of various languages"
	      :button (:toggle . (bound-and-true-p ibus-mode))))


(provide 'ibus)

;;;
;;; ibus.el ends here
