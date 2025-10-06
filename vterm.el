;;; vterm.el --- Fully-featured terminal emulator -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020 by Lukas Fürmetz & Contributors
;;
;; Author: Lukas Fürmetz <fuermetz@mailbox.org>
;; Version: 0.0.4
;; URL: https://github.com/akermu/emacs-libvterm
;; Keywords: terminals
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

(require 'term/xterm)
(require 'subr-x)
(require 'find-func)
(require 'cl-lib)
(require 'term)
(require 'color)
(require 'compile)
(require 'face-remap)
(require 'tramp)
(condition-case err
    (require 'vterm-module)
  (error (when (or (not module-file-suffix)
                   (not (member module-file-suffix load-suffixes)))
           (user-error "Emacs requires compilation switch --with-modules"))
         (signal (car err) (cdr err))))

;; a maintenance gotcha with vterm-module.c term_process_key
(defconst vterm--keys '(return tab backtab iso-lefttab backspace escape
                        up down left right
                        insert delete home end prior next
                        f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12
                        kp-0 kp-1 kp-2 kp-3 kp-4 kp-5 kp-6 kp-7 kp-8 kp-9
                        kp-add kp-subtract kp-multiply kp-divide kp-equal
                        kp-decimal kp-separator kp-enter))

(dolist (key vterm--keys)
  (let ((fun (concat "vterm-send-" (symbol-name key))))
    (defalias (intern fun)
      (lambda ()
        (interactive)
        (vterm-send-key (key-description (vector key)))))
    (make-obsolete (intern fun) nil "0.0.4")))

(declare-function vterm--reset-cursor-point "vterm-module")
(declare-function vterm--get-pwd-raw "vterm-module")
(declare-function vterm--set-size "vterm-module")
(declare-function vterm--write-input "vterm-module")
(declare-function vterm--update "vterm-module")
(declare-function vterm--set-pty-name "vterm-module")
(declare-function vterm--new "vterm-module")

(defcustom vterm-shell shell-file-name
  "The shell that gets run in the vterm."
  :type 'string
  :group 'vterm)

(defcustom vterm-tramp-shells
  '(("ssh" login-shell) ("scp" login-shell) ("docker" "/bin/sh"))
  "List of tuples of the form (TRAMP-METHOD SHELL [FALLBACK-SHELL]).

A tuple \\='(\"ssh\" login-shell \"/bin/bash\") defers to the login
shell of the remote host, determined by the getent command, which
is not present on macos.  The default fallback shell is tramp's shell."
  :type '(alist :key-type string :value-type string)
  :group 'vterm)

(defcustom vterm-buffer-name "*vterm*"
  "Generally suffixed by an instantiation ordinal \"*vterm*<2>\"."
  :type 'string
  :group 'vterm)

(defcustom vterm-max-scrollback 1000
  "The maximum allowed is 100000, set by SB_MAX in vterm-module.h."
  :type 'number
  :group 'vterm)

(defcustom vterm-min-window-width 80
  "Minimum window width."
  :type 'number
  :group 'vterm)

(defcustom vterm-kill-buffer-on-exit t
  "Whether the buffer is killed when its vterm process ends."
  :type  'boolean
  :group 'vterm)

(define-obsolete-variable-alias 'vterm-clear-scrollback
  'vterm-clear-scrollback-when-clearing "0.0.1")

(define-obsolete-variable-alias 'vterm-use-vterm-prompt
  'vterm-use-vterm-prompt-detection-method "0.0.1")

(defcustom vterm-clear-scrollback-when-clearing nil
  "If not nil `vterm-clear' clears both screen and scrollback.

The scrollback is everything that is not current visible on
screen in vterm buffers.

If `vterm-clear-scrollback-when-clearing' is nil, `vterm-clear'
clears only the screen, so the scrollback is accessible moving
the point up."
  :type 'boolean
  :group 'vterm)

(defun vterm--prefix-keys ()
  "Return prefix keys that libvterm should not intercept."
  (let (prefix-keys)
    (map-keymap
     (lambda (key binding)
       (when (keymapp binding)
         (let ((key-desc (key-description (vector key))))
           ;; Only include keys that represent actual keyboard input
           ;; Exclude special internal keys like <remap>, <vertical-line>, etc.
           (when (or (not (string-prefix-p "<" key-desc))
                     (not (string-suffix-p ">" key-desc)))
             (push key-desc prefix-keys)))))
     global-map)
    ;; meta-prefix-char (ESC) is special
    (delete (key-description (vector meta-prefix-char)) prefix-keys)))

(defcustom vterm-exit-functions nil
  "List of functions called when a vterm process exits.

Each function is called with two arguments: the vterm buffer of
the process if any, and a string describing the event passed from
the sentinel.

This hook applies only to new vterms, created after setting this
value with `add-hook'.

Note that this hook will not work if another package like
`shell-pop' sets its own sentinel to the `vterm' process."
  :type 'hook
  :group 'vterm)

(make-obsolete-variable 'vterm-set-title-functions
                        "This variable was substituted by `vterm-buffer-name-string'."
                        "0.0.1")

(defcustom vterm-buffer-name-string nil
  "Format string for the title of vterm buffers.

If `vterm-buffer-name-string' is nil, vterm will not set the
title of its buffers.  If not nil, `vterm-buffer-name-string' has
to be a format control string (see `format') containing one
instance of %s which will be substituted with the string TITLE.
The argument TITLE is provided by the shell.  This requires shell
side configuration.

For example, if `vterm-buffer-name-string' is set to \"vterm %s\",
and the shell properly configured to set TITLE=$(pwd), than vterm
buffers will be named \"vterm\" followed by the current path.

See URL http://tldp.org/HOWTO/Xterm-Title-4.html for additional
information on the how to configure the shell."
  :type 'string
  :group 'vterm)

(defcustom vterm-term-environment-variable "xterm-256color"
  "TERM value for terminal."
  :type 'string
  :group 'vterm)

(defcustom vterm-environment nil
  "List of extra environment variables to the vterm shell processes only.

demo: \\='(\"env1=v1\" \"env2=v2\")"
  :type '(repeat string)
  :group 'vterm)


(defcustom vterm-enable-manipulate-selection-data-by-osc52 nil
  "Support OSC 52 MANIPULATE SELECTION DATA(libvterm 0.2 is needed).

Support copy text to Emacs kill ring and system clipboard by using OSC 52.
For example: send base64 encoded \\='foo\\=' to kill ring: echo -en \\='\\e]52;c;Zm9v\\a\\=',
tmux can share its copy buffer to terminals by supporting osc52(like iterm2
 xterm) you can enable this feature for tmux by :
set -g set-clipboard on         #osc 52 copy paste share with iterm
set -ga terminal-overrides \\=',xterm*:XT:Ms=\\E]52;%p1%s;%p2%s\\007\\='
set -ga terminal-overrides \\=',screen*:XT:Ms=\\E]52;%p1%s;%p2%s\\007\\='

The clipboard querying/clearing functionality offered by OSC 52 is not
implemented here,And for security reason, this feature is disabled
by default."
  :type 'boolean
  :group 'vterm)

(defcustom vterm-eval-cmds '(("find-file" find-file)
                             ("message" message)
                             ("vterm-clear-scrollback" vterm-clear-scrollback))
  "Backdoor facility to execute arbitrary elisp on term refresh.

See Message passing in README.

`vterm-eval-cmds' has to be a list of pairs of the format:
\(NAME-OF-COMMAND-IN-SHELL EMACS-FUNCTION)"
  :type '(alist :key-type string)
  :group 'vterm)

(defcustom vterm-disable-underline nil
  "Render underlined text as not underlined."
  :type  'boolean
  :group 'vterm)

(defcustom vterm-disable-inverse-video nil
  "Render inverse video text as not inverse video."
  :type  'boolean
  :group 'vterm)

(define-obsolete-variable-alias 'vterm-disable-bold-font
  'vterm-disable-bold "0.0.1")

(defcustom vterm-disable-bold-font nil
  "Render bold text as not bold."
  :type  'boolean
  :group 'vterm)

(defcustom vterm-set-bold-hightbright nil
  "Wtf is hightbright (see #549)."
  :type  'boolean
  :group 'vterm)

(defcustom vterm-ignore-blink-cursor t
  "Insulate against `blink-cursor-mode'.
That globalized minor mode interferes with our control over blink."
  :type 'boolean
  :group 'vterm)

(defcustom vterm-copy-exclude-prompt t
  "Exclude prompt from copied line (useful for copying commands)."
  :type 'boolean
  :group 'vterm)

(make-obsolete 'vterm-use-vterm-prompt-detection-method nil "0.0.4")
(make-obsolete 'vterm-bookmark-check-dir nil "0.0.4")
(defvar-local vterm--exit-copy-mode-function nil)

(defface vterm-color-black
  `((t :inherit term-color-black))
  "Face used to render black color code."
  :group 'vterm)

(defface vterm-color-red
  `((t :inherit term-color-red))
  "Face used to render red color code."
  :group 'vterm)

(defface vterm-color-green
  `((t :inherit term-color-green))
  "Face used to render green color code."
  :group 'vterm)

(defface vterm-color-yellow
  `((t :inherit term-color-yellow))
  "Face used to render yellow color code."
  :group 'vterm)

(defface vterm-color-blue
  `((t :inherit term-color-blue))
  "Face used to render blue color code."
  :group 'vterm)

(defface vterm-color-magenta
  `((t :inherit term-color-magenta))
  "Face used to render magenta color code."
  :group 'vterm)

(defface vterm-color-cyan
  `((t :inherit term-color-cyan))
  "Face used to render cyan color code."
  :group 'vterm)

(defface vterm-color-white
  `((t :inherit term-color-white))
  "Face used to render white color code."
  :group 'vterm)

(defface vterm-color-bright-black
  `((t :inherit ,(if (facep 'term-color-bright-black)
                     'term-color-bright-black
                   'term-color-black)))
  "Face used to render bright black color code."
  :group 'vterm)

(defface vterm-color-bright-red
  `((t :inherit ,(if (facep 'term-color-bright-red)
                     'term-color-bright-red
                   'term-color-red)))
  "Face used to render bright red color code."
  :group 'vterm)

(defface vterm-color-bright-green
  `((t :inherit ,(if (facep 'term-color-bright-green)
                     'term-color-bright-green
                   'term-color-green)))
  "Face used to render bright green color code."
  :group 'vterm)

(defface vterm-color-bright-yellow
  `((t :inherit ,(if (facep 'term-color-bright-yellow)
                     'term-color-bright-yellow
                   'term-color-yellow)))
  "Face used to render bright yellow color code."
  :group 'vterm)

(defface vterm-color-bright-blue
  `((t :inherit ,(if (facep 'term-color-bright-blue)
                     'term-color-bright-blue
                   'term-color-blue)))
  "Face used to render bright blue color code."
  :group 'vterm)

(defface vterm-color-bright-magenta
  `((t :inherit ,(if (facep 'term-color-bright-magenta)
                     'term-color-bright-magenta
                   'term-color-magenta)))
  "Face used to render bright magenta color code."
  :group 'vterm)

(defface vterm-color-bright-cyan
  `((t :inherit ,(if (facep 'term-color-bright-cyan)
                     'term-color-bright-cyan
                   'term-color-cyan)))
  "Face used to render bright cyan color code."
  :group 'vterm)

(defface vterm-color-bright-white
  `((t :inherit ,(if (facep 'term-color-bright-white)
                     'term-color-bright-white
                   'term-color-white)))
  "Face used to render bright white color code."
  :group 'vterm)

(defface vterm-color-underline
  `((t :inherit default))
  "Face used to render cells with underline attribute.
Only foreground is used."
  :group 'vterm)

(defface vterm-color-inverse-video
  `((t :inherit default))
  "Face used to render cells with inverse video attribute.
Only background is used."
  :group 'vterm)

(defvar vterm-color-palette
  [vterm-color-black
   vterm-color-red
   vterm-color-green
   vterm-color-yellow
   vterm-color-blue
   vterm-color-magenta
   vterm-color-cyan
   vterm-color-white
   vterm-color-bright-black
   vterm-color-bright-red
   vterm-color-bright-green
   vterm-color-bright-yellow
   vterm-color-bright-blue
   vterm-color-bright-magenta
   vterm-color-bright-cyan
   vterm-color-bright-white]
  "Color palette for foreground and background.")

(defvar-local vterm--term nil "Buffer-local VTerm.")
(defvar-local vterm--process nil "Buffer-local process.")
(defvar-local vterm--partial nil)

(defun vterm--alias-set-mark ()
  (interactive)
  (let ((last-command-event (aref (kbd "C-@") 0)))
    (call-interactively #'vterm--self-insert)))

(defun vterm--alias-undo ()
  (interactive)
  (let ((last-command-event (aref (kbd "C-_") 0)))
    (call-interactively #'vterm--self-insert)))

(defvar vterm-mode-map
  (let* ((map (make-keymap))
         (remaps '((scroll-down-command . vterm--copy-mode-then)
                   (scroll-up-command . vterm--copy-mode-then)
                   ;; (mouse-yank-primary . vterm-yank-primary)
                   ;; (xterm-paste . vterm-xterm-paste)
                   (recenter-top-bottom . vterm-clear)
                   (previous-line . vterm--copy-mode-then)
                   (next-line . vterm--copy-mode-then)))
         (remap-keys
          (mapcar (lambda (pair)
                    (kbd (key-description (where-is-internal
                                           (car pair) nil :first))))
                  remaps)))
    ;; Fallback.  Anything that would invoke self-insert-command gets
    ;; remapped to vterm--self-insert.
    (define-key map [remap self-insert-command] #'vterm--self-insert)

    ;; Other bespoke remaps besides vterm--self-insert
    (dolist (remap remaps)
      (define-key map (vector 'remap (car remap)) (cdr remap)))

    ;; Self-insert 7-bit ASCII
    ;; 0-31: Control characters (C-@, C-a through C-z, C-[, etc.)
    ;; 32-126: Printable characters (space through ~)
    ;; 127: DEL
    (dotimes (i 128)
      (let ((key (char-to-string i)))
        (unless (member key remap-keys)
          (define-key map key 'vterm--self-insert))))

    ;; As emacs users, we want C-SPC to get mapped to C-@ (set-mark)
    (define-key map (kbd "C-SPC") 'vterm--alias-set-mark)

    ;; As emacs users, we want C-/ to get mapped to C-_ (undo)
    (define-key map (kbd "C-/") 'vterm--alias-undo)

    ;; Self-insert M-a through M-z
    (define-key map (vector meta-prefix-char) (make-keymap)) ;M- now a prefix
    (dotimes (i (- ?z ?a))
      (let ((key (vector meta-prefix-char (+ ?a i))))
        (unless (member (kbd (key-description key)) remap-keys)
          (define-key map key 'vterm--self-insert))))

    ;; Use vterm-yank-pop after C-c C-y, else pass through to shell
    (define-key map (kbd "M-y") 'vterm-yank-pop-dwim)

    ;; Let our fave emacs-specific prefixes pass through unharried
    (mapc (lambda (key) (define-key map (kbd key) nil)) (vterm--prefix-keys))

    ;; You can take C-g from my cold, dead hands.
    (define-key map (kbd "C-g") nil)

    ;; M-x
    (define-key map (where-is-internal #'execute-extended-command nil :first) nil)

    ;; Keys in vterm_keycodes.h (vterm-module.c term_process_key)
    (dolist (key vterm--keys)
      (define-key map (vector key) #'vterm--self-insert))

    ;; Modifier directionals
    (dolist (dir '("left" "right" "up" "down"))
      (dolist (mod '("C-" "M-"))
        (define-key map (vector (intern (concat mod dir))) #'vterm--self-insert)))

    ;; Take tighter control with Shift-PgUp and Shift-PgDn
    (define-key map [S-prior] #'scroll-down-command)
    (define-key map [S-next] #'scroll-up-command)

    ;; Mouse shit
    (define-key map [mouse-1] #'vterm-mouse-set-point)

    ;; Modifier return
    (dolist (ret '(M-return S-return C-return))
      (define-key map (vector ret) #'vterm--self-insert))

    ;; vterm.el bespoke bindings
    (define-key map (kbd "C-c C-c") #'vterm--self-insert)
    (define-key map (kbd "C-c C-/") #'vterm--self-insert)
    (define-key map (kbd "C-c C-z") #'vterm--self-insert)
    (define-key map (kbd "C-c C-d") #'vterm--self-insert)
    (define-key map (kbd "C-c C-r") #'vterm-reset-cursor-point)
    (define-key map (kbd "C-c C-n") #'vterm-next-prompt)
    (define-key map (kbd "C-c C-p") #'vterm-previous-prompt)
    (define-key map (kbd "C-c C-t") #'vterm-copy-mode)
    (define-key map (kbd "C-c C-y") #'vterm-yank)
    (define-key map (kbd "C-c M-y") #'vterm-yank-pop)
    map))

(defvar vterm-copy-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-t") #'vterm-copy-mode)
    (define-key map (kbd "C-c C-y") #'vterm--copy-mode-done-then)
    (define-key map [remap self-insert-command] 'vterm--copy-mode-done-then)
    (define-key map [return] #'vterm-copy-mode-done)
    (define-key map (kbd "RET") #'vterm-copy-mode-done)
    (define-key map (kbd "C-c C-r") #'vterm-reset-cursor-point)
    (define-key map (kbd "C-c C-n") #'vterm-next-prompt)
    (define-key map (kbd "C-c C-p") #'vterm-previous-prompt)
    map))

(define-derived-mode vterm-mode nil "VTerm"
  "Major mode for vterm buffer."
  (buffer-disable-undo)
  (hack-dir-local-variables)
  (when-let ((vterm-env (assq 'vterm-environment dir-local-variables-alist)))
    (setq-local vterm-environment (cdr vterm-env)))
  (let ((process-environment
         (append vterm-environment
                 `(,(concat "TERM="
                     vterm-term-environment-variable)
                   ,(concat "EMACS_VTERM_PATH="
                     (file-name-directory (find-library-name "vterm")))
                   "INSIDE_EMACS=vterm"
                   "LINES"
                   "COLUMNS")
                 process-environment))
        ;; TODO: Figure out why inhibit is needed for curses to render correctly.
        (inhibit-eol-conversion nil)
        (coding-system-for-read 'binary)
        (process-adaptive-read-buffering nil)
        (width (max (- (window-max-chars-per-line) (vterm--get-margin-width))
                    vterm-min-window-width)))
    (setq vterm--term (vterm--new (window-body-height)
                                  width vterm-max-scrollback
                                  vterm-disable-bold-font
                                  vterm-disable-underline
                                  vterm-disable-inverse-video
                                  vterm-ignore-blink-cursor
                                  vterm-set-bold-hightbright))
    (setq buffer-read-only t)
    (setq-local scroll-conservatively 101)
    (setq-local scroll-margin 0)
    (setq-local hscroll-margin 0)
    (setq-local hscroll-step 1)
    (setq-local truncate-lines t)

    ;; font-lock-mode remains on but defanged
    (setq-local font-lock-defaults '(nil t))

    (setq vterm--process
          (make-process
           :name "vterm"
           :buffer (current-buffer)
           :command
           `("/bin/sh" "-c"
             ,(format
               "stty -nl sane %s erase ^? rows %d columns %d >/dev/null && exec %s"
               ;; Some stty implementations (i.e. that of *BSD) do not
               ;; support the iutf8 option.  to handle that, we run some
               ;; heuristics to work out if the system supports that
               ;; option and set the arg string accordingly. This is a
               ;; gross hack but FreeBSD doesn't seem to want to fix it.
               ;;
               ;; See: https://bugs.freebsd.org/bugzilla/show_bug.cgi?id=220009
               (if (eq system-type 'berkeley-unix) "" "iutf8")
               (window-body-height)
               width (vterm--get-shell)))
           ;; :coding 'no-conversion
           :connection-type 'pty
           :file-handler t
           :filter #'vterm--filter
           ;; The sentinel is needed if there are exit functions or if
           ;; vterm-kill-buffer-on-exit is set to t.  In this latter case,
           ;; vterm--sentinel will kill the buffer
           :sentinel (when (or vterm-exit-functions
                               vterm-kill-buffer-on-exit)
                       #'vterm--sentinel))))

  ;; Change major-mode is not allowed
  ;; Vterm interfaces with an underlying process. Changing the major
  ;; mode can break this, leading to segmentation faults.
  (add-hook 'change-major-mode-hook
            (lambda () (interactive)
              (user-error "You cannot change major mode in vterm buffers")) nil t)

  (vterm--set-pty-name vterm--term (process-tty-name vterm--process))
  (process-put vterm--process 'adjust-window-size-function
               #'vterm--window-adjust-process-window-size)

  ;; Set the truncation slot for 'buffer-display-table' to the ASCII code for a
  ;; space character (32) to make the vterm buffer display a space instead of
  ;; the default truncation character ($) when a line is truncated.
  (let ((display-table (or buffer-display-table (make-display-table))))
    (set-display-table-slot display-table 'truncation 32)
    (setq buffer-display-table display-table)))

(defun vterm--tramp-get-shell (method)
  "Get the shell for a remote location as specified in `vterm-tramp-shells'.
The argument METHOD is the method string (as used by tramp) to get the shell
for, or t to get the default shell for all methods."
  (let* ((specs (cdr (assoc method vterm-tramp-shells)))
         (first (car specs))
         (second (cadr specs)))
    ;; Allow '(... login-shell) or '(... 'login-shell).
    (if (or (eq first 'login-shell)
            (and (consp first) (eq (cadr first) 'login-shell)))
        ;; If the first element is 'login-shell, try to determine the user's
        ;; login shell on the remote host.  This should work for all
        ;; POSIX-compliant systems with the getent command in PATH.  This
        ;; includes regular GNU/Linux distros, *BSDs, but not MacOS X.  If
        ;; the login-shell determination fails at any point, the second
        ;; element in the shell spec is used (if present, otherwise nil is
        ;; returned).
        (let* ((entry (ignore-errors
                        (with-output-to-string
                          (with-current-buffer standard-output
                            ;; The getent command returns the passwd entry
                            ;; for the specified user independently of the
                            ;; used name service (i.e., not only for static
                            ;; passwd files, but also for LDAP, etc).
                            ;;
                            ;; Use a shell command here to get $LOGNAME.
                            ;; Using the tramp user does not always work as
                            ;; it can be nil, e.g., with ssh host configs.
                            ;; $LOGNAME is defined in all POSIX-compliant
                            ;; systems.
                            (unless (= 0 (process-file-shell-command
                                          "getent passwd $LOGNAME"
                                          nil (current-buffer) nil))
                              (error "Unexpected return value"))
                            ;; If we have more than one line, the output is
                            ;; not the expected single passwd entry.
                            ;; Most likely, $LOGNAME is not set.
                            (when (> (count-lines (point-min) (point-max)) 1)
                              (error "Unexpected output"))))))
               (shell (when entry
                        ;; The returned Unix passwd entry is a colon-
                        ;; separated line.  The 6th (last) element specifies
                        ;; the user's shell.
                        (nth 6 (split-string entry ":" nil "[ \t\n\r]+")))))
          (or shell second))
      first)))

(defun vterm--get-shell ()
  "Get the shell that gets run in the vterm."
  (if (ignore-errors (file-remote-p default-directory))
      (with-parsed-tramp-file-name default-directory nil
        (or (vterm--tramp-get-shell method)
            (vterm--tramp-get-shell t)
            (with-connection-local-variables shell-file-name)
            vterm-shell))
    vterm-shell))

(defun vterm--enter-copy-mode ()
  (use-local-map nil)
  (vterm-send-stop)
  (let ((line-wraps (vterm--line-wraps))
        (inhibit-read-only t))
    (setq-local vterm--exit-copy-mode-function
                (apply-partially #'vterm--exit-copy-mode
                                 cursor-type line-wraps)
                truncate-lines nil
                cursor-type t)
    (mapc (lambda (cell)
            (delete-region (car cell)
                           (+ (car cell) (length (cdr cell)))))
          (sort line-wraps (lambda (a b) (> (car a) (car b)))))))

(defun vterm--exit-copy-mode (cursor-type* line-wraps)
  (setq-local cursor-type cursor-type*
              truncate-lines t)
  (let ((inhibit-read-only t))
    (mapc (lambda (cell)
            (save-excursion (goto-char (car cell))
                            (insert (cdr cell))))
          (sort line-wraps (lambda (a b) (< (car a) (car b))))))
  (vterm-reset-cursor-point)
  (use-local-map vterm-mode-map)
  (vterm-send-start)
  (setq-local vterm--exit-copy-mode-function nil))

(define-minor-mode vterm-copy-mode
  "Toggle `vterm-copy-mode'.

When `vterm-copy-mode' is enabled, the terminal will not display
additional output received from the underlying process and will
behave similarly to buffer in `fundamental-mode'.  This mode is
typically used to copy text from vterm buffers."
  :group 'vterm
  :lighter " VTermCopy"
  :keymap vterm-copy-mode-map
  (if (or (equal major-mode 'vterm-mode)
          (derived-mode-p 'vterm-mode))
      (if vterm-copy-mode
          (vterm--enter-copy-mode)
        (when vterm--exit-copy-mode-function
          (funcall vterm--exit-copy-mode-function)))
    (user-error "You cannot enable vterm-copy-mode outside vterm buffers")))

(defun vterm-copy-mode-done ()
  (interactive)
  (vterm-copy-mode -1))

(defun vterm--copy-mode-then ()
  (interactive)
  (let ((keys (key-description (this-command-keys))))
    (call-interactively #'vterm-copy-mode)
    (when-let ((command (keymap-lookup global-map keys)))
      (call-interactively command))))

(defun vterm--copy-mode-done-then ()
  (interactive)
  (let ((keys (key-description (this-command-keys))))
    (call-interactively #'vterm-copy-mode-done)
    (when-let ((command (keymap-lookup vterm-mode-map keys)))
      (call-interactively command))))

(defun vterm--self-insert ()
  "Send invoking key to libvterm."
  (interactive)
  (when vterm--term ;avoid segv if calling outside vterm buffer
    (dolist (key (vterm--translate-event-to-args
                  last-command-event))
      (apply #'vterm-send-key key))))

(defun vterm-send-key (key &optional shift meta ctrl)
  "Send KEY to libvterm with optional modifiers SHIFT, META and CTRL."
  (deactivate-mark)
  (when vterm--term ;avoid segv if calling outside vterm buffer
    (let ((inhibit-redisplay t)
          (inhibit-read-only t))
      (vterm--update vterm--term key shift meta ctrl))))

(defun vterm-send (key)
  "Send KEY to libvterm.  KEY can be anything `kbd' understands."
  (dolist (key (vterm--translate-event-to-args
                (listify-key-sequence (kbd key))))
    (apply #'vterm-send-key key)))

(defun vterm-send-next-key ()
  "Read next input event and send it to the libvterm.

With this you can directly send modified keys to applications
running in the terminal (like Emacs or Nano)."
  (interactive)
  (dolist (key (vterm--translate-event-to-args
                (read-event)))
    (apply #'vterm-send-key key)))

(defun vterm-send-start ()
  "Output from the system is started when the system receives START."
  (interactive)
  (vterm-send-key "<start>"))

(defun vterm-send-stop ()
  "Output from the system is stopped when the system receives STOP."
  (interactive)
  (vterm-send-key "<stop>"))

(defun vterm-clear-scrollback ()
  "Send <clear-scrollback> to libvterm."
  (interactive)
  (vterm-send-key "<clear_scrollback>"))

(defun vterm-clear (&optional arg)
  "Send <clear> to libvterm.

A prefix argument ARG negates the `vterm-clear-scrollback' setting."
  (interactive "P")
  (when (funcall (if arg #'not #'identity) vterm-clear-scrollback-when-clearing)
    (vterm-clear-scrollback))
  (vterm-send-key "l" nil nil :ctrl))

(defun vterm-mouse-set-point (event &optional promote-to-region)
  "Move point to the position clicked on with the mouse.
But when clicking to the unused area below the last prompt,
move the cursor to the prompt area."
  (interactive "e\np")
  (let ((pt (mouse-set-point event promote-to-region)))
    (if (= (count-words pt (point-max)) 0)
        (vterm-reset-cursor-point)
      pt))
  ;; Otherwise it selects text for every other click
  (keyboard-quit))

(defun vterm-send-string (string &optional paste-p)
  "Send the string STRING to vterm.
Optional argument PASTE-P paste-p."
  (when vterm--term
    (when paste-p
      (vterm--update vterm--term "<start_paste>" ))
    (dolist (letter (split-string string "" t))
      (vterm--update vterm--term letter))
    (when paste-p
      (vterm--update vterm--term "<end_paste>"))))

(defun vterm-insert (&rest strings)
  "Insert the arguments, either strings or characters, at point.

Provide similar behavior as `insert' for vterm."
  (when vterm--term ;avoid segv if calling outside vterm buffer
    (vterm--update vterm--term "<start_paste>")
    (dolist (s strings)
      (setq s (if (characterp s) (char-to-string s) s))
      (dolist (letter (split-string s "" t))
        (vterm--update vterm--term letter)))
    (vterm--update vterm--term "<end_paste>")))

(defun vterm-undo ()
  "Send `C-_' to the libvterm."
  (interactive)
  (vterm-send-key "_" nil nil t))

(defun vterm-yank (&optional arg)
  "Yank (paste) text in vterm.

Argument ARG is passed to `yank'."
  (interactive "P")
  (deactivate-mark)
  (save-excursion
    (let ((inhibit-read-only t)
          (this-command this-command) ;yank messes wit dis
          (start (vterm-reset-cursor-point)))
      (cl-letf (((symbol-function 'insert-for-yank) #'vterm-insert))
        (yank arg))
      (let ((end (vterm-reset-cursor-point)))
        (when (> (length arg) (- end start))
          (message "vterm-yank: partial yank"))))))

(defun vterm-yank-pop (&optional arg)
  "Replaced text just yanked with the next entry in the kill ring.

Argument ARG is passed to `yank'"
  (interactive "p")
  (save-excursion
    (cl-letf ((last-command (when (or (eq last-command 'vterm-yank)
                                      (eq last-command 'vterm-yank-pop))
                              ;; trick yank-pop
                              'yank))
              (yank-undo-function (lambda (_start _end) (vterm-undo)))
              ((symbol-function 'insert-for-yank) #'vterm-insert)
              (inhibit-read-only t))
      (yank-pop arg)))
  ;; yank-pop messed wit this
  (setq this-command 'vterm-yank-pop))

(defun vterm-yank-pop-dwim (&optional arg)
  "Context-aware yank-pop.
If previous command was `vterm-yank' or `vterm-yank-pop', rotate Emacs
kill ring. Otherwise, send M-y to bash to use its kill ring."
  (interactive "p")
  (if (memq last-command '(vterm-yank vterm-yank-pop))
      (vterm-yank-pop arg)
    (vterm--self-insert)))

(defun vterm-goto-char (pos)
  "Set point to POSITION for vterm.

The return value is `t' when point moved successfully."
  (when (and vterm--term
             (vterm-cursor-in-command-buffer-p)
             (vterm-cursor-in-command-buffer-p pos))
    (vterm-reset-cursor-point)
    (let ((diff (- pos (point))))
      (cond
       ((zerop diff) t)                   ;do not need move
       ((< diff 0)                        ;backward
        (while (and
                (vterm--backward-char)
                (> (point) pos)))
        (<= (point) pos))
       (t
        (while (and (vterm--forward-char)
                    (< (point) pos)))
        (>= (point) pos))))))

(defun vterm--forward-char ()
  "Move point 1 character forward ().

the return value is `t' when cursor moved."
  (vterm-reset-cursor-point)
  (let ((pt (point)))
    (vterm-send-key "<right>" nil nil nil)
    (cond
     ((= (point) (1+ pt)) t)
     ((and (> (point) pt)
           ;; move over the fake newline
           (get-text-property (1- (point)) 'vterm-line-wrap))
      t)
     ((and (= (point) (+ 4 pt))
           (looking-back (regexp-quote "^[[C") nil)) ;escape code for <right>
      (dotimes (_ 3) (vterm-send-key "<backspace>" nil nil nil)) ;;delete  "^[[C"
      nil)
     ((> (point) (1+ pt))             ;auto suggest
      (vterm-send-key "_" nil nil t) ;undo C-_
      nil)
     (t nil))))

(defun vterm--backward-char ()
  "Move point N characters backward.

Return count of moved characeters."
  (vterm-reset-cursor-point)
  (let ((pt (point)))
    (vterm-send-key "<left>" nil nil nil)
    (cond
     ((= (point) (1- pt)) t)
     ((and (= (point) (- pt 2))
           ;; backward cross fake newline
           (string-equal (buffer-substring-no-properties
                          (1+ (point)) (+ 2 (point)))
                         "\n"))
      t)
     ((and (= (point) (+ 4 pt))
           (looking-back (regexp-quote "^[[D") nil)) ;escape code for <left>
      (dotimes (_ 3) (vterm-send-key "<backspace>" nil nil nil)) ;;delete  "^[[D"
      nil)
     (t nil))))

(defun vterm--translate-event-to-args (event)
  "Translate EVENT as list of args for `vterm-send-key'.

When some input method is enabled, one key may generate
several characters, so the result of this function is a list,
looks like: ((\"m\" :shift ))"
  (let* ((modifiers (event-modifiers event))
         (shift (memq 'shift modifiers))
         (meta (memq 'meta modifiers))
         (ctrl (memq 'control modifiers))
         (raw-key (event-basic-type event))
         (ev-keys) keys)
    (if input-method-function
        (let ((inhibit-read-only t))
          (setq ev-keys (funcall input-method-function raw-key))
          (when (listp ev-keys)
            (dolist (k ev-keys)
              (when-let ((key (key-description (vector k))))
                (when (and (not (symbolp event)) shift (not meta) (not ctrl))
                  (setq key (upcase key)))
                (setq keys (append keys (list (list key shift meta ctrl))))))))
      (when-let ((key (key-description (vector raw-key))))
        (when (and (not (symbolp event)) shift (not meta) (not ctrl))
          (setq key (upcase key)))
        (setq keys (list (list key shift meta ctrl)))))
    keys))

;; see VTermSelectionMask in vterm.el
;; VTERM_SELECTION_CLIPBOARD = (1<<0),
;; VTERM_SELECTION_PRIMARY   = (1<<1),
(defconst vterm--selection-clipboard 1)   ;(1<<0)
(defconst vterm--selection-primary   2)   ;(1<<1)
(defun vterm--set-selection (mask data)
  "OSC 52 Manipulate Selection Data.
Search Manipulate Selection Data in
 https://invisible-island.net/xterm/ctlseqs/ctlseqs.html ."
  (when vterm-enable-manipulate-selection-data-by-osc52
    (let ((select-enable-clipboard select-enable-clipboard)
          (select-enable-primary select-enable-primary))
      (setq select-enable-clipboard
            (logand mask vterm--selection-clipboard))
      (setq select-enable-primary
            (logand mask vterm--selection-primary))
      (kill-new data)
      (message "kill-ring is updated by vterm OSC 52(Manipulate Selection Data)"))
    ))

;;;###autoload
(defun vterm (&optional arg)
  "Create an interactive Vterm buffer.
Start a new Vterm session, or switch to an already active
session.  Return the buffer selected (or created).

With a nonnumeric prefix arg, create a new session.

With a string prefix arg, create a new session with arg as buffer name.

With a numeric prefix arg (as in `C-u 42 M-x vterm RET'), switch
to the session with that number, or create it if it doesn't
already exist.

The buffer name used for Vterm sessions is determined by the
value of `vterm-buffer-name'."
  (interactive "P")
  (pop-to-buffer-same-window (vterm--get-buffer arg)))

;;;###autoload
(defun vterm-other-window (&optional arg)
  "As `vterm' in another window."
  (interactive "P")
  (pop-to-buffer (vterm--get-buffer arg)))

(defun vterm--get-buffer (arg)
  (cl-assert vterm-buffer-name)
  (let ((buf (cond ((numberp arg) (get-buffer-create
                                   (format "%s<%d>" vterm-buffer-name arg)))
                   ((stringp arg) (generate-new-buffer arg))
                   (arg (generate-new-buffer vterm-buffer-name))
                   (t (get-buffer-create vterm-buffer-name)))))
    (cl-assert (buffer-live-p buf))
    (prog1 buf
      (with-current-buffer buf
        (unless (derived-mode-p 'vterm-mode)
          (vterm-mode))))))

(defun vterm--flush-output (output)
  "Send the virtual terminal's OUTPUT to the shell."
  (process-send-string vterm--process output))
;; Terminal emulation
;; This is the standard process filter for term buffers.
;; It emulates (most of the features of) a VT100/ANSI-style terminal.

;; References:
;; [ctlseqs]: http://invisible-island.net/xterm/ctlseqs/ctlseqs.html
;; [ECMA-48]: https://www.ecma-international.org/publications/standards/Ecma-048.htm
;; [vt100]: https://vt100.net/docs/vt100-ug/chapter3.html

(defconst vterm-control-seq-regexp
  (concat
   ;; A control character,
   "\\(?:[\r\n\000\007\t\b\016\017]\\|"
   ;; a C1 escape coded character (see [ECMA-48] section 5.3 "Elements
   ;; of the C1 set"),
   "\e\\(?:[DM78c=]\\|"
   ;; another Emacs specific control sequence for term.el,
   "AnSiT[^\n]+\n\\|"
   ;; another Emacs specific control sequence for vterm.el
   ;; printf "\e]%s\e\\"
   "\\][^\e]+\e\\\\\\|"
   ;; or an escape sequence (section 5.4 "Control Sequences"),
   "\\[\\([\x30-\x3F]*\\)[\x20-\x2F]*[\x40-\x7E]\\)\\)")
  "Regexp matching control sequences handled by term.el.")

(defconst vterm-control-seq-prefix-regexp
  "[\032\e]")

(defun vterm--filter (process input*)
  "I/O Event.  Feeds PROCESS's INPUT to the virtual terminal.

Then triggers a redraw from the module."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      ;; 1. Bash spews, VTerm ingests
      (let* ((input (concat vterm--partial input*))
             (length (length input))
             (inhibit-eol-conversion t)
             (i 0))
        (setq vterm--partial nil)
        (while (< i length)
          (let* ((ctl-beg (string-match vterm-control-seq-regexp input i))
                 (ctl-end (if ctl-beg (match-end 0)
                            (setq ctl-beg (string-match vterm-control-seq-prefix-regexp
                                                        input i))
                            (if ctl-beg
                                (setq vterm--partial
                                      (substring input ctl-beg))
                              (setq ctl-beg length))
                            (1+ length)))
                 (decoded (decode-coding-string (substring input i ctl-beg)
                                                locale-coding-system t)))
            (when (= ctl-beg length)
              ;; No control sequences; check for half-baked ones
              (let ((partial-length 0)
                    (decoded-length (length decoded)))
                (while (and (< partial-length decoded-length)
                            (eq (char-charset (aref decoded
                                                    (- decoded-length 1 partial-length)))
                                'eight-bit))
                  (cl-incf partial-length))
                ;; VTERM--PARTIAL is a multibyte fragment for next iter
                (when (> partial-length 0)
                  (setq vterm--partial (substring decoded (- partial-length))
                        decoded (substring decoded 0 (- partial-length)))
                  (cl-decf length partial-length)
                  (cl-decf ctl-beg partial-length))))
            (ignore-errors (vterm--write-input vterm--term decoded))
            (when (<= ctl-end length)
              (ignore-errors (vterm--write-input
                              vterm--term (substring input ctl-beg ctl-end))))
            (setq i ctl-end))))
      ;; 2. Reflect apprised VTerm to display (judiciously as to avoid flicker)
      (let ((update (lambda (b)
                      (when (buffer-live-p b)
                        (with-current-buffer b
                          (let ((inhibit-redisplay t)
                                (inhibit-read-only t))
                            (vterm--update vterm--term)))))))
        (if (or (string-prefix-p "vterm" (symbol-name this-command))
                (string-prefix-p "vterm" (symbol-name last-command))
                noninteractive)
            (funcall update (current-buffer))
          (run-with-timer 1 nil update (current-buffer)))))))

(defun vterm--sentinel (process event)
  "Sentinel of vterm PROCESS.
Argument EVENT process event."
  (let ((buf (process-buffer process)))
    (run-hook-with-args 'vterm-exit-functions
                        (if (buffer-live-p buf) buf nil)
                        event)
    (if (and vterm-kill-buffer-on-exit (buffer-live-p buf))
        (kill-buffer buf))))

(defun vterm--window-adjust-process-window-size (process windows)
  "Adjust width of window WINDOWS associated to process PROCESS.

`vterm-min-window-width' determines the minimum width allowed."
  ;; We want `vterm-copy-mode' to resemble a fundamental buffer as much as
  ;; possible.  Hence, we must not call this function when the minor mode is
  ;; enabled, otherwise the buffer would be redrawn, messing around with the
  ;; position of the point.
  (unless vterm-copy-mode
    (let* ((size (funcall window-adjust-process-window-size-function
                          process windows))
           (width (car size))
           (height (cdr size))
           (inhibit-read-only t))
      (setq width (- width (vterm--get-margin-width)))
      (setq width (max width vterm-min-window-width))
      (when (and (processp process)
                 (process-live-p process)
                 (> width 0)
                 (> height 0))
        (vterm--set-size vterm--term height width)
        (cons width height)))))

(defun vterm--get-margin-width ()
  "Get margin width of vterm buffer when `display-line-numbers-mode' is enabled."
  (let ((width 0)
        (max-line-num (+ (frame-height) vterm-max-scrollback)))
    (when (bound-and-true-p display-line-numbers)
      (setq width (+ width 4
                     (string-width (number-to-string max-line-num)))))
    width))

(defun vterm--delete-lines (line-num count &optional delete-whole-line)
  "Delete COUNT lines from LINE-NUM.
If LINE-NUM is nonpositive, delete COUNT lines from (END + LINE-NUM).
If option DELETE-WHOLE-LINE is non-nil, then this command kills
the whole line including its terminating newline"
  (save-excursion
    (when (vterm--goto-line line-num)
      (delete-region (point) (line-end-position count))
      (when (and delete-whole-line (looking-at "\n"))
        (delete-char 1)))))

(defun vterm--goto-line (n)
  "Move point to beginning of Nth line.
If N is nonpositive, move point to beginning of (END + N)th line.
Return true on success."
  (zerop (if (> n 0)
             (progn
               (goto-char (point-min))
               (forward-line (1- n)))
           (goto-char (point-max))
           (forward-line n))))

(defun vterm--set-title (title)
  "Use TITLE to set the buffer name according to `vterm-buffer-name-string'."
  (when vterm-buffer-name-string
    (rename-buffer (format vterm-buffer-name-string title) t)))

(defun vterm--set-directory (path)
  "Set `default-directory' to PATH."
  (setq default-directory (or (vterm--get-directory path) default-directory)))

(defun vterm--get-directory (path)
  "Get normalized directory to PATH."
  (when path
    (let (directory)
      (if (string-match "^\\(.*?\\)@\\(.*?\\):\\(.*?\\)$" path)
          (progn
            (let ((user (match-string 1 path))
                  (host (match-string 2 path))
                  (dir (match-string 3 path)))
              (if (and (string-equal user user-login-name)
                       (string-equal host (system-name)))
                  (progn
                    (when (file-directory-p dir)
                      (setq directory (file-name-as-directory dir))))
                (setq directory (file-name-as-directory (concat "/-:" path))))))
        (when (file-directory-p path)
          (setq directory (file-name-as-directory path))))
      directory)))

(defun vterm--get-pwd (&optional linenum)
  "Get working directory at LINENUM."
  (when-let ((term-p vterm--term)
             (raw-pwd (vterm--get-pwd-raw
                       vterm--term
                       (or linenum (line-number-at-pos)))))
    (vterm--get-directory raw-pwd)))

(defun vterm--get-color (index &rest args)
  "Get color by INDEX from `vterm-color-palette'.

Special INDEX of -1 is used to represent default colors.  ARGS
may optionally contain `:underline' or `:inverse-video' for cells
with underline or inverse video attribute.  If ARGS contains
`:foreground', use foreground color of the respective face
instead of background."
  (let ((foreground (member :foreground args))
        (underline (member :underline args))
        (inverse-video (member :inverse-video args)))
    (funcall (if foreground #'face-foreground #'face-background)
             (cond
              ((and (>= index 0) (< index 16))
               (elt vterm-color-palette index))
              ((and (= index -1) foreground underline)
               'vterm-color-underline)
              ((and (= index -1) (not foreground) inverse-video)
               'vterm-color-inverse-video)
              (t 'default))
             nil 'default)))

(defun vterm--eval (str)
  "Check if string STR is `vterm-eval-cmds' and execute command.

All passed in arguments are strings and forwarded as string to
the called functions."
  (let* ((parts (split-string-and-unquote str))
         (command (car parts))
         (args (cdr parts))
         (fun (assoc command vterm-eval-cmds)))
    (if fun
        (apply (cadr fun) args)
      (message "Failed to find command: %s.  To execute a command,
                add it to the `vterm-eval-cmd' list" command))))

(defsubst vterm-prompt-column-p ()
  "Should be O(lg n) ?"
  (text-property-any (point-min) (point-max) 'vterm-prompt t))

(defun vterm-next-prompt (n)
  "Move to end of Nth next prompt in the buffer."
  (interactive "p")
  (unless n (setq n 1))
  (if (< n 0)
      (vterm-previous-prompt (- n))
    (if (vterm-prompt-column-p)
        (progn
          (dotimes (i (1+ n))
            (end-of-line (if (zerop i) 0 1))
            (let* ((pos (point))
                   (prev (get-text-property pos 'vterm-prompt)))
              (when-let ((end
                          (catch 'done
                            (prog1 nil
                              (while (setq pos (next-single-property-change
                                                pos 'vterm-prompt))
                                (when prev (throw 'done pos))
                                (setq prev (get-text-property pos 'vterm-prompt)))))))
                (goto-char end)))))
      (term-next-prompt n))))

(defun vterm-previous-prompt (n)
  "Move to end of Nth previous prompt in the buffer."
  (interactive "p")
  (unless n (setq n 1))
  (if (<= n 0)
      (vterm-next-prompt (- n))
    (if (vterm-prompt-column-p)
        (dotimes (_i n)
          (end-of-line 0)
          (unless (get-text-property (point) 'vterm-prompt) ;would be odd
            (when-let ((pos (previous-single-property-change (point) 'vterm-prompt)))
              (goto-char pos))))
      (term-previous-prompt n))))

(defun vterm-skip-prompt ()
  (if (vterm-prompt-column-p)
      (when-let ((end (if (get-text-property (point) 'vterm-prompt)
                          (next-single-property-change (point) 'vterm-prompt)
                        (if (get-text-property (max (point-min) (1- (point)))
                                               'vterm-prompt)
                            (point) ; p-s-p-c returns pos strictly less than point
                          (previous-single-property-change (point) 'vterm-prompt)))))
        (goto-char end))
    (term-skip-prompt)))

(defun vterm-cursor-in-command-buffer-p (&optional pt)
  "Check whether cursor in command buffer area."
  (save-excursion
    (when-let ((current (vterm-reset-cursor-point))
               (end (save-excursion (vterm-skip-prompt))))
      (>= (or pt current) end))))

(defun vterm-reset-cursor-point ()
  "Interactivise `vterm--reset-cursor-point'."
  (interactive)
  (when vterm--term
    (let ((inhibit-read-only t))
      (vterm--reset-cursor-point vterm--term))))

(defun vterm--line-wraps ()
  "Return list of conses (POS . STRING)."
  (save-excursion
    (goto-char (point-min))
    (let (result match)
      (while (setq match (text-property-search-forward 'vterm-line-wrap t t))
        (push (cons (prop-match-beginning match)
                    (buffer-substring (prop-match-beginning match)
                                      (prop-match-end match)))
              result))
      result)))

(provide 'vterm)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; vterm.el ends here
