(setq GNU (not (string-match "XEmacs\\|Lucid" (emacs-version))))
(unless (boundp 'running-xemacs)
  (defvar running-xemacs nil))

;; VG: use MELPA repository
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

;; Installs packages
;;
;; myPackages contains a list of package names
(defvar myPackages
  '(better-defaults                 ;; Set up some better Emacs defaults
    elpy                            ;; Emacs Lisp Python Environment
    flycheck                        ;; On the fly syntax checking
    py-autopep8                     ;; Run autopep8 on save
    blacken                         ;; Black formatting on save
    material-theme                  ;; Theme
    )
  )

;; Scans the list in myPackages
;; If the package listed is not already installed, install it
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

;(load-theme 'material t)            ;; Load material theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq default-screen-alist '((width . 200) (height . 155)))

(setq c-default-style "k&r")
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

(setq line-number-mode t)
(setq column-number-mode t)

;; (set-default 'truncate-lines t)

(if GNU
    (delete-selection-mode 1)
  (pending-delete-mode 1))

;; set default coding to unix
(set-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
;; the following is not working anymore in 26.1 http://ergoemacs.org/emacs/emacs26_features.html
; (set-default default-buffer-file-coding-system 'utf-8-unix)

;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

;; ========== Line by line scrolling ==========
;; This makes the buffer scroll by only a single line when the up or
;; down cursor keys push the cursor (tool-bar-mode) outside the
;; buffer. The standard emacs behaviour is to reposition the cursor in
;; the center of the screen, but this can make the scrolling confusing
(setq scroll-step 1)

(global-set-key [f3] 'grep)
(global-set-key [f9] 'compile)
(global-set-key (kbd "<C-f5>") '(lambda() "copy to register 5" (interactive) (copy-to-register '5 (region-beginning) (region-end) nil)(message "cp reg %s word %s %s" '5 (region-beginning) (region-end))))
(global-set-key (kbd "<S-f5>") '(lambda() "insert register 5" (interactive) (insert-register '5 nil)))
(global-set-key (kbd "<C-f6>") '(lambda() "copy to register 6" (interactive) (copy-to-register '6 (region-beginning) (region-end) nil)(message "cp reg %s word %s %s" '6 (region-beginning) (region-end))))
(global-set-key (kbd "<S-f6>") '(lambda() "insert register 6" (interactive) (insert-register '6 nil)))
(global-set-key (kbd "<C-f7>") '(lambda() "copy to register 7" (interactive) (copy-to-register '7 (region-beginning) (region-end) nil)(message "cp reg %s word %s %s" '7 (region-beginning) (region-end))))
(global-set-key (kbd "<S-f7>") '(lambda() "insert register 7" (interactive) (insert-register '7 nil)))
(global-set-key (kbd "<C-f8>") '(lambda() "copy to register 8" (interactive) (copy-to-register '8 (region-beginning) (region-end) nil)(message "cp reg %s word %s %s" '8 (region-beginning) (region-end))))
(global-set-key (kbd "<S-f8>") '(lambda() "insert register 8" (interactive) (insert-register '8 nil)))
(if (string-equal system-type "darwin")
    ;; copy ctrl-insert
    (global-set-key (kbd "<C-help>") 'clipboard-kill-ring-save)
    ;; paste shift-insert ... shift does not register with insert and help just starts...help
    (global-set-key (kbd "<S-help>") 'clipboard-yank))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert to point, duplicate line, select word, delete line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun val-set-ins-point ()
  "Set insertion point; use with val-cp-to-ins-point"
  (interactive)
  (setq inspoint (point))
  (setq insbuffer (current-buffer))
  (message "Insert buf: %s, point: %d" (buffer-name) inspoint))

(defun val-cp-to-ins-point (start end)
  "Copy the region to insertion point."
  (interactive "r")
  (setq srcbuffer (current-buffer))
  (let (cpbuffer (current-buffer))
    (switch-to-buffer insbuffer)
    (goto-char inspoint)
    (message "Copy to dest")
    (insert-buffer-substring srcbuffer start end)))

(defun val-dup-line ()
  "Duplicate current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((startpos (point)))
      (end-of-line)
      (let ((endpos (point)))
	(insert "\n")
	(insert-buffer-substring (current-buffer) startpos endpos)))))

(defun val-sel-word ()
  "Select curent word."
  (interactive)
  (backward-word)
  (set-mark (point))
  (forward-word)
  (exchange-point-and-mark)
    ;(copy-region-as-kill (point) (mark))
  (message "select word %s %s" (mark) (point)))

;; ===== Function to delete a line =====
;; First define a variable which will store the previous column position
(defvar previous-column nil "Save the column position")

;; Define the nuke-line function. The line is killed, then the newline
;; character is deleted. The column which the cursor was positioned at is then
;; restored. Because the kill-line function is used, the contents deleted can
;; be later restored by usibackward-delete-char-untabifyng the yank commands.
(defun nuke-line()
  "Kill an entire line, including the trailing newline character"
  (interactive)

  ;; Store the current column position, so it can later be restored for a more
  ;; natural feel to the deletion
  (setq previous-column (current-column))

  ;; Now move to the end of the current line
  (end-of-line)

  ;; Test the length of the line. If it is 0, there is no need for a
  ;; kill-line. All that happens in this case is that the new-line character
  ;; is deleted.
  (if (= (current-column) 0)
    (delete-char 1)

    ;; This is the 'else' clause. The current line being deleted is not zero
    ;; in length. First remove the line by moving to its start and then
    ;; killing, followed by deletion of the newline character, and then
    ;; finally restoration of the column position.
    (progn
      (beginning-of-line)
      (kill-line)
      (delete-char 1)
      (move-to-column previous-column))))

(defun move-cursor-previous-pane()
  "Move cursor to the previous pane."
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-<return>") 'val-dup-line)
(global-set-key "\C-cd" 'val-dup-line)
(global-set-key (kbd "S-<return>") 'val-sel-word)
(global-set-key "\C-cs" 'val-sel-word)
(global-set-key "\C-ci" 'val-set-ins-point)
(global-set-key "\C-cc" 'val-cp-to-ins-point)
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key (kbd "S-<delete>") 'nuke-line)
(global-set-key [home] 'move-beginning-of-line)
(global-set-key [end] 'move-end-of-line)
(global-set-key (kbd "S-<tab>") 'tab-to-tab-stop)
(global-set-key (kbd "M-<right>") 'forward-sexp)
(global-set-key (kbd "M-<left>") 'backward-sexp)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-S-<tab>") 'move-cursor-previous-pane)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bind windows keyboard keys while running on Mac
(if GNU
    (progn
     (global-set-key [S-kp-delete] 'nuke-line)
     ;;(global-set-key [C-H-help] 'ns-copy-including-secondary)
     (global-set-key [C-H-help] 'kill-ring-save)
     (global-set-key [S-H-help] 'yank)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special setings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add region to search ring to start a search with region quickly
(if GNU
    ;; gnu emacs has fewer args to buffer-substring, and needs to explicitly deactivate mark
    (add-hook 'isearch-mode-hook '(lambda() "add region to search-ring" (interactive)
                                    (if (region-active-p)
                                        (setq search-ring (cons (buffer-substring (region-beginning) (region-end)) search-ring)
                                              deactivate-mark t))))
  (add-hook 'isearch-mode-hook '(lambda() "add region to search-ring" (interactive)
                                  (if (region-active-p)
                                      (setq search-ring (cons (buffer-substring (region-beginning) (region-end) nil) search-ring))))))

(defun scroll-down-keep-cursor ()
   ;; Scroll the text one line down while keeping the cursor
   (interactive)
   (scroll-down 1))

(defun scroll-up-keep-cursor ()
   ;; Scroll the text one line up while keeping the cursor
   (interactive)
   (scroll-up 1))

;Bind the functions to the /-key and the *-key (on the numeric keypad) with:
(global-set-key (kbd "C-<prior>") 'scroll-down-keep-cursor)
(global-set-key (kbd "C-<next>") 'scroll-up-keep-cursor)

;; ; set correct path to find/grep on windows with cygwin
;; (when (or (eq system-type 'windows-nt) (eq system-type 'msdos))
;;   (setenv "PATH" (concat "c:\\cygwin\\bin;" (getenv "PATH")))
;;   (setq find-program "c:\\cygwin\\bin\\find.exe")
;;   ; try to improve slow performance on windows.
;;   (setq w32-get-true-file-attributes nil))

;; (unless (string-equal system-type "darwin")
;;   (setq grep-command "wsl grep --exclude-dir=\".git\" -n \"\" -R *")
;;   (setq grep-command "wsl echo * | wsl xargs grep --exclude-dir=\".git\" -nHR ")
;; 	;; (setq grep-command "find ./ -type f | grep -v \"\\.svn/\\|test/\\|\\.dep\\|\\.obj\\|\\.exe\\|\\.git/\\|/doc/\\|/vc/\\|/doxy\\|~$\\|#$\" | xargs grep -n ")
;; 	;(setq grep-command "find ./ | grep -v \"\\.svn/\\|.git/\\|/doc/\\|/vc/\\|/doxy\\|~$\\|#$\" | xargs grep -n ")
;; )

;prevent emacs from spliting windows by itself
(setq split-height-threshold nil
      split-width-threshold nil)

(if (string-equal system-type "darwin")
    (setq mac-command-modifier 'meta
          exec-path (append exec-path '("/usr/local/bin"))
          ggtags-executable-directory "/usr/local/bin"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;; PERL ;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;;;;;;;;;;;;;;;;;;; PERL ;;;;;;;;;;;;;;;;;;;;
(add-hook 'cperl-mode-hook 'n-cperl-mode-hook t)
(defun n-cperl-mode-hook ()
  (setq cperl-indent-level 4)
  (setq cperl-continued-statement-offset 0)
  (setq cperl-extra-newline-before-brace t)
  ;(set-face-background 'cperl-array-face "wheat")
  ;(set-face-background 'cperl-hash-face "wheat")
)

;;;;;;;;;;;;;;;;;;;; BASH ;;;;;;;;;;;;;;;;;;;;

(defun valg-setup-sh-mode ()
  "My own personal preferences for `sh-mode'.

This is a custom function that sets up the parameters I usually
refer for `sh-mode'.  It is automatically added to
`sh-mode-hook', but is can also be called interactively."
  (interactive)
  (setq sh-indent-for-do 0
        sh-indent-after-do '+
        sh-indent-comment t))
(add-hook 'sh-mode-hook 'valg-setup-sh-mode)

;;;;;;;;;;;;;;;;;;;; JASON ;;;;;;;;;;;;;;;;;;;;
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(show-paren-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Others
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; macros
; (defalias 'logerr
;   (read-kbd-macro "TAB LOG_ER(S-SPC LEVEL, SPC \"Err: SPC \" SPC);"))
; (defalias 'logwar (read-kbd-macro
; "TAB LOG_EA 2*<backspace> WA(SPC level 5*<backspace> LEVEL. <backspace> , SPC \"wA 2*<backspace> w <backspace> War: SPC \" SPC);"))
; (defalias 'loginf (read-kbd-macro
; "TAB lo 2*<backspace> K <backspace> LOG_IN* <backspace> (SPC LEVEL, SPC : <backspace> \"ING 2*<backspace> nf: SPC \" SPC _ <backspace>);"))
; (defalias 'logdbg (read-kbd-macro
; "TAB LOG_IN(SPC LEVEL10, SPC \"DEBUG: SPC \" SPC _ <backspace>);"))
;
; (defalias 'header-shell (read-kbd-macro
; ;(concat "#!/bin/bash RET #/// SPC \\file SPC RET #/// SPC \\brief RET # RET # SPC Created SPC by SPC Valeriu SPC Goldberger SPC on SPC "
; (concat "#!/bin/bash RET  RET #/// SPC \\file SPC  SPC "
;         (buffer-file-name nil)
;         "RET #/// SPC \\brief RET # RET # SPC Created SPC by SPC Valeriu SPC Goldberger SPC on SPC "
;         (format-time-string "%D SPC %R" (current-time))
;         " RET # RET")))

(defun logerr ()
  "insert C LOG_ERR call"
  (interactive)
  (indent-according-to-mode)
  (insert "LOG_ER(LEVEL, \"Err: \");\n"))
(defun logwar ()
  "insert C LOG_WAR call"
  (interactive)
  (indent-according-to-mode)
  (insert "LOG_WA(LEVEL, \"War: \");\n"))
(defun loginf ()
  "insert C LOG_INF call"
  (interactive)
  (indent-according-to-mode)
  (insert "LOG_IN(LEVEL, \"Inf: \");\n"))
(defun logdbg ()
  "insert C LOG_INF DEBUG call"
  (interactive)
  (indent-according-to-mode)
  (insert "LOG_IN(LEVEL10, \"DEBUG: \");\n"))
(defun header-shell ()
  "insert C LOG_ERR call"
  (interactive)
  (indent-according-to-mode)
  (insert
   (concat "#!/bin/bash

#/// \\file  " (buffer-name) "
#/// \\brief
#
# Created by Valeriu Goldberger on "
           (format-time-string "%D SPC %R" (current-time))
           "\n#\n"
)))

(defun vg-setter-c (fctName varName varType)
  "…"
  (interactive "sEnter function's name: \nsEnter variable name: \nsEnter variable type(int|char):")
  (message "Fct name: %s, Var name is: %s, Typeis %s" fctName varName varType)
  (insert "void " fctName "(")
  (if (string= varType "int")
      (insert "int " varName)
    (insert "const char *"))
  (insert " x) {\n    ")
  (if (string= varType "int")
      (insert varName " = x;\n")
      (insert "strncpy(" varName ", x, sizeof(" varName ");\n"))
  (insert "}\n"))

; macro to apply google style over ValG style.
(fset 'vg-apply-google-style
      (lambda (&optional arg) "Keyboard macro." (interactive "p")
        (kmacro-exec-ring-item (quote ([134217788 201326629 40 32 43 return 40 return 33 134217788 201326629 32 43 41 return 41 return 33 134217788 134217765 32 105 102 40 return 32 105 102 32 40 return 33 134217788 201326629 32 119 104 105 108 101 40 return 32 119 104 105 108 101 32 40 return 33 134217788 201326629 32 102 111 114 40 return 32 102 111 114 32 40 return 33 134217788 201326629 32 115 119 105 116 99 104 40 return 32 115 119 105 116 99 104 32 40 return 33 134217788 201326629 92 40 94 32 42 91 94 47 32 93 46 42 92 41 17 10 32 42 123 17 10 return 92 49 32 123 17 10 return 33 134217788 201326629 32 43 36 return return 33 134217788] 0 "%d")) arg)))

;; installed package auto-complete
; start auto-complete with emacs
(require 'auto-complete)
; perform default config for auto-complete
(require 'auto-complete-config)
;;; VG: uncommenting the bellow will allow nice autocompletion menus, but very slow...
;(ac-config-default)
;;; VG: bellow is supposed to make autocomplete faster, but it doesn't
;(ac-flyspell-workaround)

;; installed package yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; installed flymake-google-cpplint-load
;; copied cpplint
; start flymake-google-cpplint-load
; let's define a function for flymake initialization
(defun my:flymake-google-init ()
  (require 'flymake-google-cpplint)
  (custom-set-variables
    ;; '(flymake-google-cpplint-verbose "30")
   '(flymake-google-cpplint-linelength "120"))
    ;; '(flymake-google-cpplint-command "c:/Python27/Scripts/cpplint.bat"))
  (if (file-readable-p "c:/Python27/Scripts/cpplint.bat")
      (custom-set-variables
       '(flymake-google-cpplint-command "c:/Python27/Scripts/cpplint.bat")))
  (flymake-google-cpplint-load)
)
(add-hook 'c-mode-hook 'my:flymake-google-init)
(add-hook 'c++-mode-hook 'my:flymake-google-init)

;;; ; turn on Semantic
;;; (semantic-mode 1)
;;; ; let's define a function which adds semantic as a suggestion backend to auto complete
;;; ; and hook this function to c-mode-common-hook
;;; (defun my:add-semantic-to-autocomplete()
;;;   (add-to-list 'ac-sources 'ac-source-semantic)
;;;)
;;; (add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
; turn on ede mode
(global-ede-mode 1)



;;; golang

;;Load Go-specific language syntax
;;For gocode use https://github.com/mdempsky/gocode

;;Goimports
(defun go-mode-setup ()
  (linum-mode 1)
  ;(go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump)
  (setq compile-command "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golint")
  (setq compilation-read-command nil)
  ;;  (define-key (current-local-map) "\C-c\C-c" 'compile)
  (local-set-key (kbd "M-,") 'compile))
(add-hook 'go-mode-hook 'go-mode-setup)

;;; ;;Load auto-complete
;; (require 'go-autocomplete)
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (ac-flyspell-workaround)

;;Go rename
; (require 'go-rename)

;;Configure golint
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

;;; ;;Smaller compilation buffer
;;; (setq compilation-window-height 14)
;;; (defun my-compilation-hook ()
;;;   (when (not (get-buffer-window "*compilation*"))
;;;     (save-selected-window
;;;       (save-excursion
;;;         (let* ((w (split-window-vertically))
;;;                (h (window-height w)))
;;;           (select-window w)
;;;           (switch-to-buffer "*compilation*")
;;;           (shrink-window (- h compilation-window-height)))))))
;;; (add-hook 'compilation-mode-hook 'my-compilation-hook)

;;Other Key bindings
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

;;Compilation autoscroll
(setq compilation-scroll-output t)


;;; python

;; Enable elpy
(elpy-enable)

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; for pdb, create these files: (https://stackoverflow.com/questions/9167614/python-mode-in-emacs-no-such-file-or-directory-pdb)
;; windows: pdb.bat: python -u -m pdb %1
;; linux/mac: #!/bin/sh
;;            exec python -m pdb "$@"

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(beacon-color "#d33682")
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes (quote (material)))
 '(custom-safe-themes
   (quote
    ("732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "5a45c8bf60607dfa077b3e23edfb8df0f37c4759356682adf7ab762ba6b10600" "bdb4509c123230a059d89fc837c40defdecee8279c741b7f060196b343b2d18d" "ed17fef69db375ae1ced71fdc12e543448827aac5eb7166d2fd05f4c95a7be71" "6515fcc302292f29a94f6ac0c5795c57a396127d5ea31f37fc5f9f0308bbe19f" "54f2d1fcc9bcadedd50398697618f7c34aceb9966a6cbaa99829eb64c0c1f3ca" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(fci-rule-color "#383838")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(flymake-google-cpplint-linelength "120")
 '(frame-background-mode (quote dark))
 '(grep-command "wsl echo * | wsl xargs grep --exclude-dir=\".git\" -nHR ")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (elpy better-defaults material-theme groovy-mode flymake-json json-navigator nofrils-acme-theme highlight-indent-guides iedit json-snatcher terraform-mode web-mode yaml-imenu yaml-mode zenburn-theme json-mode nhexl-mode yasnippet auto-complete deferred go-autocomplete go-eldoc go-mode go-playground go-playground-cli golint gotest color-theme color-theme-sanityinc-solarized color-theme-solarized flycheck flycheck-yamllint flymake-cursor flymake-easy flymake-google-cpplint flymake-yaml ggtags epoch-view)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
