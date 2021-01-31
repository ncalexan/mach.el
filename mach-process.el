;;; mach-process.el --- Mach Process Major Mode -*-lexical-binding: t-*-

;; Copyright (C) 2015  Kevin W. van Rooijen

;; Author: Kevin W. van Rooijen <kevin.van.rooijen@attichacker.com>
;; Keywords: processes, tools

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

;; Mach Process Major mode.
;; Used to run mach background processes.
;; Current supported mach functions:
;;  * mach-process-build              - Compile the current project.
;;  * mach-process-clean              - Remove the target directory.
;;  * mach-process-run                - Build and execute src/main.rs.
;;  * mach-process-test               - Run all unit tests.
;;  * mach-process-repeat             - Run the last mach-process command.
;;  * mach-process-current-file-tests - Run the current file unit tests.
;;  * mach-process-current-directory-tests - Run the current directory unit tests.
;;  * mach-process-check              - Run the optional mach command check.

;;
;;; Code:

;; (eval-after-load 'compile
;;   (let ((cmd (concat (flow-minor-binary) " status"))
;;         (regexp '(flow "^\\(Error:\\)[ \t]+\\(\\(.+\\):\\([[:digit:]]+\\)\\)"
;;                        3 4 nil (1) 2 (1 compilation-error-face))))
;;     (add-to-list 'compilation-error-regexp-alist 'flow)
;;     (add-to-list 'compilation-error-regexp-alist-alist regexp)

(require 'compile)

(defgroup mach-process nil
  "Mach Process group."
  :prefix "mach-process-"
  :group 'mach)

(defcustom mach-process--custom-path-to-bin
  (or (executable-find "mach")
      "./mach")
  "Custom path to the mach executable"
  :type 'file
  :group 'mach-process)

(defcustom mach-process--command-flags ""
  "Flags to be added to every mach command when run."
  :group 'mach-process
  :type 'string)

(defvar mach-process-mode-map
  (nconc (make-sparse-keymap) compilation-mode-map)
  "Keymap for mach major mode.")

(defvar mach-process-last-command nil "Command used last for repeating.")

(make-variable-buffer-local 'mach-process-last-command)

(defcustom mach-process--command-build "build"
  "Subcommand used by `mach-process-build'."
  :type 'string)

(defcustom mach-process--command-clean "clean"
  "Subcommand used by `mach-process-clean'."
  :type 'string)

(defcustom mach-process--command-run "run"
  "Subcommand used by `mach-process-run'."
  :type 'string)

(defcustom mach-process--command-test "test --headless"
  "Subcommand used by `mach-process-test'."
  :type 'string)

(defcustom mach-process--command-current-file-tests "test --headless"
  "Subcommand used by `mach-process-current-file-tests'."
  :type 'string)

(defcustom mach-process--command-current-file-gtests "gtest"
  "Subcommand used by `mach-process-current-file-tests'."
  :type 'string)

(defcustom mach-process--command-current-directory-tests "test --headless"
  "Subcommand used by `mach-process-current-directory-tests'."
  :type 'string)

(defcustom mach-process--command-check "check"
  "Subcommand used by `mach-process-check'."
  :type 'string)

(defcustom mach-process--command-current-file-lint "lint --fix"
  "Subcommand used by `mach-process-current-file-lint'."
  :type 'string)

(defcustom mach-process--command-outgoing-lint "lint -o --fix"
  "Subcommand used by `mach-process-outgoing-lint'."
  :type 'string)

(defface mach-process--ok-face
  '((t (:inherit success)))
  "Ok face"
  :group 'mach-process)

(defface mach-process--error-face
  '((t (:inherit error)))
  "Error face"
  :group 'mach-process)

(defface mach-process--warning-face
  '((t (:inherit warning)))
  "Warning face"
  :group 'mach-process)

(defface mach-process--pointer-face
  '((t (:inherit font-lock-negation-char-face)))
  "Pointer face"
  :group 'mach-process)

(defface mach-process--standard-face
  '((t (:inherit font-lock-comment-face)))
  "Standard face"
  :group 'mach-process)

(defface mach-process--errno-face
  '((t (:inherit link)))
  "Error number face"
  :group 'mach-process)

(defconst mach-process-test-regexp "^[[:space:]]*add_task.*function[[:space:]]*"
  "Regex to find current unit test function.")

(defconst mach-process-test-mod-regexp "^[[:space:]]*mod[[:space:]]+[[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*[[:space:]]*{")

(defconst mach-process-font-lock-keywords
  '(("^error\\:?" . 'mach-process--error-face)
    ("^warning\\:?" . 'mach-process--warning-face)
    ("^\s*\\^\\~*\s*$" . 'mach-process--pointer-face)
    ("^\s*Compiling.*" . 'mach-process--standard-face)
    ("^\s*Running.*" . 'mach-process--standard-face)
    ("^\s*Updating.*" . 'mach-process--standard-face)
    ("test result: FAILED." . 'mach-process--error-face)
    ("test result: ok." . 'mach-process--ok-face)
    ("test\s.*\sFAILED" . 'mach-process--error-face)
    ("test\s.*\sok" . 'mach-process--ok-face))
  "Minimal highlighting expressions for mach-process mode.")

;; resource:///modules/Foo.jsm -> $TOPOBJDIR/dist/bin/browser/modules/Foo.jsm.
;; resource://gre/modules/Foo.jsm -> $TOPOBJDIR/dist/bin/modules/Foo.jsm.
;; TODO: 0:03.32 pid:8483 JavaScript error: /Users/nalexander/Mozilla/objdirs/objdir-browser-artifact/_tests/xpcshell/browser/components/attribution/test/xpcshell/test_AttributionCode.js, line 37: ReferenceError: XRE_UPDATE_ROOT_DIR is not defined

;;  0:07.37 FAIL  - UpdRootD - "/Users/nalexander/Library/Caches/mozilla/firefox/updates/Users/nalexander/Mozilla/objdirs/objdir-browser-artifact/dist/NightlyDebug" == "xyz"
;; /Users/nalexander/Mozilla/objdirs/objdir-browser-artifact/_tests/xpcshell/browser/components/attribution/test/xpcshell/test_AttributionCode.js:null:97
;; /Users/nalexander/Mozilla/gecko/testing/xpcshell/head.js:_run_next_test/<:1618
;; /Users/nalexander/Mozilla/gecko/testing/xpcshell/head.js:_run_next_test:1618
;; /Users/nalexander/Mozilla/gecko/testing/xpcshell/head.js:run:777
;; /Users/nalexander/Mozilla/gecko/testing/xpcshell/head.js:_do_main:248
;; /Users/nalexander/Mozilla/gecko/testing/xpcshell/head.js:_execute_test:577
;; -e:null:1
;;  0:07.37 INFO exiting test
;;  0:07.37 INFO (xpcshell/head.js) | test run_next_test 0 finished (2)
;;  0:07.37 ERROR Unexpected exception NS_ERROR_ABORT: 
;; _abort_failed_test@/Users/nalexander/Mozilla/gecko/testing/xpcshell/head.js:833:20
;; do_report_result@/Users/nalexander/Mozilla/gecko/testing/xpcshell/head.js:934:5
;; Assert<@/Users/nalexander/Mozilla/gecko/testing/xpcshell/head.js:73:21
;; XXX proto.report@resource://testing-common/Assert.jsm:233:10
;; XXX equal@resource://testing-common/Assert.jsm:275:8
;; @/Users/nalexander/Mozilla/objdirs/objdir-browser-artifact/_tests/xpcshell/browser/components/attribution/test/xpcshell/test_AttributionCode.js:97:8
;; _run_next_test/<@/Users/nalexander/Mozilla/gecko/testing/xpcshell/head.js:1618:22
;; _run_next_test@/Users/nalexander/Mozilla/gecko/testing/xpcshell/head.js:1618:38
;; run@/Users/nalexander/Mozilla/gecko/testing/xpcshell/head.js:777:9
;; _do_main@/Users/nalexander/Mozilla/gecko/testing/xpcshell/head.js:248:6
;; _execute_test@/Users/nalexander/Mozilla/gecko/testing/xpcshell/head.js:577:5
;; @-e:1:1

(defvar mach-test-compilation-regexp
  (let ((re ; "[^@]+@[^-]\\(\\([^:]+\\):\\([0-9]+\\)\\(:\\([0-9]+\\)\\)?\\)")) .
       (rx
        bol
        (group-n
         1
         (* (not (any " @\r\n"))) ;; function name.  Anonymous functions don't have one.
         "@"
         (opt (or "resource:///modules/" "resource://gre/modules/" "-e"))
         (group-n 2
                  ;; (not (any "-"))
                  (+ (not (any ":\r\n")))) ;; filename
         ":"
         (group-n 3 (+ digit)) ;; line number
         (opt
          ":"
          (group-n 4 (+ digit))) ;; column number
         )
        eol)))
    (list re 2 3 4 nil 0)))

          

                 

  ;; (let ((file "\\([^\n]+\\)")
  ;;       (start-line "\\([0-9]+\\)")
  ;;       (start-col  "\\([0-9]+\\)"))
  ;;   (let ((re (concat "\\(" file ":" start-line "\\(?::" start-col)))) ; functionName@resource://path:line:col


;; (eval-after-load 'compile
;;   '(progn
;;      (add-to-list 'compilation-error-regexp-alist-alist
;;                   (cons 'mach mach-compilation-regexps))
;;      (add-to-list 'compilation-error-regexp-alist 'mach)))


(defun mach-process--defun-at-point-p ()
  (string-match mach-process-test-regexp
                (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position))))

(defun mach-process--project-root (&optional extra)
  "Find the root of the current mach project."
  (let ((root (locate-dominating-file (or buffer-file-name default-directory) "mach")))
    (when root
      (file-truename (concat root extra)))))

(define-derived-mode mach-process-mode compilation-mode "mach-Process."
  "Major mode for the mach process buffer."
  (use-local-map mach-process-mode-map)
  (setq major-mode 'mach-process-mode)
  (setq mode-name "mach-Process")
  (setq-local truncate-lines t)
  ;; (setq-local compilation-error-regexp-alist (list 'mach-test))
  ;; (setq-local compilation-error-regexp-alist-alist (list (cons 'mach-test mach-test-compilation-regexp)))
  (run-hooks 'mach-process-mode-hook)
  (font-lock-add-keywords nil mach-process-font-lock-keywords))

(defun mach-process--finished-sentinel (process event)
  "Execute after PROCESS return and EVENT is 'finished'."
  (compilation-sentinel process event)
  (when (equal event "finished\n")
    (message "mach process finished.")))

;; (defun mach-process--activate-mode (buffer)
;;   "Execute commands BUFFER at process start."
;;   (with-current-buffer buffer
;;     (funcall 'mach-process-mode)
;;     (setq-local window-point-insertion-type t)))

(defun mach-process--start (name command &optional last-cmd opens-external)
  "Start the mach process NAME with the mach command COMMAND.
OPENS-EXTERNAL is non-nil if the COMMAND is expected to open an external application.
Returns the created process."
  (let* ((buffer (concat "*mach " name "*"))
         (project-root (mach-process--project-root))
         (cmd
          (or last-cmd
              (mach-process--maybe-read-command
               (mach-process--augment-cmd-for-os opens-external
                                                 (mapconcat #'identity (list (format "env MOZCONFIG=%s MOZ_DISABLE_STACK_FIX=1" (mach-get-current-mozconfig))
                                                                             (shell-quote-argument mach-process--custom-path-to-bin)
                                                                             command
                                                                             mach-process--command-flags)
                                                            " ")))))
         (default-directory (or project-root default-directory)))
    (save-some-buffers (not compilation-ask-about-save)
                       (lambda ()
                         (and project-root
                              buffer-file-name
                              (string-prefix-p project-root (file-truename buffer-file-name)))))
    (setq mach-process-last-command (list name command cmd))
    (let ((default-directory (or (mach-process--project-root)
                                 default-directory))
          topobjdir (mach-get-current-topobjdir))
      (when topobjdir
        (setq compilation-search-path (list
                                       (f-join topobjdir "dist" "bin" "browser" "modules")
                                       (f-join topobjdir "dist" "bin" "modules"))))
      (compilation-start cmd 'mach-process-mode (lambda(_) buffer)))
    (let ((process (get-buffer-process buffer)))
      (set-process-sentinel process 'mach-process--finished-sentinel)
      process)))

(defun mach-process--maybe-read-command (default)
  "Prompt to modify the DEFAULT command when the prefix argument is present.
Without the prefix argument, return DEFAULT immediately."
  (if current-prefix-arg
      (read-shell-command "mach command: " default)
    default))

(defun mach-process--augment-cmd-for-os (opens-external cmd)
  "Augment the mach CMD according to OS. OPENS-EXTERNAL is non-nil
if the CMD is expected to open and external application."
  (if (and opens-external
           (not (member system-type '(windows-nt ms-dos)))
           (executable-find "setsid"))
      (concat "setsid -w " cmd)
    cmd))

;;;###autoload
(defun mach-process-build ()
  "Run the mach build command.
With the prefix argument, modify the command's invocation.
Mach: Compile the current project."
  (interactive)
  (mach-process--start "build" mach-process--command-build))

;;;###autoload
(defun mach-process-clean ()
  "Run the mach clean command.
With the prefix argument, modify the command's invocation.
Mach: Remove the target directory."
  (interactive)
  (mach-process--start "clean" mach-process--command-clean))

;;;###autoload
(defun mach-process-run ()
  "Run the mach run command.
With the prefix argument, modify the command's invocation.
Mach: Build and execute src/main.rs."
  (interactive)
  (mach-process--start "run" mach-process--command-run))

;;;###autoload
(defun mach-process-test ()
  "Run the mach test command.
With the prefix argument, modify the command's invocation.
Mach: Run the tests."
  (interactive)
  (mach-process--start "test" mach-process--command-test))

;;;###autoload
(defun mach-process-current-file-tests ()
  "Run the mach test command for the current file.
With the prefix argument, modify the command's invocation.
Mach: Run the tests."
  (interactive)
  (let* ((target (or (buffer-file-name) default-directory))
         (base (f-base target))
         (ext (downcase (f-ext (buffer-file-name)))))
    (if (and (s-equals? "cpp" ext)
             (string-match "^Test\\(.+\\)" base))
        (mach-process--start "test" (concat mach-process--command-current-file-gtests
                                            " '"
                                            (match-string 1 base)
                                            ".*'"))
      (mach-process--start "test" (concat mach-process--command-current-file-tests
                                          " "
                                          target)))))
;;;###autoload
(defun mach-process-current-directory-tests ()
  "Run the mach test command for the current directory.
With the prefix argument, modify the command's invocation.
Mach: Run the tests."
  (interactive)
  (mach-process--start "test" (concat mach-process--command-current-directory-tests
                                       " "
                                       default-directory)))

;;;###autoload
(defun mach-process-check ()
  "Run the mach check command.
With the prefix argument, modify the command's invocation.
Mach: Check compile the current project.
Requires mach-check to be installed."
  (interactive)
  (mach-process--start "check" mach-process--command-check))

;;;###autoload
(defun mach-process-current-file-lint ()
  "Run the mach lint command for the current file.
With the prefix argument, modify the command's invocation.
Mach: Lint."
  (interactive)
  (mach-process--start "lint" (concat mach-process--command-current-file-lint
                                      " "
                                      (or (buffer-file-name) default-directory))))

;;;###autoload
(defun mach-process-outgoing-lint ()
  "Run the mach lint command for all outgoing files.
With the prefix argument, modify the command's invocation.
Mach: Lint."
  (interactive)
  (mach-process--start "lint" mach-process--command-outgoing-lint))

;;;###autoload
(defun mach-process-repeat ()
  "Run the last mach-process command."
  (interactive)
  (if mach-process-last-command
      (apply 'mach-process--start mach-process-last-command)
    (message "No last mach command.")))

(define-key mach-process-mode-map (kbd "n") 'forward-button)
(define-key mach-process-mode-map (kbd "p") 'backward-button)

(provide 'mach-process)
;;; mach-process.el ends here
