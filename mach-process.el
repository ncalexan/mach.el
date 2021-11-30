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
;;  * mach-process-clobber            - Remove the current object directory.
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
(require 'dash)
(require 'f)
(require 's)
(require 'transient)

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

(defcustom mach-process--command-flags '()
  "Flags to be added to every mach command when run."
  :group 'mach-process
  :type '(list string))

(defvar mach-process-mode-map
  (nconc (make-sparse-keymap) compilation-mode-map)
  "Keymap for mach major mode.")

(defvar mach-process-last-command nil "Command used last for repeating.")

(make-variable-buffer-local 'mach-process-last-command)

(defcustom mach-process--command-build "build"
  "Subcommand used by `mach-process-build'."
  :type 'string)

(defcustom mach-process--command-clobber "clobber"
  "Subcommand used by `mach-process-clobber'."
  :type 'string)

(defcustom mach-process--command-run "run"
  "Subcommand used by `mach-process-run'."
  :type 'string)

(defcustom mach-process--command-test "test"
  "Subcommand used by `mach-process-test'."
  :type 'string)

(defcustom mach-process--command-current-file-tests "test"
  "Subcommand used by `mach-process-current-file-tests'."
  :type 'string)

(defcustom mach-process--command-current-file-gtests "gtest"
  "Subcommand used by `mach-process-current-file-tests'."
  :type 'string)

(defcustom mach-process--command-current-directory-tests "test"
  "Subcommand used by `mach-process-current-directory-tests'."
  :type 'string)

(defcustom mach-process--command-check "check"
  "Subcommand used by `mach-process-check'."
  :type 'string)

(defcustom mach-process--command-current-file-lint "lint"
  "Subcommand used by `mach-process-current-file-lint'."
  :type 'string)

(defcustom mach-process--command-outgoing-lint "lint --outgoing"
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

;; Match `mach lint` errors.  Modeled on
;; https://github.com/Fuco1/compile-eslint/blob/20b2d34894530818ca042e5b5a9bb9ab6d024cc7/compile-eslint.el.

;;;###autoload
(defun compile-mach-lint--find-filename ()
  "Find the filename for current error."
  (save-match-data
    (save-excursion
      ;; TODO: Make sure that this works on Windows as well.
      (when (re-search-backward (rx bol (group (| (seq "/" (+ any))
                                                  (seq letter ":" (| "/" "\\") (+ any)))) eol))
        (list (match-string 1))))))

;;;###autoload
(eval-after-load 'compile
  (lambda ()
    (let ((form `(mach-lint
		  ,(rx-to-string
                    '(and (group (group (+ digit)) ":" (group (+ digit)))
			  (+ " ") (or "error" "warning")))
		  compile-mach-lint--find-filename
		  2 3 2 1)))
      (if (assq 'mach-lint compilation-error-regexp-alist-alist)
	  (setf (cdr (assq 'mach-lint compilation-error-regexp-alist-alist)) (cdr form))
	(push form compilation-error-regexp-alist-alist)))))

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
  (let ((root (locate-dominating-file (or buffer-file-name default-directory)
                                      (lambda (dir)
                                        (and (file-directory-p dir)
                                             (let ((file (expand-file-name "mach" dir)))
                                               (and (file-exists-p file)
                                                    (not (file-directory-p file)))))))))
    (when root
      (file-truename (concat root extra)))))

(define-derived-mode mach-process-mode compilation-mode "mach-Process."
  "Major mode for the mach process buffer."
  (use-local-map mach-process-mode-map)
  (setq major-mode 'mach-process-mode)
  (setq mode-name "mach-Process")
  (setq-local truncate-lines t)
  (add-to-list (make-local-variable 'compilation-error-regexp-alist) 'mach-lint)
  ;; (add-to-list 'compilation-error-regexp-alist 'mach-test)
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

(defvar-local mach-process--originating-buffer nil)

(defun mach-environment-variables ()
  (interactive)
  (let ((env (or (catch 'result
                   (when-let ((fboundp 'projectile-root)
			                  (root (projectile-project-root)))
                     (while root
                       (when-let ((eshell-buffer (get-buffer (projectile-generate-process-name "eshell" nil))))
                         (with-current-buffer eshell-buffer
                           (throw 'result (eshell-environment-variables))))
                       (setq root (f-parent root)))))
                 process-environment)))
    ;; Avoid `mach lint` failing printing a UTF-8 cross or a check mark under Windows.
    (cons "PYTHONIOENCODING=utf-8"
          (cons "MOZ_PURGE_CACHES=1"
                (-sort #'s-less?
                       (--filter (and (s-contains? "=" it)
                                      (not (s-ends-with? "=" it))
                                      (s-starts-with? "MOZ" it)
                                      (not (s-starts-with? "MOZILLABUILD=" it))) env))))))

(defun mach-process--start (name command &optional last-cmd opens-external)
  "Start the mach process NAME with the mach command COMMAND.
OPENS-EXTERNAL is non-nil if the COMMAND is expected to open an external application.
Returns the created process."
  (let* ((originating-buffer (current-buffer))
         (buffer (concat "*mach " name "*"))
         (project-root (mach-process--project-root))
         (cmd
          (or last-cmd
              (mach-process--maybe-read-command
               (mach-process--augment-cmd-for-os opens-external
                                                 (mapconcat #'identity (append (list "env")
                                                                               (mach-environment-variables)
                                                                               (list (shell-quote-argument mach-process--custom-path-to-bin)
                                                                                     command)
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
    (with-current-buffer buffer
      (setq mach-process--originating-buffer originating-buffer))
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
(defun mach-process-build-all (args)
  (interactive (list
                (mach-process-build-arguments)))
  (mach-process--start "build" (s-join " " (append
                                            (list mach-process--command-build)
                                            args))))

;;;###autoload
(defun mach-process-build-binaries (args)
  (interactive (list
                (mach-process-build-arguments)))
  (mach-process--start "build" (s-join " " (append
                                            (list mach-process--command-build "binaries")
                                            args))))

;;;###autoload
(defun mach-process-build-faster (args)
  (interactive (list
                (mach-process-build-arguments)))
  (mach-process--start "build" (s-join " " (append
                                            (list mach-process--command-build "faster")
                                            args))))

;;;###autoload (autoload 'mach-process-build "mach-process" nil t)
(transient-define-prefix mach-process-build ()
  "Run the mach test command.
With the prefix argument, modify the command's invocation.
Mach: Compile the current project."
  ["Arguments"
   ("-v" "Verbose" "--verbose")
   ]
  [["Actions"
    ("a" "All"           mach-process-build-all)
    ("b" "Binaries"      mach-process-build-binaries)
    ("f" "Faster"        mach-process-build-faster)
   ]])

(defun mach-process-build-arguments ()
  (transient-args 'mach-process-build))

;;;###autoload
(defun mach-process-clobber ()
  "Run the mach clobber command.
With the prefix argument, modify the command's invocation.
Mach: Remove the object directory."
  (interactive)
  (mach-process--start "clobber" mach-process--command-clobber))

;;;###autoload
(defun mach-process-run ()
  "Run the mach run command.
With the prefix argument, modify the command's invocation.
Mach: Run the current project."
  (interactive)
  (mach-process--start "run" mach-process--command-run))

;;;###autoload
(defun mach-process-current-file-tests (args)
  "Run the mach test command for the current file.
With the prefix argument, modify the command's invocation.
Mach: Run the tests."
  (interactive (list
                (mach-process-test-arguments)))
  (let* ((target (or (buffer-file-name) default-directory))
         (target (if (file-remote-p target)
                     (tramp-file-name-localname (tramp-dissect-file-name target))
                   target))
         (base (f-base target))
         (ext (downcase (f-ext (buffer-file-name)))))
    (if (and (s-equals? "cpp" ext)
             (string-match "^Test\\(.+\\)" base))
        (mach-process--start "test" (concat
                                     (s-join " " (append
                                                  (list mach-process--command-current-file-gtests)
                                                  args))
                                     " '"
                                     (match-string 1 base)
                                     ".*'"))
      (mach-process--start "test" (s-join " " (append
                                               (list mach-process--command-current-file-tests)
                                               args
                                               (list target)))))))

;;;###autoload
(defun mach-process-current-directory-tests (args)
  "Run the mach test command for the current directory.
With the prefix argument, modify the command's invocation.
Mach: Run the tests."
  (interactive (list
                (mach-process-test-arguments)))
  (mach-process--start "test" (s-join " " (append
                                           (list mach-process--command-current-directory-tests)
                                           args
                                           (list default-directory)))))

;;;###autoload (autoload 'mach-process-test "mach-process" nil t)
(transient-define-prefix mach-process-test ()
  "Run the mach test command.
With the prefix argument, modify the command's invocation.
Mach: Run the tests."
  ["Arguments"
   ("-h" "Headless" "--headless")
   ]
  [["Actions"
    ("o" "Current file"      mach-process-current-file-tests)
    ("d" "Current directory" mach-process-current-directory-tests)
   ]])

(defun mach-process-test-arguments ()
  (transient-args 'mach-process-test))

;;;###autoload
(defun mach-process-check ()
  "Run the mach check command.
With the prefix argument, modify the command's invocation.
Mach: Check compile the current project.
Requires mach-check to be installed."
  (interactive)
  (mach-process--start "check" mach-process--command-check))

(defun mach--process-current-file-lint-compilation-finish-function (buffer desc)
  (when (and mach-process--originating-buffer
             (buffer-live-p mach-process--originating-buffer)
             (with-current-buffer buffer
               (and (derived-mode-p 'mach-process-mode)
                    (s-contains? (mapconcat #'identity
                                            (list
                                             (shell-quote-argument mach-process--custom-path-to-bin)
                                             mach-process--command-current-file-lint)
                                            " ")
                                 (car compilation-arguments)))))
    (with-current-buffer mach-process--originating-buffer
      (revert-buffer t t t))))

;;;###autoload
(defun mach-process-current-file-lint (args)
  "Run the mach lint command for the current file.
With the prefix argument, modify the command's invocation.
Mach: Lint."
  (interactive (list
                (mach-process-lint-arguments)))
  (add-hook
   'compilation-finish-functions
   'mach--process-current-file-lint-compilation-finish-function)

  (let* ((target (or (buffer-file-name) default-directory))
         (target (if (file-remote-p target)
                     (tramp-file-name-localname (tramp-dissect-file-name target))
                   target)))
    (mach-process--start "lint" (s-join " " (append
                                             (list mach-process--command-current-file-lint)
                                             args
                                             (list target))))))

;;;###autoload
(defun mach-process-outgoing-lint (args)
  "Run the mach lint command for all outgoing files.
With the prefix argument, modify the command's invocation.
Mach: Lint."
  (interactive (list
                (mach-process-lint-arguments)))
  (add-hook
   'compilation-finish-functions
   'mach--process-current-file-lint-compilation-finish-function)

  (mach-process--start "lint" (s-join " " (append
                                           (list mach-process--command-outgoing-lint)
                                           args))))

;;;###autoload (autoload 'mach-process-lint "mach-process" nil t)
(transient-define-prefix mach-process-lint ()
  "Run the mach lint command.
With the prefix argument, modify the command's invocation.
Mach: Lint."
  ["Arguments"
   ("-f" "Fix" "--fix")
   ("-W" "Warnings" "-W")
   ("-i" "No ignore list" "--no-ignore")
   ]
  [["Actions"
    ("f" "Current file"      mach-process-current-file-lint)
    ;; ("d" "Current directory" mach-process-current-directory-tests)
    ("o" "Outgoing"          mach-process-outgoing-lint)
   ]])

(defun mach-process-lint-arguments ()
  (transient-args 'mach-process-lint))

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
