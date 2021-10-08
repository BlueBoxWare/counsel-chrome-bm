;;; test-counsel-chrome-bm.el --- Tests for counsel-chrome-bm.el -*-lexical-binding:t-*-

;; Copyright (C) 2021  BlueBoxWare

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'seq)
(require 'counsel-chrome-bm)
(require 'buttercup)
(require 'mustache)
(eval-when-compile
  (require 'cl-lib))

;;;
;;; Helpers
;;;
(defconst counsel-chrome-bm--expected-bms
  '(("GNU Emacs - GNU Project" . "https://www.gnu.org/software/emacs/")
    ("GitHub: Where the world builds software · GitHub" . "https://github.com/")
    ("The Chromium Projects" . "https://www.chromium.org/")
    ("Wikipedia" . "https://www.wikipedia.org/")
    ("Google" . "https://www.google.com/")
    ("MELPA" . "https://melpa.org/#/")))

(defconst counsel-chrome-bm--expected-one-occurrence-quotes
  '("\"'#:/[]{}%$\";;'<>| - one occurrence of quotes"
    . "http://%22%27/#:/[]{}%$%22%22;;%60'%3C%3E|"))
(defconst counsel-chrome-bm--expected-multiple-occurrences-quotes
  '("\"'#:/[]{}%%$\"\"\"\";;'''<>| - multiple occurrences of quotes"
    . "http://%22%27/#:/[]{}%$%22%22;;%60'%3C%3E|"))
(defconst counsel-chrome-bm--expected-unicode
  '("イロハニホヘト חברה น่าฟังเอย - unicode" . "http://Γαζέες.⠙⠊⠌⠥⠗⠃░░▒▒▓▓██"))

(defconst counsel-chrome-bm--test-debug nil)

(defvar counsel-chrome-bm--test-expr)
(defvar counsel-chrome-bm--test-result)
(defvar counsel-chrome-bm--test-error)
(defvar counsel-chrome-bm--no-jq-old)

(defun counsel-chrome-bm--test-with (expr keys)
  "Evaluate EXPR followed by KEYS."
  (let ((counsel-chrome-bm--test-expr expr)
        (inhibit-message t)
        (buf (current-buffer)))
    (save-window-excursion
      (unwind-protect
          (execute-kbd-macro
           (vconcat (kbd "C-c C-c e")
                    (kbd keys)))
        (switch-to-buffer buf)))
    counsel-chrome-bm--test-result))

(cl-defun counsel-chrome-bm--test-with-file
    (file &optional &key ignore-folders keys all do-not-split)
  "Test with FILE and optionally use IGNORE-FOLDERS and provide KEYS.
When ALL, do no ignoring.  When DO-NOT-SPLIT, don't split the results."
  (let (
        (counsel-chrome-bm-file file)
        (counsel-chrome-bm-ignore-folder ignore-folders))
    (condition-case err
        (counsel-chrome-bm--test-with
         (if all '(counsel-chrome-bm-all) '(counsel-chrome-bm))
         (concat keys " C-g"))
      (quit nil)
      (t (progn
           (setq counsel-chrome-bm--test-error (error-message-string err))
           (signal (car err) (cdr err)))))
    (if (not counsel-chrome-bm-no-jq)
        (sleep-for 1))
    (if do-not-split
        ivy--old-cands
      (mapcar #'counsel-chrome-bm--split ivy--old-cands))))

(defun counsel-chrome-bm--test-with-content
    (content &optional ignore-folders keys all do-not-split)
  "Test with CONTENT and optionally use IGNORE-FOLDERS and provide KEYS.
When ALL, do no ignoring.  When DO-NOT-SPLIT, don't split the results."
  (let ((file
         (make-temp-file "bookmarks"
                         nil nil
                         (subst-char-in-string ?` ?\" content))))
    (counsel-chrome-bm--test-with-file file
                                       :ignore-folders ignore-folders
                                       :keys keys
                                       :all all
                                       :do-not-split do-not-split)))

(cl-defun counsel-chrome-bm--test-with-template
    (&optional &key flag ignore-folders keys all do-not-split)
  "Test with template and use FLAG to configure the template.
Optionally use IGNORE-FOLDERS and provide KEYS.
When ALL, do no ignoring.  When DO-NOT-SPLIT, don't split the results."
  (let ((template (with-temp-buffer
                    (insert-file-contents "test/template")
                    (buffer-string)))
        (available-flags
         '("no-folder-names"
           "no-url-names"
           "no-types"
           "no-urls"
           "folder-without-children"))
        (ht (make-hash-table :test #'equal)))
    (mapc (lambda (key) (puthash key nil ht)) available-flags)
    (if flag
        (progn
          (if (not (member flag available-flags))
              (error "Invalid flag: %s" flag))
          (puthash flag t ht)))
    (let ((content (mustache-render template ht)))
      (if counsel-chrome-bm--test-debug (message "%s" content))
      (counsel-chrome-bm--test-with-content content
                                            ignore-folders
                                            keys
                                            all
                                            do-not-split))))

(defun counsel-chrome-bm--test-action (keys action)
  "Test ACTION after inputting KEYS."
  (let ((result
         (counsel-chrome-bm--test-with-template :keys keys :do-not-split t))
        (counsel-chrome-bm-default-action action))
    (counsel-chrome-bm--dispatch (car result))))

;;;
;;; Matchers
;;;
(buttercup-define-matcher-for-binary-function :to-contain-all
    (lambda (given expected)
      (seq-every-p (lambda (e)
                     (if (and counsel-chrome-bm--test-debug
                              (not (member e given)))
                         (message "Missing %s." e))
                     (member e given)) expected))
  :expect-match-phrase
  "Expected `%A' to be a list containing all of `%b', but instead it was `%a`."
  :expect-mismatch-phrase
  "Expected `%A' to be a list not containing all of `%b', but instead it was `%a'.")

(buttercup-define-matcher-for-binary-function :to-contain-none
    (lambda (given not-expected)
      (seq-every-p (lambda (e)
                     (if (and counsel-chrome-bm--test-debug
                              (member e given))
                         (message "Found %s." e))
                     (not  (member e given))) not-expected))
  :expect-match-phrase
  "Expected `%A' to be a list containing none of `%b', but instead it was `%a'."
  :expect-mismatch-phrase
  "Expected `%A' to be a list containing some of `%b', but instead it was `%a'.")

;;;
;;; Tests
;;;
(defun counsel-chrome-bm--run-general-tests (no-jq)
  "Create test suites to use both with jq and without jq, depending on NO-JQ."
  (before-all
    (setq counsel-chrome-bm--no-jq-old counsel-chrome-bm-no-jq)
    (setq counsel-chrome-bm-no-jq no-jq))

  (after-all
    (setq counsel-chrome-bm-no-jq counsel-chrome-bm--no-jq-old))

  (describe "Malformed files"
    (it "Empty file"
      (if no-jq
          (progn (expect (counsel-chrome-bm--test-with-content "")
                         :to-throw)
                 (expect counsel-chrome-bm--test-error
                         :to-match "Could not parse.*End of file.*JSON"))
        (expect (counsel-chrome-bm--test-with-content "")
                :to-equal nil)))

    (it "Malformed file"
      (if no-jq
          (progn
            (expect (counsel-chrome-bm--test-with-content "{") :to-throw)
            (expect counsel-chrome-bm--test-error
                    :to-match "Could not parse.*End of buffer"))
        (let ((result (counsel-chrome-bm--test-with-content "{")))
          (expect (length result) :to-equal 1)
          (expect (caar result) :to-match "Could not parse.*bookmark file")
          (expect (cdar result) :to-equal ""))))

    (it "Empty object"
      (let ((result (counsel-chrome-bm--test-with-content "{}")))
        (if no-jq
            (expect (length result) :to-equal 0)
          (expect (length result) :to-equal 1)
          (expect (caar result) :to-match "Could not parse.*bookmark file")
          (expect (cdar result) :to-equal ""))))

    (it "Empty roots"
      (expect (counsel-chrome-bm--test-with-content "{ `roots`: {} }")
              :to-equal nil))

    (it "Missing folder names"
      (expect (counsel-chrome-bm--test-with-template :flag "no-folder-names")
              :to-contain-all counsel-chrome-bm--expected-bms))

    (it "Missing types"
      (expect (counsel-chrome-bm--test-with-template :flag "no-types")
              :to-contain-all counsel-chrome-bm--expected-bms))

    (it "Missing url names"
      (let ((should-include `(("GNU Emacs - GNU Project"
                               . "https://www.gnu.org/software/emacs/")
                              ("GitHub: Where the world builds software · GitHub"
                               . "https://github.com/")
                              ("" . "https://www.chromium.org/")
                              ("Wikipedia" . "https://www.wikipedia.org/")
                              ("" . "https://www.google.com/")
                              ("MELPA" . "https://melpa.org/#/"))))
        (expect (counsel-chrome-bm--test-with-template :flag "no-url-names")
                :to-contain-all should-include)))

    (it "Missing urls"
      (let ((should-include '(("GNU Emacs - GNU Project"
                               . "https://www.gnu.org/software/emacs/")
                              ("GitHub: Where the world builds software · GitHub"
                               . "")
                              ("The Chromium Projects"
                               . "https://www.chromium.org/")
                              ("Wikipedia" . "https://www.wikipedia.org/")
                              ("Google" . "https://www.google.com/")
                              ("MELPA" . ""))))
        (expect (counsel-chrome-bm--test-with-template :flag "no-urls")
                :to-contain-all should-include)))


    (it "Folder without children"
      (let ((should-include '(("GNU Emacs - GNU Project"
                               . "https://www.gnu.org/software/emacs/")
                              ("The Chromium Projects"
                               . "https://www.chromium.org/")
                              ("Wikipedia" . "https://www.wikipedia.org/")
                              ("Google" . "https://www.google.com/")
                              ("MELPA" . "https://melpa.org/#/"))))
        (expect (counsel-chrome-bm--test-with-template
                 :flag "folder-without-children")
                :to-contain-all should-include))))

  (describe "Searching"
    (it "Searching for 'g'"
      (let ((should-include '(("GNU Emacs - GNU Project"
                               . "https://www.gnu.org/software/emacs/")
                              ("GitHub: Where the world builds software · GitHub"
                               . "https://github.com/")
                              ("The Chromium Projects"
                               . "https://www.chromium.org/")
                              ("Wikipedia" . "https://www.wikipedia.org/")
                              ("Google" . "https://www.google.com/")
                              ("MELPA" . "https://melpa.org/#/"))))
        (expect (counsel-chrome-bm--test-with-template :keys "g")
                :to-contain-all should-include)))

    (it "Searching for 'org'"
      (let ((should-include '(("GNU Emacs - GNU Project"
                               . "https://www.gnu.org/software/emacs/")
                              ("MELPA" . "https://melpa.org/#/")
                              ("The Chromium Projects"
                               . "https://www.chromium.org/")
                              ("Wikipedia" . "https://www.wikipedia.org/")
                              ("Wikipedia" . "https://www.wikipedia.org/"))))
        (expect (counsel-chrome-bm--test-with-template :keys "org")
                :to-contain-all should-include))))

  (describe "Special characters"

    (it "Searching for double quote"
      (let ((result (counsel-chrome-bm--test-with-template :keys "\"")))
        (expect result
                :to-contain-all
                (list
                 counsel-chrome-bm--expected-one-occurrence-quotes
                 counsel-chrome-bm--expected-multiple-occurrences-quotes))
        (expect result :to-contain-none counsel-chrome-bm--expected-bms)))

    (it "Searching for 2 double quotes"
      (let ((result (counsel-chrome-bm--test-with-template :keys "\"\"")))
        (expect result
                :not :to-contain
                counsel-chrome-bm--expected-one-occurrence-quotes)
        (expect result
                :to-contain
                counsel-chrome-bm--expected-multiple-occurrences-quotes)))


    (it "Searching for single quote"
      (let ((result (counsel-chrome-bm--test-with-template :keys "'")))
        (expect result
                :to-contain-all
                (list
                 counsel-chrome-bm--expected-one-occurrence-quotes
                 counsel-chrome-bm--expected-multiple-occurrences-quotes) )
        (expect result :to-contain-none counsel-chrome-bm--expected-bms)))

    (it "Searching for 2 single quotes"
      (let ((result (counsel-chrome-bm--test-with-template :keys "''")))
        (expect result
                :not :to-contain
                counsel-chrome-bm--expected-one-occurrence-quotes)
        (expect result
                :to-contain
                counsel-chrome-bm--expected-multiple-occurrences-quotes)))

    (if (not no-jq)
        (it "Searching for $"
          (let ((result (counsel-chrome-bm--test-with-template :keys "$")))
            (expect result
                    :to-contain
                    counsel-chrome-bm--expected-one-occurrence-quotes)
            (expect result
                    :to-contain-none counsel-chrome-bm--expected-bms))))

    (it "Searching for single %"
      (let ((result (counsel-chrome-bm--test-with-template :keys "%")))
        (expect result
                :to-contain-all
                (list
                 counsel-chrome-bm--expected-one-occurrence-quotes
                 counsel-chrome-bm--expected-multiple-occurrences-quotes) )
        (expect result :to-contain-none counsel-chrome-bm--expected-bms)))

    (it "Searching for 2 %'s"
      (let ((result (counsel-chrome-bm--test-with-template :keys "%%")))
        (expect result
                :not :to-contain
                counsel-chrome-bm--expected-one-occurrence-quotes)
        (expect result
                :to-contain
                counsel-chrome-bm--expected-multiple-occurrences-quotes))))

  (describe "Unicode"
    (it "Finding"
      (expect (counsel-chrome-bm--test-with-template)
              :to-contain counsel-chrome-bm--expected-unicode))

    (it "Searching"
      (let ((result (counsel-chrome-bm--test-with-template :keys "น่าฟังเอย")))
        (expect result :to-contain counsel-chrome-bm--expected-unicode)
        (expect result :to-contain-none counsel-chrome-bm--expected-bms)))

    (it "Excluding"
      (let ((result
             (counsel-chrome-bm--test-with-template
              :ignore-folders '("░⠗⠃イ"))))
        (expect result :to-contain-all counsel-chrome-bm--expected-bms)
        (expect result :not :to-contain counsel-chrome-bm--expected-unicode))))


  (describe "Excluding"
    (it "Excluding root key"
      (let ((counsel-chrome-bm-ignore-key '("synced")))
        (let ( (result (counsel-chrome-bm--test-with-template)))
          (expect result
                  :to-contain-all counsel-chrome-bm--expected-bms)
          (expect result
                  :not :to-contain
                  counsel-chrome-bm--expected-one-occurrence-quotes)
          (expect result
                  :not :to-contain
                  counsel-chrome-bm--expected-multiple-occurrences-quotes))
        (let ((result (counsel-chrome-bm--test-with-template :all t)))
          (expect result
                  :to-contain counsel-chrome-bm--expected-one-occurrence-quotes)
          (expect result
                  :to-contain
                  counsel-chrome-bm--expected-multiple-occurrences-quotes))))

    (it "Excluding toplevel folder"
      (let ((result
             (counsel-chrome-bm--test-with-template
              :ignore-folders '("Bookmarks bar" "Mobile bookmarks"))))
        (expect result
                :not :to-contain
                counsel-chrome-bm--expected-one-occurrence-quotes)
        (expect result
                :not :to-contain
                '("GNU Emacs - GNU Project"
                  . "https://www.gnu.org/software/emacs/"))
        (expect result
                :to-contain
                '("GitHub: Where the world builds software · GitHub"
                  . "https://github.com/"))
        (expect result
                :to-contain
                '("The Chromium Projects" . "https://www.chromium.org/")))
      (let ((result (counsel-chrome-bm--test-with-template
                     :ignore-folders '("Bookmarks bar" "Mobile bookmarks")
                     :all t)))
        (expect result
                :to-contain counsel-chrome-bm--expected-one-occurrence-quotes)
        (expect result
                :to-contain
                '("GNU Emacs - GNU Project"
                  . "https://www.gnu.org/software/emacs/"))
        (expect result
                :to-contain
                '("GitHub: Where the world builds software · GitHub"
                  . "https://github.com/"))
        (expect result
                :to-contain
                '("The Chromium Projects" . "https://www.chromium.org/"))))

    (it "Excluding 'normal' folder"
      (let ((result (counsel-chrome-bm--test-with-template
                     :ignore-folders '("folder2"))))
        (expect result
                :to-contain counsel-chrome-bm--expected-one-occurrence-quotes)
        (expect result
                :to-contain
                '("GNU Emacs - GNU Project"
                  . "https://www.gnu.org/software/emacs/"))
        (expect result
                :to-contain
                '("GitHub: Where the world builds software · GitHub"
                  . "https://github.com/"))
        (expect result
                :not :to-contain
                '("The Chromium Projects"
                  . "https://www.chromium.org/"))
        (expect result
                :not :to-contain '("Google" . "https://www.google.com/"))
        (expect result
                :not :to-contain '("Wikipedia" . "https://www.wikipedia.org/")))
      (let ((result (counsel-chrome-bm--test-with-template
                     :ignore-folders '("folder2")
                     :all t)))
        (expect result
                :to-contain '("The Chromium Projects"
                              . "https://www.chromium.org/"))
        (expect result
                :to-contain '("Google" . "https://www.google.com/"))
        (expect result
                :to-contain '("Wikipedia" . "https://www.wikipedia.org/"))))

    (it "Excluding subfolder"
      (let ((result (counsel-chrome-bm--test-with-template
                     :ignore-folders '("subfolder1"))))
        (expect result
                :to-contain counsel-chrome-bm--expected-one-occurrence-quotes)
        (expect result
                :to-contain
                '("GNU Emacs - GNU Project"
                  . "https://www.gnu.org/software/emacs/"))
        (expect result
                :to-contain
                '("GitHub: Where the world builds software · GitHub"
                  . "https://github.com/"))
        (expect result
                :not :to-contain
                '("The Chromium Projects" . "https://www.chromium.org/"))
        (expect result
                :to-contain '("Google" . "https://www.google.com/"))
        (expect result
                :to-contain '("Wikipedia" . "https://www.wikipedia.org/"))))

    (it "Special chars in exclude lists"
      (let ((counsel-chrome-bm-ignore-key '("'\"$%<>/")))
        (expect (counsel-chrome-bm--test-with-template
                 :ignore-folders '("'\"$%<>/"))
                :not :to-throw))))

  (describe "Actions"

    (it "Copy title"
      (counsel-chrome-bm--test-action "%%" #'counsel-chrome-bm--copy-title)
      (expect (car kill-ring)
              :to-equal
              "\"'#:/[]{}%%$\"\"\"\";;'''<>| - multiple occurrences of quotes"))

    (it "Copy URL"
      (counsel-chrome-bm--test-action "%%" #'counsel-chrome-bm--copy-url)
      (expect (car kill-ring)
              :to-equal "http://%22%27/#:/[]{}%$%22%22;;%60'%3C%3E|"))))

(describe "Tests"
  (before-all
    (setf (symbol-function 'counsel-chrome-bm--eval-expr)
          (lambda ()
            (interactive)
            (setq counsel-chrome-bm--test-result
                  (eval counsel-chrome-bm--test-expr))))
    (global-set-key (kbd "C-c C-c e")
                    'counsel-chrome-bm--eval-expr))

  (after-each
    (setq counsel-chrome-bm--test-expr nil
          counsel-chrome-bm--test-result nil
          counsel-chrome-bm--test-error nil))

  (if (not (file-executable-p (counsel-chrome-bm--jq)))
      (error "Jq is required"))

  (describe "Miscellaneous"

    (it "No file defined"
      (expect (counsel-chrome-bm--test-with-file nil)
              :to-throw)
      (expect counsel-chrome-bm--test-error
              :to-match "counsel-chrome-bm-file is not set"))

    (it "Invalid jq executable"
      (let ((counsel-chrome-bm-jq "foo"))
        (expect (counsel-chrome-bm--test-with-content "")
                :to-throw)
        (expect counsel-chrome-bm--test-error
                :to-match "Can not execute.*/foo")))

    (it "Initializing"
      (spy-on #'counsel-chrome-bm-initialize :and-call-through)
      (setq counsel-chrome-bm--initialized nil)
      (counsel-chrome-bm--test-with-template)
      (expect #'counsel-chrome-bm-initialize :to-have-been-called-times 1)
      (counsel-chrome-bm--test-with-template)
      (expect #'counsel-chrome-bm-initialize :to-have-been-called-times 1))

    (it "Falling back to native JSON"
      (spy-on #'message)
      (let ((exec-path nil))
        (counsel-chrome-bm--test-with-template)
        (expect #'message
                :to-have-been-called-with
                "counsel-chrome-bm: Could not find jq. Falling back to native JSON parsing.")
        (expect #'message :to-have-been-called-times 1)
        (counsel-chrome-bm--test-with-template)
        (expect #'message :to-have-been-called-times 1)))

    (it "counsel-chrome-bm-no-jq set"
      (spy-on #'message)
      (spy-on #'counsel-chrome-bm--read-native)
      (let ((counsel-chrome-bm-no-jq t))
        (counsel-chrome-bm--test-with-template)
        (expect #'counsel-chrome-bm--read-native :to-have-been-called)
        (expect #'message :not :to-have-been-called)))

    (it "Prefix and suffix set to nil"
      (let ((counsel-chrome-bm-url-prefix nil)
            (counsel-chrome-bm-url-suffix nil))
        (expect (counsel-chrome-bm--transformer "") :not :to-throw))))

  (describe "With jq"
    (counsel-chrome-bm--run-general-tests nil))

  (describe "Without jq"
    (counsel-chrome-bm--run-general-tests t)))

(provide 'test-counsel-chrome-bm)

;;; test-counsel-chrome-bm.el ends here
