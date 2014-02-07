;;; projectile-drupal.el --- Minor mode for Drupal projects based on projectile-mode

;; Copyright (C) 2014 Damon Haley

;; Author:            Damon Haley <dkh@member.fsf.org>
;; URL:               https://github.com/dhaley/projectile-drupal
;; Version:           0.1.1
;; Keywords:          drupal, projectile
;; Package-Requires:  ((projectile "1.0.0-cvs") (inflections "1.1") (f "0.13.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; To make it start alongside projectile-mode:
;;
;;    (add-hook 'projectile-mode-hook 'projectile-drupal-on)
;;
;;; Code:

(require 'projectile)
(require 'inflections)
(require 'f)

(defgroup projectile-drupal nil
  "Drupal mode based on projectile"
  :prefix "projectile-drupal-"
  :group 'projectile)

(defcustom projectile-drupal-errors-re
  "\\([0-9A-Za-z@_./\:-]+\\.rb\\):?\\([0-9]+\\)?"
  "The regex used to find errors with file paths."
  :group 'projectile-drupal
  :type 'string)

(defcustom projectile-drupal-generate-filepath-re
  "^\\s-+\\(?:create\\|exists\\|conflict\\|skip\\)\\s-+\\(.+\\)$"
  "The regex used to find file paths in `projectile-drupal-generate-mode'."
  :group 'projectile-drupal
  :type 'string)

(defcustom projectile-drupal-javascript-dirs
  '("app/assets/javascripts/" "lib/assets/javascripts/" "public/javascripts/")
  "The list directories to for javascript files in."
  :group 'projectile-drupal
  :type '(repeat stirng))

(defcustom projectile-drupal-expand-snippet t
  "If not nil newly created buffers will be pre-filled with class skeleton.")

(defcustom projectile-drupal-add-keywords t
  "If not nil the drupal keywords will be font locked in the mode's bufffers.")

(defcustom projectile-drupal-keymap-prefix (kbd "C-8")
  "`projectile-drupal-mode' keymap prefix."
  :group 'projectile-drupal
  :type 'string)

(defmacro projectile-drupal-with-preloader (&rest cases)
  `(cond ((projectile-drupal-spring-p)
          ,(plist-get cases :spring))
         ((projectile-drupal-zeus-p)
          ,(plist-get cases :zeus))
         (t
          ,(plist-get cases :vanilla))))

(defmacro projectile-drupal-with-root (body-form)
  `(let ((default-directory (projectile-drupal-root)))
     ,body-form))

(defmacro projectile-drupal-find-current-resource (dir re)
  "RE will be the argument to `s-lex-format'.

The binded variables are \"singular\" and \"plural\"."
  `(let* ((singular (projectile-drupal-current-resource-name))
          (plural (pluralize-string singular))
          (files (--filter
                  (string-match-p (s-lex-format ,re) it)
                  (projectile-dir-files (projectile-expand-root ,dir)))))
     (projectile-drupal-goto-file
       (if (= (length files) 1)
           (-first-item files)
         (projectile-completing-read "Which exactly: " files)))))

(defun projectile-drupal-spring-p ()
  (file-exists-p (f-canonical
                  (concat
                   temporary-file-directory
                   "spring/"
                   (md5 (projectile-project-root) 0 -1)
                   ".pid")))
  )

(defun projectile-drupal-zeus-p ()
  (file-exists-p (projectile-expand-root ".zeus.sock")))


(defun projectile-drupal-choices (dirs)
  "Uses `projectile-dir-files' function to find files in directories.

The DIRS is list of lists consisting of a directory path and regexp to filter files from that directory.
Returns a hash table with keys being short names and values being relative paths to the files."
  (let ((hash (make-hash-table :test 'equal)))
    (loop for (dir re) in dirs do
          (loop for file in (projectile-dir-files (projectile-expand-root dir)) do
                (when (string-match re file)
                  (puthash (match-string 1 file) file hash))))
    hash))

(defun projectile-drupal-hash-keys (hash)
  (let (keys)
    (maphash (lambda (key value) (setq keys (cons key keys))) hash)
    keys))

(defun projectile-drupal-find-resource (prompt dirs)
  (let ((choices (projectile-drupal-choices dirs)))
     (projectile-drupal-goto-file
      (gethash (projectile-completing-read prompt (projectile-drupal-hash-keys choices)) choices))))

(defun projectile-drupal-find-model ()
  (interactive)
  (projectile-drupal-find-resource "model: " '(("app/models/" "/models/\\(.+\\)\\.rb$"))))

(defun projectile-drupal-find-controller ()
  (interactive)
  (projectile-drupal-find-resource "controller: " '(("app/controllers/" "/controllers/\\(.+\\)_controller\\.rb$"))))

(defun projectile-drupal-find-view ()
  (interactive)
  (projectile-drupal-find-resource
   "view: "
   `(("app/views/" ,(concat "app/views/\\(.+\\)" projectile-drupal-views-re)))))

(defun projectile-drupal-find-layout ()
  (interactive)
  (projectile-drupal-find-resource
   "layout: "
   `(("app/views/layouts/" ,(concat "app/views/layouts/\\(.+\\)" projectile-drupal-views-re)))))

(defun projectile-drupal-find-helper ()
  (interactive)
  (projectile-drupal-find-resource "helper: " '(("app/helpers/" "/helpers/\\(.+\\)_helper\\.rb$"))))

(defun projectile-drupal-find-lib ()
  (interactive)
  (projectile-drupal-find-resource "lib: " '(("lib/" "lib/\\(.+\\)\\.rb$"))))

(defun projectile-drupal-find-spec ()
  (interactive)
  (projectile-drupal-find-resource "spec: " '(("spec/" "spec/\\(.+\\)_spec\\.rb$"))))

(defun projectile-drupal-find-migration ()
  (interactive)
  (projectile-drupal-find-resource "migration: " '(("db/migrate/" "db/migrate/\\(.+\\)\\.rb$"))))

(defun projectile-drupal-find-javascript ()
  (interactive)
  (projectile-drupal-find-resource
   "stylesheet: "
   (--map (list it "/\\(.+\\)\\.[^.]+$") projectile-drupal-javascript-dirs)))

(defun projectile-drupal-find-initializer ()
  (interactive)
  (projectile-drupal-find-resource "initializer: " '(("config/initializers/" "config/initializers/\\(.+\\)\\.rb$"))))

(defun projectile-drupal-find-environment ()
  (interactive)
  (projectile-drupal-find-resource
   "environment: "
   '(("config/" "/\\(application\\|environment\\)\\.rb$")
     ("config/environments/" "/\\([^/]+\\)\\.rb$"))))

(defun projectile-drupal-find-locale ()
  (interactive)
  (projectile-drupal-find-resource "locale: " '(("config/locales/" "config/locales/\\(.+\\)\\.\\(?:rb\\|yml\\)$"))))

(defun projectile-drupal-find-mailer ()
  (interactive)
  (projectile-drupal-find-resource "mailer: " '(("app/mailers/" "app/mailers/\\(.+\\)\\.rb$"))))

(defun projectile-drupal-find-current-model ()
  (interactive)
  (projectile-drupal-find-current-resource
   "app/models/" "/${singular}\\.rb$"))

(defun projectile-drupal-find-current-controller ()
  (interactive)
  (projectile-drupal-find-current-resource
   "app/controllers/" "/${plural}_controller\\.rb$"))

(defun projectile-drupal-find-current-view ()
  (interactive)
  (projectile-drupal-find-current-resource
   "app/views/" "/${plural}/.+$"))

(defun projectile-drupal-find-current-helper ()
  (interactive)
  (projectile-drupal-find-current-resource
   "app/helpers/" "/${plural}_helper\\.rb$"))

(defun projectile-drupal-find-current-spec ()
  (interactive)
  (if (fboundp 'rspec-toggle-spec-and-target)
      (rspec-toggle-spec-and-target)
    (projectile-find-test-file)))

(defun projectile-drupal-find-current-migration ()
  (interactive)
  (projectile-drupal-find-current-resource
   "db/migrate/" "/[0-9]\\{14\\}.*_\\(${plural}\\|${singular}\\).*\\.rb$"))

(defun projectile-drupal-current-resource-name ()
  "Returns a resource name extracted from the name of the currently visiting file"
  (let ((file-name (buffer-file-name)))
    (if file-name
        (singularize-string
         (loop for re in '("app/models/\\(.+\\)\\.rb$"
                           "app/controllers/\\(.+\\)_controller\\.rb$"
                           "app/views/\\(.+\\)/[^/]+$"
                           "app/helpers/\\(.+\\)_helper\\.rb$"
                           "spec/.*/\\([a-z_]+?\\)\\(_controller\\)?_spec\\.rb$")
               until (string-match re file-name)
               finally return (match-string 1 file-name))))
    )
  )

(defun projectile-drupal-list-entries (fun dir)
  (--map
   (substring it (length (concat (projectile-drupal-root) dir)))
   (funcall fun (projectile-expand-root dir))))

(defun projectile-drupal-find-log ()
  (interactive)
  ;;logs tend to not be under scm so do not resort to projectile-dir-files
  (find-file (projectile-expand-root
              (concat
               "log/"
               (projectile-completing-read
                "log: "
                (projectile-drupal-list-entries 'f-files "log/")))))
  (auto-revert-tail-mode +1)
  (setq-local auto-revert-verbose nil)
  (buffer-disable-undo)
  (projectile-drupal-on))

(defun projectile-drupal-root ()
  "Returns drupal root directory if this file is a part of a Drupal application else nil"
  (and
   (projectile-project-p)
   (file-exists-p (projectile-expand-root "config/environment.rb"))
   (projectile-project-root)))

;; (defun projectile-drupal-console ()
;;   (interactive)
;;   (projectile-drupal-with-root
;;    (with-current-buffer (run-ruby
;;                          (projectile-drupal-with-preloader
;;                           :spring "spring drupal console"
;;                           :zeus "zeus console"
;;                           :vanilla "bundle exec drupal console"))
;;      (projectile-drupal-mode +1))))

(defun projectile-drupal-expand-snippet-maybe ()
  (when (and (fboundp 'yas-expand-snippet)
             (and (buffer-file-name) (not (file-exists-p (buffer-file-name))))
             (s-blank? (buffer-string))
        (projectile-drupal-expand-corresponding-snippet))))

(defun projectile-drupal-expand-corresponding-snippet ()
  (let ((name (buffer-file-name)))
    (yas-expand-snippet
     (cond ((string-match "app/controllers/\\(.+\\)\\.rb$" name)
            (format
             "class %s < ${1:ApplicationController}\n$2\nend"
             (s-join "::" (projectile-drupal-classify (match-string 1 name)))))
           ((string-match "spec/[^/]+/\\(.+\\)_spec\\.rb$" name)
            (format
             "require \"spec_helper\"\n\ndescribe %s do\n$1\nend"
             (s-join "::" (projectile-drupal-classify (match-string 1 name)))))
           ((string-match "app/models/\\(.+\\)\\.rb$" name)
            (format
             "class %s < ${1:ActiveRecord::Base}\n$2\nend"
             (s-join "::" (projectile-drupal-classify (match-string 1 name)))))
           ((string-match "lib/\\(.+\\)\\.rb$" name)
            (let ((parts (projectile-drupal-classify (match-string 1 name))))
              (format
               (concat
                (s-join
                 ""
                 (--map (s-lex-format "module ${it}\n") (butlast parts)))
                "${1:module} %s\n$2\nend"
                (s-join "" (make-list (1- (length parts)) "\nend")))
               (-last-item parts))))))))

(defun projectile-drupal-classify (name)
  "Accepts a filepath, splits it by '/' character and classifieses each of the element"
  (--map (replace-regexp-in-string "_" "" (upcase-initials it)) (split-string name "/")))

(defun projectile-drupal-declassify (name)
  "Converts passed string to a relative filepath."
  (let ((case-fold-search nil))
    (downcase
     (replace-regexp-in-string
      "::" "/"
      (replace-regexp-in-string
       " " "_"
       (replace-regexp-in-string
        "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" name))))))

(defun projectile-drupal-generate ()
  "Runs drupal generate command"
  (interactive)
  (projectile-drupal-with-root
   (let ((command-prefix (projectile-drupal-with-preloader
                          :spring "spring drupal generate "
                          :zeus "zeus generate "
                          :vanilla "bundle exec drupal generate ")))
     (compile
      (concat command-prefix (read-string command-prefix))
      'projectile-drupal-generate-mode))))

(defun projectile-drupal-sanitize-and-goto-file (dir name &optional ext)
  "Calls `projectile-drupal-goto-file' with passed arguments sanitizing them before."
  (projectile-drupal-goto-file
   (concat
    (projectile-drupal-sanitize-dir-name dir) (projectile-drupal-declassify name) ext)))

(defun projectile-drupal-goto-file (filepath)
  "Finds the FILEPATH after expanding root."
  (projectile-drupal-ff (projectile-expand-root filepath)))

(defun projectile-drupal-goto-gem (gem)
  "Uses `bundle-open' to open GEM. If the function is not defined notifies user."
  (if (not (fboundp 'bundle-open))
      (user-error "Please install budler.el from https://github.com/tobiassvn/bundler.el")
    (message "Using bundle-open command to open the gem")
    (bundle-open (car (s-split "/" gem))))
  )

(defun projectile-drupal-goto-javascript-at-point ()
  (let ((name
         (projectile-drupal-sanitize-name (thing-at-point 'filename))))
    (projectile-drupal-ff
     (loop for dir in projectile-drupal-javascript-dirs
           for re = (s-lex-format "${dir}${name}\\..+$")
           for files = (projectile-dir-files (projectile-expand-root dir))
           for file = (--first (string-match-p re it) files)
           until file
           finally return (and file (projectile-expand-root file)))))
  )

(defun projectile-drupal-goto-file-at-point ()
  "Tries to find file at point"
  (interactive)
  (let ((name (projectile-drupal-name-at-point))
        (line (projectile-drupal-current-line))
        (case-fold-search nil))
    (cond ((string-match "Processing by \\(.+\\)#\\(?:[^ ]+\\)" line)
           (projectile-drupal-sanitize-and-goto-file
            "app/controllers/" (match-string 1 line) ".rb"))

          ((string-match "Rendered \\([^ ]+\\)" line)
           (projectile-drupal-sanitize-and-goto-file
            "app/views/" (match-string 1 line)))

          ((string-match-p "\\_<render\\_>" line)
           (projectile-drupal-goto-template-at-point))

          ((string-match-p "^//= require .+$" line)
           (projectile-drupal-goto-javascript-at-point))

          ((string-match-p "\\_<require_relative\\_>" line)
           (projectile-drupal-ff (expand-file-name (concat (thing-at-point 'filename) ".rb"))))

          ((string-match-p "\\_<require\\_>" line)
           (projectile-drupal-goto-gem (thing-at-point 'filename)))

          ((not (string-match-p "^[A-Z]" name))
           (projectile-drupal-sanitize-and-goto-file "app/models/" (singularize-string name) ".rb"))

          ((not (string-match-p "^[A-Z]" name))
           (projectile-drupal-sanitize-and-goto-file "app/models/" (singularize-string name) ".rb"))

          ((string-match-p "^[A-Z]" name)
           (loop for dir in (-concat
                             (--map
                              (concat "app/" it)
                              (projectile-drupal-list-entries 'f-directories "app/"))
                             '("lib/"))
                 until (projectile-drupal-sanitize-and-goto-file dir name ".rb"))))
    )
  )

(defun projectile-drupal-template-name (template)
  (-first-item (s-split "\\." (-last-item (s-split "/" template)))))

(defun projectile-drupal-template-format (template)
  (let ((at-point-re "\\.\\([^.]+\\)\\.[^.]+$")
        (at-line-re "formats\\(?:'\"\\|:\\)?\\s-*\\(?:=>\\)?\\s-*\\[[:'\"]\\([a-zA-Z0-9]+\\)['\"]?\\]"))
    (cond ((string-match at-point-re template)
           (match-string 1 template))
          ((string-match at-line-re (projectile-drupal-current-line))
           (match-string 1 (projectile-drupal-current-line)))
          (t
           (when (string-match at-point-re (buffer-file-name))
             (match-string 1 (buffer-file-name)))))))

(defun projectile-drupal-template-dir (template)
  (projectile-drupal-sanitize-dir-name
   (cond ((string-match "\\(.+\\)/[^/]+$" template)
          (projectile-expand-root
           (concat "app/views/" (match-string 1 template))))
         ((string-match "app/controllers/\\(.+\\)_controller\\.rb$" (buffer-file-name))
          (projectile-expand-root
           (concat "app/views/" (match-string 1 (buffer-file-name)))))
         (t
          default-directory))))

(defun projectile-drupal-goto-template-at-point ()
  (interactive)
  (let* ((template (projectile-drupal-filename-at-point))
         (dir (projectile-drupal-template-dir template))
         (name (projectile-drupal-template-name template))
         (format (projectile-drupal-template-format template)))
    (if format
        (loop for processor in '("erb" "haml" "slim")
              for template = (s-lex-format "${dir}${name}.${format}.${processor}")
              for partial = (s-lex-format "${dir}_${name}.${format}.${processor}")
              until (or
                     (projectile-drupal-ff template)
                     (projectile-drupal-ff partial)))
      (message "Could not recognize the template's format")
      (dired dir))))

(defun projectile-drupal-goto-gemfile ()
  (interactive)
  (projectile-drupal-goto-file "Gemfile"))

(defun projectile-drupal-goto-schema ()
  (interactive)
  (projectile-drupal-goto-file "db/schema.rb"))

(defun projectile-drupal-goto-routes ()
  (interactive)
  (projectile-drupal-goto-file "config/routes.rb"))

(defun projectile-drupal-goto-spec-helper ()
  (interactive)
  (projectile-drupal-goto-file "spec/spec_helper.rb"))

(defun projectile-drupal-ff (path &optional ask)
  "Calls `find-file' function on PATH when it is not nil and the file exists.

If file does not exist and ASK in not nil it will ask user to proceed."
  (if (or (and path (file-exists-p path))
          (and ask (yes-or-no-p (s-lex-format "File does not exists. Create a new buffer ${path} ?"))))
      (find-file path)))

(defun projectile-drupal-name-at-point ()
  (projectile-drupal-sanitize-name (symbol-name (symbol-at-point))))

(defun projectile-drupal-filename-at-point ()
  (projectile-drupal-sanitize-name (thing-at-point 'filename)))

(defun projectile-drupal-apply-ansi-color ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

(defun projectile-drupal-make-buttons (buffer exit-code)
  (with-current-buffer buffer
    (goto-char 0)
    (while (re-search-forward projectile-drupal-generate-filepath-re (max-char) t)
      (let ((beg (match-beginning 1))
            (end (match-end 1)))
        (when (file-exists-p (projectile-expand-root (buffer-substring-no-properties beg end)))
          (make-button beg end 'action 'projectile-drupal-generate-ff 'follow-link t))))
    )
  )

(defun projectile-drupal-generate-ff (button)
  (find-file (projectile-expand-root (button-label button))))

(defun projectile-drupal-sanitize-name (name)
  (when (or
         (and (s-starts-with? "'" name) (s-ends-with? "'" name))
         (and (s-starts-with? "\"" name) (s-ends-with? "\"" name)))
    (setq name (substring name 1 -1)))
  (when (s-starts-with? "./" name)
    (setq name (substring name 2)))
  (when (or (s-starts-with? ":" name) (s-starts-with? "/" name))
    (setq name (substring name 1)))
  (when (s-ends-with? "," name)
    (setq name (substring name 0 -1)))
  name)

(defun projectile-drupal-sanitize-dir-name (name)
  (if (s-ends-with? "/" name) name (concat name "/")))

(defun projectile-drupal-current-line ()
  (save-excursion
    (let (beg)
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (buffer-substring-no-properties beg (point)))))

(defun projectile-drupal-set-javascript-dirs ()
  (set
   (make-local-variable 'projectile-drupal-javascript-dirs)
   (--filter
    (file-exists-p (projectile-expand-root it))
    projectile-drupal-javascript-dirs)))

(defvar projectile-drupal-mode-goto-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") 'projectile-drupal-find-current-model)
    (define-key map (kbd "c") 'projectile-drupal-find-current-controller)
    (define-key map (kbd "v") 'projectile-drupal-find-current-view)
    (define-key map (kbd "h") 'projectile-drupal-find-current-helper)
    (define-key map (kbd "s") 'projectile-drupal-find-current-spec)
    (define-key map (kbd "n") 'projectile-drupal-find-current-migration)
    (define-key map (kbd "f") 'projectile-drupal-goto-file-at-point)
    (define-key map (kbd "g") 'projectile-drupal-goto-gemfile)
    (define-key map (kbd "r") 'projectile-drupal-goto-routes)
    (define-key map (kbd "h") 'projectile-drupal-goto-schema)
    (define-key map (kbd "p") 'projectile-drupal-goto-spec-helper)
    map)
  "A goto map for `projectile-drupal-mode'."
)

(defvar projectile-drupal-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map (kbd "m") 'projectile-drupal-find-model)
      (define-key prefix-map (kbd "M") 'projectile-drupal-find-current-model)
      (define-key prefix-map (kbd "c") 'projectile-drupal-find-controller)
      (define-key prefix-map (kbd "C") 'projectile-drupal-find-current-controller)
      (define-key prefix-map (kbd "v") 'projectile-drupal-find-view)
      (define-key prefix-map (kbd "V") 'projectile-drupal-find-current-view)
      (define-key prefix-map (kbd "h") 'projectile-drupal-find-helper)
      (define-key prefix-map (kbd "H") 'projectile-drupal-find-current-helper)
      (define-key prefix-map (kbd "l") 'projectile-drupal-find-lib)
      (define-key prefix-map (kbd "s") 'projectile-drupal-find-spec)
      (define-key prefix-map (kbd "S") 'projectile-drupal-find-current-spec)
      (define-key prefix-map (kbd "n") 'projectile-drupal-find-migration)
      (define-key prefix-map (kbd "N") 'projectile-drupal-find-current-migration)
      (define-key prefix-map (kbd "i") 'projectile-drupal-find-initializer)
      (define-key prefix-map (kbd "j") 'projectile-drupal-find-javascript)
      (define-key prefix-map (kbd "o") 'projectile-drupal-find-log)
      (define-key prefix-map (kbd "e") 'projectile-drupal-find-environment)
      (define-key prefix-map (kbd "a") 'projectile-drupal-find-locale)
      (define-key prefix-map (kbd "@") 'projectile-drupal-find-mailer)
      (define-key prefix-map (kbd "y") 'projectile-drupal-find-layout)
      (define-key prefix-map (kbd "r") 'projectile-drupal-console)
      (define-key prefix-map (kbd "t") 'projectile-drupal-generate)
      (define-key prefix-map (kbd "RET") 'projectile-drupal-goto-file-at-point)

      (define-key prefix-map (kbd "g") projectile-drupal-mode-goto-map)
      (define-key map projectile-drupal-keymap-prefix prefix-map))
    map)
  "Keymap for `projectile-drupal-mode'.")

(easy-menu-define projectile-drupal-menu projectile-drupal-mode-map
  "Menu for `projectile-drupal-mode'."
  '("Drupal"
    ["Find model"                projectile-drupal-find-model]
    ["Find controller"           projectile-drupal-find-controller]
    ["Find view"                 projectile-drupal-find-view]
    ["Find helper"               projectile-drupal-find-helper]
    ["Find lib"                  projectile-drupal-find-lib]
    ["Find spec"                 projectile-drupal-find-spec]
    ["Find log"                  projectile-drupal-find-log]
    ["Find migration"            projectile-drupal-find-migration]
    ["Find javascript"           projectile-drupal-find-javascript]
    ["Find initializer"          projectile-drupal-find-initializer]
    ["Find environment"          projectile-drupal-find-environment]
    ["Find locale"               projectile-drupal-find-locale]
    ["Find mailer"               projectile-drupal-find-mailer]
    ["Find layout"               projectile-drupal-find-layout]
    "--"
    ["Go to file at point"       projectile-drupal-goto-file-at-point]
    "--"
    ["Go to Gemfile"             projectile-drupal-goto-gemfile]
    ["Go to routes"              projectile-drupal-goto-routes]
    ["Go to schema"              projectile-drupal-goto-schema]
    ["Go to spec_helper"         projectile-drupal-goto-spec-helper]
    "--"
    ["Go to current model"       projectile-drupal-find-current-spec]
    ["Go to current controller"  projectile-drupal-find-current-controller]
    ["Go to current view"        projectile-drupal-find-current-view]
    ["Go to current spec"        projectile-drupal-find-current-spec]
    ["Go to current migration"   projectile-drupal-find-current-migration]
    "--"
    ["Run console"               projectile-drupal-console]
    ["Run drupal generate"       projectile-drupal-generate]))

;;;###autoload
(define-minor-mode projectile-drupal-mode
  "Drupal mode based on projectile"
  :init-value nil
  :lighter " Drupal"
  (when projectile-drupal-mode
    (and projectile-drupal-expand-snippet (projectile-drupal-expand-snippet-maybe))
    (projectile-drupal-set-javascript-dirs)))

;;;###autoload
(defun projectile-drupal-on ()
  "Enable `projectile-drupal-mode' minor mode if this is a drupal project."
  (when (projectile-drupal-root)
    (projectile-drupal-mode +1)))

(defun projectile-drupal-off ()
  "Disable `projectile-drupal-mode' minor mode."
  (projectile-drupal-mode -1))

(define-derived-mode projectile-drupal-compilation-mode compilation-mode "Projectile Drupal Compilation"
  "Compilation mode used by `projectile-drupal'."
  (add-hook 'compilation-filter-hook 'projectile-drupal-apply-ansi-color nil t))

(define-derived-mode projectile-drupal-generate-mode projectile-drupal-compilation-mode "Projectile Drupal Generate"
  "Mode for output of drupal generate."
  (add-hook 'compilation-finish-functions 'projectile-drupal-make-buttons nil t))

(provide 'projectile-drupal)

;;; projectile-drupal.el ends here
