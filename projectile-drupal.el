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

(defcustom projectile-drupal-controller-keywords
  '("logger" "polymorphic_path" "polymorphic_url" "mail" "render" "attachments"
    "default" "helper" "helper_attr" "helper_method" "layout" "url_for"
    "serialize" "exempt_from_layout" "filter_parameter_logging" "hide_action"
    "cache_sweeper" "protect_from_forgery" "caches_page" "cache_page"
    "caches_action" "expire_page" "expire_action" "rescue_from" "params"
    "request" "response" "session" "flash" "head" "redirect_to"
    "render_to_string" "respond_with" "before_filter" "append_before_filter"
    "prepend_before_filter" "after_filter" "append_after_filter"
    "prepend_after_filter" "around_filter" "append_around_filter"
    "prepend_around_filter" "skip_before_filter" "skip_after_filter" "skip_filter")
  "List of keywords to highlight for controllers"
  :group 'projectile-drupal
  :type '(repeat string))

(defcustom projectile-drupal-migration-keywords
  '("create_table" "change_table" "drop_table" "rename_table" "add_column"
    "rename_column" "change_column" "change_column_default" "remove_column"
    "add_index" "remove_index" "rename_index" "execute")
  "List of keywords to highlight for migrations"
  :group 'projectile-drupal
  :type '(repeat string))

(defcustom projectile-drupal-model-keywords
  '("default_scope" "named_scope" "scope" "serialize" "belongs_to" "has_one"
    "has_many" "has_and_belongs_to_many" "composed_of" "accepts_nested_attributes_for"
    "before_create" "before_destroy" "before_save" "before_update" "before_validation"
    "before_validation_on_create" "before_validation_on_update" "after_create"
    "after_destroy" "after_save" "after_update" "after_validation"
    "after_validation_on_create" "after_validation_on_update" "around_create"
    "around_destroy" "around_save" "around_update" "after_commit" "after_find"
    "after_initialize" "after_rollback" "after_touch" "attr_accessible"
    "attr_protected" "attr_readonly" "validates" "validate" "validate_on_create"
    "validate_on_update" "validates_acceptance_of" "validates_associated"
    "validates_confirmation_of" "validates_each" "validates_exclusion_of"
    "validates_format_of" "validates_inclusion_of" "validates_length_of"
    "validates_numericality_of" "validates_presence_of" "validates_size_of"
    "validates_existence_of" "validates_uniqueness_of" "validates_with")
  "List of keywords to highlight for models"
  :group 'projectile-drupal
  :type '(repeat string))

(defcustom projectile-drupal-view-keywords
  '("action_name" "atom_feed" "audio_path" "audio_tag" "auto_discovery_link_tag"
    "button_tag" "button_to" "button_to_function" "cache" "capture" "cdata_section"
    "check_box" "check_box_tag" "collection_select" "concat" "content_for"
    "content_tag" "content_tag_for" "controller" "controller_name"
    "controller_path" "convert_to_model" "cookies" "csrf_meta_tag" "csrf_meta_tags"
    "current_cycle" "cycle" "date_select" "datetime_select" "debug"
    "distance_of_time_in_words" "distance_of_time_in_words_to_now" "div_for"
    "dom_class" "dom_id" "email_field" "email_field_tag" "escape_javascript"
    "escape_once" "excerpt" "favicon_link_tag" "field_set_tag" "fields_for"
    "file_field" "file_field_tag" "flash" "form_for" "form_tag"
    "grouped_collection_select" "grouped_options_for_select" "headers"
    "hidden_field" "hidden_field_tag" "highlight" "image_alt" "image_path"
    "image_submit_tag" "image_tag" "j" "javascript_cdata_section"
    "javascript_include_tag" "javascript_path" "javascript_tag" "l" "label"
    "label_tag" "link_to" "link_to_function" "link_to_if" "link_to_unless"
    "link_to_unless_current" "localize" "logger" "mail_to" "number_field"
    "number_field_tag" "number_to_currency" "number_to_human" "number_to_human_size"
    "number_to_percentage" "number_to_phone" "number_with_delimiter"
    "number_with_precision" "option_groups_from_collection_for_select"
    "options_for_select" "options_from_collection_for_select" "params"
    "password_field" "password_field_tag" "path_to_audio" "path_to_image"
    "path_to_javascript" "path_to_stylesheet" "path_to_video" "phone_field"
    "phone_field_tag" "pluralize" "provide" "radio_button" "radio_button_tag"
    "range_field" "range_field_tag" "raw" "render" "request"
    "request_forgery_protection_token" "reset_cycle" "response" "safe_concat"
    "safe_join" "sanitize" "sanitize_css" "search_field" "search_field_tag"
    "select" "select_date" "select_datetime" "select_day" "select_hour"
    "select_minute" "select_month" "select_second" "select_tag" "select_time"
    "select_year" "session" "simple_format" "strip_links" "strip_tags"
    "stylesheet_link_tag" "stylesheet_path" "submit_tag" "t" "tag" "telephone_field"
    "telephone_field_tag" "text_area" "text_area_tag" "text_field" "text_field_tag"
    "time_ago_in_words" "time_select" "time_tag" "time_zone_options_for_select"
    "time_zone_select" "translate" "truncate" "url_field" "url_field_tag"
    "url_for" "url_options" "video_path" "video_tag" "word_wrap")
  "List of keywords to highlight for views"
  :group 'projectile-drupal
  :type '(repeat string))

(defcustom projectile-drupal-active-support-keywords
  '("alias_attribute" "with_options" "delegate")
  "List of keywords to highlight for all `projectile-drupal-mode' buffers"
  :group 'projectile-drupal
  :type '(repeat string))

(defcustom projectile-drupal-font-lock-face-name 'font-lock-keyword-face
  "Face to be used for higlighting drupal the keywords")

(defcustom projectile-drupal-views-re
  "\\.\\(?:html\\|erb\\|haml\\|js\\|slim\\|json\\|coffee\\|css\\)$"
  "Regexp for filtering for view files"
  :group 'projectile-drupal
  :type 'string)

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

(defcustom projectile-drupal-keymap-prefix (kbd "C-c r")
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

(defun projectile-drupal--highlight-keywords (keywords)
  "Highlight the passed KEYWORDS in current buffer."
  (font-lock-add-keywords
   nil
   (list (list
          (concat "\\(^\\|[^_:.@$]\\|\\.\\.\\)\\b"
                  (regexp-opt keywords t)
                  "\\_>")
          (list 2 projectile-drupal-font-lock-face-name)))))

(defun projectile-drupal-add-keywords-for-file-type ()
  "Apply extra font lock keywords specific to models, controllers etc."
  (loop for (re keywords) in `(("_controller\\.rb$"   ,projectile-drupal-controller-keywords)
                               ("app/models/.+\\.rb$" ,projectile-drupal-model-keywords)
                               ("db/migrate/.+\\.rb$" ,projectile-drupal-migration-keywords))
        do (when (and (buffer-file-name) (string-match-p re (buffer-file-name)))
             (projectile-drupal--highlight-keywords
              (append keywords projectile-drupal-active-support-keywords)))))

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

(defun projectile-drupal-rake-tmp-file ()
  (projectile-expand-root "tmp/rake-output"))

(defun projectile-drupal-rake-tasks ()
  "Returns a content of tmp file with rake tasks."
  (if (file-exists-p (projectile-drupal-rake-tmp-file))
      (with-temp-buffer
        (insert-file-contents (projectile-drupal-rake-tmp-file))
        (buffer-string))
    (projectile-drupal-regenerate-rake)
    (projectile-drupal-rake-tasks)))

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
      (define-key prefix-map (kbd "k") 'projectile-drupal-rake)
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
    ["Run rake"                  projectile-drupal-rake]
    ["Run drupal generate"       projectile-drupal-generate]))

;;;###autoload
(define-minor-mode projectile-drupal-mode
  "Drupal mode based on projectile"
  :init-value nil
  :lighter " Drupal"
  (when projectile-drupal-mode
    (and projectile-drupal-expand-snippet (projectile-drupal-expand-snippet-maybe))
    (and projectile-drupal-add-keywords (projectile-drupal-add-keywords-for-file-type))
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
