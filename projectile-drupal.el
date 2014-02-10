;;; projectile-drupal.el --- Minor mode for Drupal projects based on projectile-mode

;; Copyright (C) 2014 Damon Haley

;; Author:            Damon Haley <dkh@member.fsf.org>
;; URL:               https://github.com/dhaley/projectile-drupal
;; Version:           0.1.1
;; Keywords:          drupal, projectile
;; Package-Requires:  ((projectile "1.0.0-cvs") (f "0.13.0"))

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

(defmacro projectile-drupal-with-root (body-form)
  `(let ((default-directory (projectile-drupal-root)))
     ,body-form))

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
   (file-exists-p (projectile-expand-root "includes/bootstrap.inc"))
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

(defun projectile-drupal-name-at-point ()
  (projectile-drupal-sanitize-name (symbol-name (symbol-at-point))))

(defun projectile-drupal-filename-at-point ()
  (projectile-drupal-sanitize-name (thing-at-point 'filename)))

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
    (and projectile-drupal-expand-snippet (projectile-drupal-expand-snippet-maybe))))

;;;###autoload
(defun projectile-drupal-on ()
  "Enable `projectile-drupal-mode' minor mode if this is a drupal project."
  (when (projectile-drupal-root)
    (projectile-drupal-mode +1)))

(defun projectile-drupal-off ()
  "Disable `projectile-drupal-mode' minor mode."
  (projectile-drupal-mode -1))

(provide 'projectile-drupal)

;;; projectile-drupal.el ends here
