(require 'f)

(defvar projectile-drupal-support-path
  (f-dirname load-file-name))

(defvar projectile-drupal-features-path
  (f-parent projectile-drupal-support-path))

(defvar projectile-drupal-root-path
  (f-parent projectile-drupal-features-path))

(add-to-list 'load-path projectile-drupal-root-path)

(defvar projectile-drupal-test-app-path
  (f-canonical (concat (make-temp-file "projectile-drupal-test" t) "/")))

(defvar projectile-drupal-test-spring-pid-file
  (concat
   temporary-file-directory
   "spring/"
   (md5 projectile-drupal-test-app-path 0 -1)
   ".pid"))

(defvar projectile-drupal-test-zeus-pid-file
  (concat projectile-drupal-test-app-path ".zeus.sock"))

(defvar projectile-drupal-test-rake-cache-file
  (concat projectile-drupal-test-app-path "/tmp/rake-output"))

(defun projectile-drupal-test-touch-file (filepath)
  (let ((fullpath (expand-file-name filepath projectile-drupal-test-app-path)))
    (unless (file-exists-p fullpath)
      (if (s-ends-with? "/" fullpath)
          (make-directory fullpath)
        (f-touch fullpath)))))

(defun projectile-drupal-test-create-foo-gem (dir)
  (make-directory (concat projectile-drupal-test-app-path dir))
  (find-file (concat projectile-drupal-test-app-path dir "foo.gemspec"))
  (insert "Gem::Specification.new do |spec|
  spec.name          = 'foo'
  spec.version       = '0.0.0'
end")
  (save-buffer)
  )

(require 'projectile-drupal)
(require 'espuds)
(require 'ert)

(Setup
 (setq kill-buffer-query-functions
       (remq 'process-kill-buffer-query-function
             kill-buffer-query-functions))

 (make-temp-file projectile-drupal-test-app-path t)
 (setq projectile-indexing-method 'native)
 (loop for path in `("app/"
                     "app/assets/"
                     "app/assets/javascripts/"
                     "app/assets/stylesheets/"
                     "app/models/"
                     "app/models/admin/"
                     "app/controllers/"
                     "app/controllers/admin/"
                     "app/helpers/"
                     "app/views/"
                     "app/views/users/"
                     "app/views/admin/"
                     "app/views/admin/users/"
                     "app/views/layouts/"
                     "app/jobs/"
                     "app/jobs/admin/"
                     "app/mailers/"
                     "config/"
                     "config/environments/"
                     "config/initializers/"
                     "config/locales/"
                     "db/"
                     "db/migrate/"
                     "lib/"
                     "lib/admin/"
                     "lib/assets/"
                     "lib/assets/javascripts/"
                     "lib/assets/stylesheets/"
                     "public/"
                     "public/javascripts/"
                     "log/"
                     "spec/"
                     "spec/lib/"
                     "spec/models/"
                     "spec/controllers/"
                     "spec/controllers/admin/"
                     "tmp/"
                     "vendor/"
                     "Gemfile"
                     "config/environment.rb"
                     ,(concat temporary-file-directory "spring/"))
       do (projectile-drupal-test-touch-file path))
 )

(Before
 (require 'yasnippet)
 (require 'bundler)
 (require 'rspec-mode)
 (require 'projectile-drupal)

 (add-hook 'projectile-mode-hook 'projectile-drupal-on)

 (loop for file in (list projectile-drupal-test-spring-pid-file
                         projectile-drupal-test-zeus-pid-file
                         projectile-drupal-test-rake-cache-file)
       do (when (f-exists? file) (f-delete file)))

 (setq projectile-completion-system 'ido
       projectile-drupal-expand-snippet nil)

 (cd projectile-drupal-test-app-path)
 )

(After
 (yas-exit-all-snippets)
 (--map (kill-buffer it) (buffer-list))
 )

(Teardown
 (delete-directory projectile-drupal-test-app-path t)
 )
