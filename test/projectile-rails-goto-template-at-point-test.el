(expectations
 (desc "projectile-drupal-template-name"
       (expect "index"
               (projectile-drupal-template-name "index"))
       (expect "index"
               (projectile-drupal-template-name "index.html"))
       (expect "index"
               (projectile-drupal-template-name "index.html.erb"))
       (expect "_index"
               (projectile-drupal-template-name "_index.html.erb"))
       (expect "index"
               (projectile-drupal-template-name "users/index"))
       (expect "index"
               (projectile-drupal-template-name "users/index.html"))
       (expect "index"
               (projectile-drupal-template-name "users/index.html.erb"))
       (expect "index"
               (projectile-drupal-template-name "admin/users/index"))
       (expect "index"
               (projectile-drupal-template-name "admin/users/index.html"))
       (expect "index"
               (projectile-drupal-template-name "admin/users/index.html.erb"))
       )

 (desc "projectile-drupal-template-format"
       (expect nil
               (with-mock
                (stub buffer-file-name => "users_controller.rb")
                (projectile-drupal-template-format "new")))
       (expect "html"
               (projectile-drupal-template-format "users/index.html.erb"))
       (expect "js"
               (projectile-drupal-template-format "users/index.js.erb"))
       (expect "js"
               (with-mock
                (stub buffer-file-name => "new.js.erb")
                (projectile-drupal-template-format "users/index")))
       (expect "json"
               (with-mock
                (stub projectile-drupal-current-line => "render 'users/index', formats: [:json]")
                (projectile-drupal-template-format "users/index")))
       (expect "json"
               (with-mock
                (stub projectile-drupal-current-line => "render 'users/index', formats: ['json']")
                (projectile-drupal-template-format "users/index")))
       (expect "json"
               (with-mock
                (stub projectile-drupal-current-line => "render 'users/index', formats: [\"json\"]")
                (projectile-drupal-template-format "users/index")))
       (expect "json"
               (with-mock
                (stub projectile-drupal-current-line => "render 'users/index', :formats => [:json]")
                (projectile-drupal-template-format "users/index")))
       (expect "json"
               (with-mock
                (stub projectile-drupal-current-line => "render 'users/index', :formats => ['json']")
                (projectile-drupal-template-format "users/index")))
       (expect "json"
               (with-mock
                (stub projectile-drupal-current-line => "render 'users/index', :formats => [\"json\"]")
                (projectile-drupal-template-format "users/index")))
       )

 (desc "projectile-drupal-template-dir"
       (expect "/path/to/project/app/views/admin/users/"
               (with-mock
                (stub projectile-project-root => "/path/to/project/")
                (projectile-drupal-template-dir "admin/users/index")))
       (expect "/path/to/project/app/views/admin/users/"
               (with-mock
                (stub buffer-file-name => "/path/to/project/app/controllers/admin/users_controller.rb")
                (stub projectile-project-root => "/path/to/project/")
                (projectile-drupal-template-dir "index")))
       (expect "/path/to/project/app/views/admin/users/"
               (let ((default-directory "/path/to/project/app/views/admin/users/"))
                 (with-mock
                  (stub buffer-file-name => "/path/to/project/app/views/admin/users/index.html.erb")
                  (stub projectile-project-root => "/path/to/project/")
                  (projectile-drupal-template-dir "index"))))
       )
 )
