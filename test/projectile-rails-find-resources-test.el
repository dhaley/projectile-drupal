(expectations
 (desc "projectile-drupal-choices"
       (expect '("user" "admin/user")
               (with-mock
                (stub projectile-dir-files => '("/path/app/models/admin/user.rb"
                                                "/path/app/models/user.rb"))
                (projectile-drupal-hash-keys
                 (projectile-drupal-choices '(("/path/app/models/" "/app/models/\\(.+\\)\\.rb$"))))))

       (expect '("users" "admin/users")
               (with-mock
                (stub projectile-dir-files => '("/path/app/controllers/admin/users_controller.rb"
                                                "/path/app/controllers/users_controller.rb"))
                (projectile-drupal-hash-keys
                 (projectile-drupal-choices '(("/path/app/controllers/" "/app/controllers/\\(.+\\)_controller\\.rb$"))))))

       (expect '("users/index.html.slim" "users/_user.html.erb" "admin/users/index.html.haml")
               (with-mock
                (stub projectile-dir-files => '("/path/app/views/admin/users/index.html.haml"
                                                "/path/app/views/users/_user.html.erb"
                                                "/path/app/views/users/index.html.slim"))
                (projectile-drupal-hash-keys
                 (projectile-drupal-choices '(("/path/app/views/" "/app/views/\\(.+\\)$"))))))

       (expect '("application")
               (with-mock
                (stub projectile-dir-files => '("/path/app/helpers/application_helper.rb"))
                (projectile-drupal-hash-keys
                 (projectile-drupal-choices '(("/path/app/helpers/" "/app/helpers/\\(.+\\)_helper\\.rb$"))))))

       (expect '("admin/logging")
               (with-mock
                (stub projectile-dir-files => '("/path/lib/admin/logging.rb"))
                (projectile-drupal-hash-keys
                 (projectile-drupal-choices '(("/path/lib/" "lib/\\(.+\\)\\.rb$"))))))

       (expect '("controllers/users_controller" "lib/logging" "models/user")
               (with-mock
                (stub projectile-dir-files => '("/path/spec/models/user_spec.rb"
                                                "/path/spec/lib/logging_spec.rb"
                                                "/path/spec/controllers/users_controller_spec.rb"
                                                "/path/spec/spec_helper"))
                (projectile-drupal-hash-keys
                 (projectile-drupal-choices '(("/path/spec/" "/spec/\\(.+\\)_spec\\.rb$"))))))

       )
 )
