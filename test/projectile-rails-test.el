(expectations
 (desc "projectile-drupal-sanitize-name"
       (expect "name"
               (projectile-drupal-sanitize-name ":name"))
       (expect "name"
               (projectile-drupal-sanitize-name "/name"))
       (expect "path/name"
               (projectile-drupal-sanitize-name "/path/name"))
       (expect "name"
               (projectile-drupal-sanitize-name "'name'"))
       (expect "name"
               (projectile-drupal-sanitize-name "\"name\""))
       )
 (desc "projectile-drupal-declassify"
       (expect "user"
               (projectile-drupal-declassify "user"))
       (expect "memberships"
               (projectile-drupal-declassify "Memberships"))
       (expect "users_controller"
               (projectile-drupal-declassify "UsersController"))
       (expect "admin/users_controller"
               (projectile-drupal-declassify "Admin::UsersController"))
       (expect "users/index.html.erb"
               (projectile-drupal-declassify "users/index.html.erb"))
       )
 (desc "projectile-drupal-hash-keys"
       (expect '("baz" "bar" "foo")
               (let ((hash (make-hash-table :test 'equal)))
                 (puthash "foo" 1 hash)
                 (puthash "bar" 1 hash)
                 (puthash "baz" 1 hash)
                 (projectile-drupal-hash-keys hash))))
 )
