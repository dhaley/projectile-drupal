Feature: Going from a line in a log file
  In order to do find fast files at point in the log file
  As a user
  I want to be able to run projectile-drupal command and jump to the correct file

  Scenario: Going from: Processing by Admin/UsersController#new as HTML
    Given file "app/controllers/admin/users_controller.rb" exists
    And I open the app file "app/models/user.rb"
    And I turn on projectile-mode
    And I clear the buffer and insert:
    """
    Processing by Admin::UsersController#new as HTML
    """
    And I place the cursor between "Admin::Use" and "rs"
    When I run "projectile-drupal-goto-file-at-point"
    Then I am in file "app/controllers/admin/users_controller.rb"

  Scenario: Going from: Rendered users/index.html.erb (43.5ms)
    Given file "app/views/users/index.html.erb" exists
    And I open the app file "app/models/user.rb"
    And I turn on projectile-mode
    And I clear the buffer and insert:
    """
        Rendered users/index.html.erb (43.5ms)
    """
    And I place the cursor between "users/in" and "dex"
    When I run "projectile-drupal-goto-file-at-point"
    Then I am in file "app/views/users/index.html.erb"
