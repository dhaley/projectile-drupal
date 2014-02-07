Feature: Going from a template to a template at point
  In order to do find fast templates and partials at point
  As a user
  I want to be able to run projectile-drupal command and jump to the template or partial at point

Background:
  Given I turn on projectile-mode

  Scenario: Going from: render 'index'
    And file "app/views/users/index.html.erb" exists
    And I open the app file "app/views/users/new.html.erb"
    And I clear the buffer and insert:
    """
    render 'index'
    """
    And I place the cursor between "ind" and "ex"
    When I run "projectile-drupal-goto-file-at-point"
    Then I am in file "app/views/users/index.html.erb"

  Scenario: Going from: render 'users/index'
    And file "app/views/users/index.html.erb" exists
    And I open the app file "app/views/users/new.html.erb"
    And I clear the buffer and insert:
    """
    render 'users/index'
    """
    And I place the cursor between "ind" and "ex"
    When I run "projectile-drupal-goto-file-at-point"
    Then I am in file "app/views/users/index.html.erb"

  Scenario: Going from: render 'admin/users/index'
    And file "app/views/admin/users/index.html.haml" exists
    And I open the app file "app/views/users/new.html.erb"
    And I clear the buffer and insert:
    """
    render 'admin/users/index'
    """
    And I place the cursor between "ind" and "ex"
    When I run "projectile-drupal-goto-file-at-point"
    Then I am in file "app/views/admin/users/index.html.haml"

  Scenario: Going from: render 'users/user'
    And file "app/views/users/_user.html.erb" exists
    And I open the app file "app/views/users/new.html.erb"
    And I clear the buffer and insert:
    """
    render 'users/user'
    """
    And I place the cursor between "use" and "r"
    When I run "projectile-drupal-goto-file-at-point"
    Then I am in file "app/views/users/_user.html.erb"

  Scenario: Going from: render 'user'
    And file "app/views/users/_user.html.erb" exists
    And I open the app file "app/views/users/new.html.erb"
    And I clear the buffer and insert:
    """
    render 'user'
    """
    And I place the cursor between "use" and "r"
    When I run "projectile-drupal-goto-file-at-point"
    Then I am in file "app/views/users/_user.html.erb"
