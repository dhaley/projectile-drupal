Feature: Finding a layout file

Background:
  Given I open the app file "app/models/user.rb"
  And file "app/views/layouts/application.html.erb" exists
  And file "app/views/layouts/admin.html.erb" exists
  And I turn on projectile-mode

Scenario: Finding user mailer
  When I run command "projectile-drupal-find-layout" selecting "application"
  Then I am in file "app/views/layouts/application.html.erb"
