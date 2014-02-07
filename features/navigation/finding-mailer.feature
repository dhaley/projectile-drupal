Feature: Finding a mailer file

Background:
  Given I open the app file "app/models/user.rb"
  And file "app/mailers/user_mailer.rb" exists
  And file "app/mailers/admin_mailer.rb" exists
  And I turn on projectile-mode

Scenario: Finding user mailer
  When I run command "projectile-drupal-find-mailer" selecting "user"
  Then I am in file "app/mailers/user_mailer.rb"
