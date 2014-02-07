Feature: Going to the files that each Drupal project has
  In order to go to universal files fast
  As a user
  I want to be able to run projectile-drupal command and go to the file

Background:
  Given file "app/models/user.rb" exists
  And I open the app file "app/models/user.rb"
  And I turn on projectile-mode

Scenario: Going to Gemfile
  And file "Gemfile" exists
  When I run "projectile-drupal-goto-gemfile"
  Then I am in file "Gemfile"

Scenario: Going to schema
  And file "db/schema.rb" exists
  When I run "projectile-drupal-goto-schema"
  Then I am in file "db/schema.rb"

Scenario: Going to routes
  And file "config/routes.rb" exists
  When I run "projectile-drupal-goto-routes"
  Then I am in file "config/routes.rb"

Scenario: Going to spec_helper
  And file "spec/spec_helper.rb" exists
  When I run "projectile-drupal-goto-spec-helper"
  Then I am in file "spec/spec_helper.rb"
