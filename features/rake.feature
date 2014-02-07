Feature: Using rake
  In order to use rake
  As a user
  I want to be able to run a rake tasks from Emacs

Background:
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode

Scenario: Running about task
  When I run command "projectile-drupal-rake" selecting "about"
  And I switch to buffer "*projectile-drupal-compilation*"
  Then I should see "bundle exec rake about"

Scenario: Running about task when spring is running
  And spring is running
  When I run command "projectile-drupal-rake" selecting "about"
  And I switch to buffer "*projectile-drupal-compilation*"
  Then I should see "spring rake about"

Scenario: Running about task when zeus is running
  And zeus is running
  When I run command "projectile-drupal-rake" selecting "about"
  And I switch to buffer "*projectile-drupal-compilation*"
  Then I should see "zeus rake about"

Scenario: Using caching
  And the cache file with projectile-drupal task exists
  When I run command "projectile-drupal-rake" selecting "projectile-drupal"
  And I switch to buffer "*projectile-drupal-compilation*"
  Then I should see "bundle exec rake projectile-drupal"
