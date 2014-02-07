Feature: Using drupal generate command
  In order to generate projects's files
  As a user
  I want to be able to run drupal generate command from Emacs

Scenario: Runnning generate rspec:install
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  When I run command "projectile-drupal-generate" inputting "rspec:install"
  And I switch to buffer "*projectile-drupal-generate*"
  Then I should see "bundle exec drupal generate rspec:install"

Scenario: Running generate rspec:install when spring is running
  And spring is running
  When I run command "projectile-drupal-generate" inputting "rspec:install"
  And I switch to buffer "*projectile-drupal-generate*"
  Then I should see "spring drupal generate rspec:install"

Scenario: Running generate rspec:install when zeus is running
  And zeus is running
  When I run command "projectile-drupal-generate" inputting "rspec:install"
  And I switch to buffer "*projectile-drupal-generate*"
  Then I should see "zeus generate rspec:install"

Scenario: Using buttons
  Given I open the app file "app/models/user.rb"
  And file "spec/spec_helper.rb" exists
  And I simulate running "projectile-drupal-generate" inputting "rspec:install" with output:
  """
  be drupal g rspec:install
      create  .rspec
       exist  spec
    conflict  spec/spec_helper.rb
        skip  spec/spec_helper.rb
  """
  When I place the cursor between "conflict  spe" and "c"
  And I press "RET"
  Then I am in file "spec/spec_helper.rb"
