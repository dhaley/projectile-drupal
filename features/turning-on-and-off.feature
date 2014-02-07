Feature: Enabling projectile-drupal mode
  In order to use the mode
  As a user
  I want to it be turned on when inside a drupal project alongside projectile minor mode

  Scenario: Turning on alongside with projectile
    Given I open the app file "app/models/user.rb"
    And I turn on projectile-mode
    Then projectile-drupal should be turned on

  Scenario: Turning off
    Given I open the app file "app/models/user.rb"
    When I turn on projectile-mode
    Then projectile-drupal should be turned on
    When I turn off projectile-drupal-mode
    Then projectile-drupal should not be turned on

  Scenario: Not turning on if project is not a drupal project
    Given I open the file ".cask"
    And I turn on projectile-mode
    Then projectile-drupal should not be turned on
