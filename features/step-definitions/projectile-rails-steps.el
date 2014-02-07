(When "^I open the app file \"\\(.+\\)\""
      (lambda (filename)
        (find-file (concat projectile-drupal-test-app-path "/" filename))))

(When "^I open the file \"\\(.+\\)\""
      (lambda (filename) (find-file (concat projectile-drupal-root-path "/" filename))))

(When "^I turn on projectile-mode"
      (lambda ()
        (projectile-on)))

(When "^I turn off projectile-drupal-mode"
      (lambda ()
        (projectile-drupal-off)))

(When "^I run command \"\\(.+\\)\" \\(?:selecting\\|inputting\\) \"\\(.+\\)\"$"
      (lambda (command argument)
        (When "I start an action chain")
        (When "I press \"M-x\"")
        (And (s-lex-format "I type \"${command}\""))
        (When "I press \"RET\"")
        (And (s-lex-format "I type \"${argument}\""))
        (And "I execute the action chain")))

(When "^I force font lock refresh"
      (lambda()
        (font-lock-fontify-buffer)))

(Given "^I am using a test completion system$"
      (lambda ()
        (setq projectile-completion-system
              (lambda (prompt choices)
                (with-current-buffer (get-buffer-create projectile-drupal-test-completion-buffer)
                  (insert (s-join "\n" choices)))))))

(Given "^I turn on snippet expansion"
      (lambda ()
        (setq projectile-drupal-expand-snippet t)))

(Given "^I turn off snippet expansion"
      (lambda ()
        (setq projectile-drupal-expand-snippet nil)))

(Given "^I turn off adding keywords"
      (lambda ()
        (setq projectile-drupal-add-keywords nil)))

(When "^I run \"\\(.+\\)\""
      (lambda (command)
        (When "I start an action chain")
        (When "I press \"M-x\"")
        (And (s-lex-format "I type \"${command}\""))
        (And "I execute the action chain")))

(When "^I sleep for \\([0-9]+\\) seconds"
      (lambda(seconds) (sit-for (string-to-int seconds))))

(Given "^spring is running"
       (lambda ()
         (f-touch projectile-drupal-test-spring-pid-file)))

(Given "^zeus is running"
       (lambda ()
         (f-touch projectile-drupal-test-zeus-pid-file)))

(Given "the cache file with projectile-drupal task exists"
       (lambda ()
         (with-temp-file projectile-drupal-test-rake-cache-file
           (insert "rake projectile-drupal #a test task\n"))))

(Then "^projectile-drupal should be turned on"
      (lambda ()
        (should projectile-drupal-mode)))

(Then "^projectile-drupal should not be turned on"
      (lambda () (should-not projectile-drupal-mode)))

(Then "^I am in file \"\\(.+\\)\""
      (lambda (filename)
        (should (string-match-p (s-lex-format "${filename}$") (buffer-file-name)))))

(Then "^I should see \"\\(.+\\)\" font locked"
      (lambda (keyword)
        (When (s-lex-format "I go to word \"${keyword}\""))
        (should (equal (get-text-property (+ (point) 1) 'face) 'font-lock-keyword-face))))

(Then "^I should not see \"\\(.+\\)\" font locked"
      (lambda (keyword)
        (When (s-lex-format "I go to word \"${keyword}\""))
        (should (not (equal (get-text-property (+ (point) 1) 'face) 'font-lock-keyword-face)))))

(Then "^the buffer is auto reverting"
      (lambda ()
        (should (and auto-revert-tail-mode (not auto-revert-verbose)))))

(And "I exit the snippets"
     (lambda ()
       (yas-exit-all-snippets)))

(Then "^I am in a dired buffer \"\\(.+\\)\""
      (lambda (name)
        (should (string=
                 (dired-current-directory)
                 (projectile-expand-root name)))))

(And "I wait for \\([0-9]+\\) seconds"
     (lambda (seconds)
       (sit-for (string-to-int seconds))))

(And "I print the buffer content"
     (lambda ()
       (print (buffer-string))))

(And "I clear the buffer and insert:"
     (lambda (content)
       (When "I clear the buffer")
       (When "I insert:" content)))

(When "I set read-only to false"
      (lambda ()
        (read-only-mode -1)))

(Then "I should be at line \\([0-9]+\\)"
      (lambda (line)
        (should (= (count-lines 1 (point)) (string-to-int line)))))

(When "I simulate running \"\\(.*\\)\" inputting \"\\(.+\\)\" with output:"
      (lambda (command arg output)
        (When (s-lex-format "I run command \"${command}\" inputting \"${arg}\""))
        (And "I switch to buffer \"*projectile-drupal-generate*\"")
        (And "I set read-only to false")
        (When "I clear the buffer")
        (compilation-filter (get-buffer-process (current-buffer)) output)
        (run-hook-with-args 'compilation-finish-functions (current-buffer) "")))

(Given "\\(?:directory\\|file\\) \"\\(.*\\)\" exists"
       (lambda (filepath)
         (projectile-drupal-test-touch-file filepath)))

(Given "there is foo gem in directory \"\\(.+\\)\""
       (lambda (dir)
         (projectile-drupal-test-create-foo-gem dir)))

(Given "I save the buffer"
       (lambda ()
         (save-buffer)))
