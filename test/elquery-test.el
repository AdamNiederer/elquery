(load-file "elquery.el")
(require 'elquery)

(ert-deftest elquery--query-parser-test ()
  (let ((queries '("*"
                   "kek"
                   ".bur"
                   "#kek.bur"
                   ".kek > bur"
                   "[data-id=kek] + bur > kek#bur.bar[foo=baz]")))
    ;; TODO: Should this work on ""?
    (dolist (query queries)
      (should (equal query (elquery--fmt-union (elquery--parse-union query)))))))

(ert-deftest elquery--read-test ()
  (let ((tree (elquery-read-file "test/test.html")))
    (should (equal "input" (elquery-el (car (elquery-$ "#color-input" tree)))))
    (should (equal "console.log(\"Hello, world!\")" (s-trim (elquery-text (car (elquery-$ "script" tree))))))))

(ert-deftest elquery--write-test ()
  (let ((tree (elquery-read-file "test/test.html"))
        (sgml-basic-offset 2))
    ;; TODO: Make self-closing tags work. For now, just ensure they don't crash.
    (elquery-write tree t)
    (elquery-write tree nil)
    (should (equal "<button id=\"color-submit-button\" type=\"submit\">"
                   (elquery-fmt (car (elquery-$ "button" tree)))))
    (should (equal "<section class=\"form-section\">"
                   (elquery-fmt (car (elquery-$ ".form-section" tree)))))
    (should (equal "<script>" (elquery-fmt (car (elquery-$ "script" tree)))))))

(ert-deftest elquery--next-sibling-test ()
  (let ((tree (elquery-read-file "test/test.html")))
    (should (eq (elquery-next-sibling (car (elquery-$ "form h1" tree)))
                (car (elquery-$ "form input" tree))))))

(ert-deftest elquery--$-+-test ()
  (let ((tree (elquery-read-file "test/test.html")))
    (should (equal "input" (elquery-el (car (elquery-$ "form h1 + *" tree)))))))

(ert-deftest elquery--$->-test ()
  (let ((tree (elquery-read-file "test/test.html")))
    (should (equal nil (car (elquery-$ ".form-section > h1" tree))))
    (should (equal "form" (elquery-el (car (elquery-$ ".form-section > *" tree)))))))

(ert-deftest elquery--$--test ()
  (let ((tree (elquery-read-file "test/test.html")))
    (should (equal "h1" (elquery-el (car (elquery-$ ".form-section h1" tree)))))
    (should (equal 4 (length (-filter #'elquery-elp (elquery-$ ".form-section *" tree)))))
    (should (not (elquery-$ ".form-section h2" tree)))))

(ert-deftest elquery--$-~-test ()
  (let ((tree (elquery-read-file "test/test.html")))
    (should (equal 3 (length (elquery-$ "#color-input ~ *" tree))))))

(provide 'elquery-test)
;;; elquery-test.el ends here
