(load-file "elquery.el")
(require 'elquery)

(ert-deftest elquery--query-parser-test ()
  (let ((queries '("kek"
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
    ;;
    (should (equal "<button id=\"color-submit-button\" type=\"submit\">"
                   (elquery-fmt (car (elquery-$ "button" tree)))))
    (should (equal "<section class=\"form-section\">"
                   (elquery-fmt (car (elquery-$ ".form-section" tree)))))
    (should (equal "<script>" (elquery-fmt (car (elquery-$ "script" tree)))))))

(provide 'elquery-test)
;;; elquery-test.el ends here
