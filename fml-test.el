(require 'fml)

(ert-deftest fml-translate-attr ()
  (should
   (string-equal
    "foo=\"bar\""
    (fml-translate-attr '(foo . "bar")))))

(ert-deftest fml-translate-attrs ()
  (should
   (string-equal
    "foo=\"bar\" baz=\"qux\""
    (fml-translate-attrs '((foo . "bar") (baz . "qux"))))))

(ert-deftest fml-start-tag ()
  (should
   (string-equal
    "<div>"
    (fml-start-tag 'div)))
  (should
   (string-equal
    "<div foo=\"bar\" baz=\"qux\">"
    (fml-start-tag 'div '((foo . "bar") (baz . "qux"))))))

(ert-deftest fml-end-tag ()
  (should
   (string-equal
    "</div>"
    (fml-end-tag 'div))))

(ert-deftest fml-oneline ()
  (should
   (string-equal
    "<div></div>"
    (fml-oneline '(div nil nil))))
  (should
   (string-equal
    "<div>contents</div>"
    (fml-oneline '(div nil "contents"))))
  (should
   (string-equal
    "<div id=\"foo\">contents</div>"
    (fml-oneline '(div ((id . "foo")) "contents"))))
  (should
   (string-equal
    "<div id=\"foo\" class=\"container\">contents</div>"
    (fml-oneline '(div ((id . "foo") (class . "container")) "contents"))))
  (should
   (string-equal
    "<div><div>contents</div></div>"
    (fml-oneline '(div nil (div nil "contents")))))
  (should
   (string-equal
    "<div>contentsA<div>contentsB</div></div>"
    (fml-oneline '(div nil "contentsA" (div nil "contentsB")))))
  (should
   (string-equal
    "<div><div>contentsA</div><div>contentsB</div></div>"
    (fml-oneline '(div nil (div nil "contentsA") (div nil "contentsB")))))
  (should
   (string-equal
    "<meta>"
    (fml-oneline '(meta nil nil))))
  (should
   (string-equal
    "<meta id=\"foo\" class=\"container\">"
    (fml-oneline '(meta ((id . "foo") (class . "container")) nil))))
  (should
   (string-equal
    "<meta>"
    (fml-oneline '(meta nil "contents"))))
  (should
   (string-equal
    "<html lang=\"ja\"><head></head><body width=\"101\"><div class=\"thing\">Foo<div>Yes</div></div></body></html>"
    (fml-oneline '(html ((lang . "ja")) (head nil) (body ((width . "101")) (div ((class . "thing")) "Foo" (div nil "Yes"))))))))

(ert-deftest fml-multiline ()
  (should
   (string-equal
    "<html lang=\"ja\">
<head></head>
<body width=\"101\">
<div class=\"thing\">
Foo
<div>Yes</div>
</div>
</body>
</html>"
    (fml-multiline '(html ((lang . "ja")) (head nil) (body ((width . "101")) (div ((class . "thing")) "Foo" (div nil "Yes"))))))))
