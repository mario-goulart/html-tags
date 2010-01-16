#!/usr/bin/csi -script

(use test)

(load "html-tags.scm")
(import html-tags)

(xhtml-style? #t)

(test "<hr />" (<hr>))
(test "<br />" (<br>))
(test "<img src='pic.png' alt='nothing' />" (<img> src: "pic.png" alt: "nothing"))

(xhtml-style? #f)

(test "<a href='ali'>opa</a>" (<a> href: "ali" "opa"))
(test "<pre>a</pre>" (<pre> "a"))
(test "<div id='some'><img src='pic.jpg'></div>" (<div> id: "some" (<img> src: "pic.jpg")))
(test "<div><p>a</p><p>b</p></div>" (<div> (<p> "a") (<p> "b")))
(test "<p align=\"center\">aloalo</p>" (<p> align: "center" quote-procedure: (lambda (x) (conc "\"" x "\"")) "alo" "alo"))
(test "<select><option value='val' selected>opt</option></select>" (<select> (<option> value: "val" selected: #t "opt")))
(test "<select><option value='val'>opt</option></select>" (<select> (<option> value: "val" selected: #f "opt")))
(test "<p>&lt;p&gt;hello&lt;/p&gt;</p>" (<p> convert-to-entities?: #t (<p> "hello")))

