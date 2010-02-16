(module html-tags *
  
  (import scheme chicken srfi-1 srfi-13 data-structures)
  (use utils)
    
  (define xhtml-style? (make-parameter #f))
  
  (define-for-syntax tags/attribs
    (let ((common-attribs
           '(quote-procedure: convert-to-entities?:
             id: class: lang title style dir lang xml:lang tabindex:
             accesskey: onclick: onabort: onblur: onchange: onclick: ondblclick:
             onfocus: onkeydown: onkeypress: onkeyup: onload: onmousedown:
             onmousemove: onmouseover: onmouseout: onmouseup: onreset:
             onselect: onsubmit: onunload:
             )))
      (map (lambda (tags/attribs)
             (append tags/attribs common-attribs))
           '((a            name: href: hreflang: type: rel: rev: charset: coords: shape: accesskey: tabindex: target:)
             (abbr         )
             (acronym      )
             (address      )
             (applet       )
             (area         accesskey: alt: coords: hash: host: hostname: href: noHref: pathname:
                           port: protocol: search: shape: tabindex: target:)
             (b            )
             (base         href: target:)
             (basefont     )
             (bdo          )
             (big          )
             (blink        ) ;; attributes?
             (blockquote   )
             (body         background: bgcolor: text: link: vlink: alink: accesskey: aLink: scrollleft: scrolltop:)
             (bold         )
             (br           clear:)
             (button       accesskey: disabled: form: name: tabindex: type: value:)
             (caption      )
             (center       )
             (cite         )
             (code         )
             (colgroup     )
             (dd           )
             (del          )
             (dir          )
             (div          )
             (dfn          )
             (dl           )
             (dt           )
             (em           )
             (embed        src: width: height: align: name: pluginspage: pluginurl: hidden: href: target: units:
                           autostart: loop: playcount: volume: controls: controller: mastersound: starttime: endtime:)
             (fieldset     )
             (font         color: face: size:)
             (form         action: method: acceptcharset: encoding: enctype: length: name: tabindex: target:)
             (frame        src: contentdocument: frameborder: longdesc: marginheight: marginwidth: name: noresize: scrolling:)
             (frameset     rows: cols:)
             (h1           align:)
             (h2           align:)
             (h3           align:)
             (h4           align:)
             (h5           align:)
             (h6           align:)
             (head         )
             (html         )
             (hr           align:)
             (i            )
             (iframe       src: width: align: height: contentdocument: frameborder: longdesc:
                           marginheight: marginwidth: name: noresize: scrolling:)
             (img          src: alt: align: height: width: border: hspace: vspace: usemap: ismap: longdesc: lowsrc:)
             (input        type: name: value: size: maxlength: checked: src: accept: accesskey:
                           align: alt: defaultchecked: disabled: form: tabindex:)
             (ins          )
             (kbd          )
             (label        for: accesskey: onfocus: onblur:)
             (legend       )
             (li           type: value:)
             (link         charset: disabled: href: hreflang: media: name: rev: rel: target: type:)
             (map          )
             (menu         )
             (meta         name: content: charset: disabled: http-equiv: scheme:)
             (noembed      )
             (noframes     )
             (noscript     )
             (object       )
             (option       value: defaultselected: disabled: form: index: label: selected: text:)
             (optgroup     )
             (ol           )
             (p            align:) ;; something else?
             (param        )
             (pre          width:)
             (q            )
             (script       src: type: language:)
             (s            )
             (samp         )
             (select       name: accesskey: align: disabled: form: length: multiple: selectedindex: size: tabindex: type: value:)
             (small        )
             (span         )
             (strong       )
             (sub          )
             (sup          )
             (strike       )
             (style        media: type:)
             (table        align: border: cellspacing: cellpadding: color: frame: rules: summary: valign: width: bgcolor:)
             (td           rowspan: colspan: nowrap: align: valign: width: height: abbr: accesskey: axis: background:
                           bgcolor: bordercolor: cellindex: ch: choff: disabled: headers: innerhtml: innertext:
                           rowspan: scope: tabindex:)
             (textarea     name: rows: cols: wrap: accesskey: defaultvalue: disabled: readonly: form:)
             (thead        )
             (tbody        )
             (tfoot        )
             (th           rowspan: colspan: nowrap: align: valign: width: height: abbr: accesskey: axis: background:
                           bgcolor: bordercolor: cellindex: ch: choff: disabled: headers: innerhtml: innertext:
                           rowSpan: scope: tabindex:)
             (title        )
             (tr           align: valign: bgcolor: rowspan: colspan: nowrap: align: valign: width: height: abbr:
                           accesskey: axis: background: bgcolor: bordercolor: rowindex: ch: choff: disabled: headers:
                           innerhtml: innertext: scope: tabindex: sectionrowindex: outerhtml: outertext:)
             (tt           )
             (u            bgcolor:)
             (ul           type compact:)
             (var          )
             ))))

  (define open-only-tags (map symbol->string '(base br col embed hr img input link meta param)))
  
  (define check-html-syntax (make-parameter #f))
  
  (define-syntax make-tag
    (lambda (exp r cmp)
      (let ((tag (cadr exp)))
        `(,(r 'define) ,(string->symbol (string-append "<" (symbol->string tag) ">"))
          (,(r 'lambda) attribs
           ,(let ((tag (->string tag)))
              `(let ((tag-attribs (quote ,(alist-ref (string->symbol tag) tags/attribs)))
                     (check-syntax (check-html-syntax))
                     (warnings '())
                     (tag-text (string-append "<" ,tag))
                     (attrs/vals (chop attribs 2))
                     (contents "")
                     (open-only (member ,tag open-only-tags))
                     (quote-proc (or (get-keyword 'quote-procedure: attribs)
                                     (lambda (text) (string-append "'" text "'"))))
                     (convert-to-entities? (get-keyword 'convert-to-entities?: attribs))
                     (htmlize (lambda (str) ;; stolen from spiffy
                                (string-translate* str '(("<" . "&lt;")    (">" . "&gt;")
                                                         ("\"" . "&quot;") ("'" . "&#x27;") ("&" . "&amp;"))))))
                 (for-each (lambda (attr/val)
                             (unless (null? attr/val)
                               (let ((attr (car attr/val))
                                     (val (cdr attr/val)))
                                 (if (keyword? attr)
                                     (begin
                                       (when check-syntax
                                         (unless (memq attr tag-attribs)
                                           (set! warnings (cons attr warnings))))
                                       (unless (memq attr '(quote-procedure: convert-to-entities?:))
                                         (unless (null? val)
                                           (let* ((val (car val))
                                                  (boolean-val? (boolean? val)))
                                             (if boolean-val?
                                                 (when val
                                                   (set! tag-text (string-append tag-text " " (keyword->string attr))))
                                                 (set! tag-text
                                                       (string-append tag-text
                                                                      " "
                                                                      (keyword->string attr)
                                                                      "="
                                                                      (quote-proc (->string val)))))))))
                                     (set! contents (string-append contents (->string attr)
                                                                   (if (null? val)
                                                                       ""
                                                                       (->string (car val)))))))))
                           attrs/vals)
                 (set! tag-text (string-append tag-text
                                               (if (and open-only (xhtml-style?))
                                                   " />"
                                                   ">")))
                 (string-append (if (null? warnings)
                                    ""
                                    (string-append "<!-- WARNING: (" ,tag "): invalid attributes: "
                                                   (string-intersperse (map ->string warnings)) " -->"))
                                tag-text
                                (if convert-to-entities?
                                    (htmlize contents)
                                    contents)
                                (if open-only
                                    ""
                                    (string-append "</" ,tag ">"))))))))))

  (define (<!-- . comments)
    (string-append "<!-- " (string-intersperse (map ->string comments)) " -->"))
  
  (define-syntax make-tags
    (lambda (exp r cmp)
      `(begin
         ,@(map (lambda (tag)
                  `(make-tag ,tag))
                (map car tags/attribs)))))

  (make-tags tags)
  
  )
