#lang racket

(require toml)

(define (list-mods working-directory)
  (filter
   (λ (path) (string-suffix? (path->string path) ".pw.toml"))
   (directory-list (if working-directory
                       working-directory
                       "./packwiz/mods/")
                   #:build? #t)))

(define (get-mod-name mod-file)
  (hash-ref (parse-toml (file->string mod-file)) 'name))

(define (get-mod-names mod-files)
    (sort (map get-mod-name mod-files) string<?))

(define (generate-markdown-modlist mod-names [title "Mod List"])
  (string-append "# " title "\n\n"
                 (foldr (λ (mod-name acc)
                          (string-append "- " mod-name "\n" acc))
                        ""
                        mod-names)))

(define (write-modlist modlist output-path)
  (display-to-file modlist
                   (if output-path
                       output-path
                       "./mods.md")
                   #:exists 'replace))

;; CLI
(require racket/cmdline)

(define working-directory (make-parameter #f))
(define output-path (make-parameter #f))

(define cli-parser
  (command-line
   #:program "Generate Mod Name List for Packwiz"
   #:once-each
   [("-w" "--workdir") workdir
                       "Set working directory"
                       (working-directory workdir)]
   [("-o" "--output") output
                      "Set output file path"
                      (output-path output)]))

;; Main Entry Point
(define (main)
  (write-modlist (generate-markdown-modlist
                  (get-mod-names
                   (list-mods (working-directory))))
                 (output-path)))

(main)

