#lang racket

(require toml)
(require net/http-easy)

(define (list-mod-files working-directory)
  (filter (λ (path) (string-suffix? (path->string path) ".pw.toml"))
          (directory-list working-directory #:build? #t)))

(define (file->mod mod-file)
  (parse-toml (file->string mod-file)))

(define (find-modrinth-game-version target loader hs)
  (findf (λ (arg)
           (and (member target (hash-ref arg 'game_versions))
                (member loader (hash-ref arg 'loaders))))
         hs))

(define (find-curseforge-game-version target loader hs)
  (findf (λ (arg)
           (and (member target (hash-ref arg 'gameVersions))
                (member (string-titlecase loader)
                        (hash-ref arg 'gameVersions))))
         hs))

(define (check-update filename mod target loader)
  (let* ([update (hash-ref mod 'update)]
         [provider (if (hash-has-key? update 'modrinth) 'modrinth 'curseforge)]
         [ids (if (equal? provider 'modrinth)
                  (hash-ref update 'modrinth)
                  (hash-ref update 'curseforge))])
    (if (equal? provider 'modrinth)
        (check-update-modrinth filename ids target loader)
        (check-update-curseforge filename ids target loader))))

(define (check-update-modrinth filename ids target loader)
  (let* ([mod-id (hash-ref ids 'mod-id)]
         [res (response-json
               (get (format "https://api.modrinth.com/v2/project/~a/version"
                            mod-id)))])
    (if (find-modrinth-game-version target loader res) filename #f)))

(define (check-update-curseforge filename ids target loader)
  (let* ([project-id (hash-ref ids 'project-id)]
         [res
          (response-json
           (get (format "https://api.curseforge.com/v1/mods/~a" project-id)
                #:headers
                (hash
                 'x-api-key
                 "$2a$10$crl9R.EvJCxJfXPrsqrDOOgvlfv1uhDv4mY2USrkPs6leYVde2AC."
                 'Accept
                 "application/json")))])
    (if (find-curseforge-game-version target
                                      loader
                                      (hash-ref (hash-ref res 'data)
                                                'latestFiles))
        filename
        #f)))

;; CLI
(require racket/cmdline)

(define working-directory (make-parameter "./disabled_mods"))
(define target-version (make-parameter "1.21"))
(define loader-name (make-parameter "fabric"))

(define cli-parser
  (command-line
   #:program "Check updates with target version and loader for disabled mods"
   #:once-each [("-w" "--workdir")
                workdir
                "Set working directory"
                (working-directory workdir)]
   [("-t" "--target") target "Set target version" (target-version target)]
   [("-l" "--loader") loader "Set loader" (loader-name loader)]))

(define (main)
  (for-each (λ (filename-mod)
              (let ([result (check-update (car filename-mod)
                                          (cdr filename-mod)
                                          (target-version)
                                          (loader-name))])
                (when result
                  (displayln result))))
            (map (λ (path) (cons (file-name-from-path path) (file->mod path)))
                 (list-mod-files (working-directory)))))

(main)
