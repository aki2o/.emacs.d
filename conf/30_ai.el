(use-package copilot
  :defer t
  :custom ((copilot-idle-delay 3)
           (copilot-max-char 300000)
           (copilot-indent-warning-suppress t)) ; copilot--indentation-alist に登録されていない mode では tab-width が使われるらしく、そのことを知らせる警告の抑制
  :hook ((prog-mode . copilot-mode))

  :config
  (bind-keys :map copilot-completion-map
             ("C-<return>" . copilot-accept-completion)
             ("C-S-<return>" . copilot-accept-completion-by-word)
             ("C-M-<return>" . copilot-accept-completion-by-line)
             ("C-," . copilot-previous-completion)
             ("C-." . copilot-next-completion))

  (bind-keys :map ~keyjack-mode-map
             ("C-M-SPC" . copilot-complete))

  (add-to-list 'copilot-major-mode-alist '("enh-ruby" . "ruby")))

(defun my:copilot-notify-project-files ()
  (interactive)
  (let* ((root (projectile-project-root))
         (files (if root
                    (projectile-project-files root)
                  (error "You're not in project."))))
    (dolist (file (my:filtering-read files))
      (my:run-deferred-with (expand-file-name file root) 1
        (my:copilot-notify it)))))

(defun my:copilot-notify (file)
  (let* ((persp-add-buffer-on-find-file nil)
         (buf (find-file-noselect file)))
    (when (not (-contains-p copilot--opened-buffers buf))
      (with-current-buffer buf
        (copilot--notify 'textDocument/didOpen
                         (list :textDocument (list :uri (copilot--get-uri)
                                                   :languageId (copilot--get-language-id)
                                                   :version copilot--doc-version
                                                   :text (copilot--get-source)))))
      (add-to-list 'copilot--opened-buffers buf))))


(use-package chatblade
  :defer t
  :init
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("OPENAI_API_KEY")))
  :custom ((chatblade-default-model "gpt-4-turbo-preview")
           (chatblade-default-switched-model "gpt-4")
           (chatblade-prompt-name-alist '((rust-mode           . "rust")
                                          (typescript-mode     . "ts")
                                          (typescript-tsx-mode . "ts")
                                          (emacs-lisp-mode     . "elisp")))
           (chatblade-prompt-template-function 'my:chatblade-make-prompt-template))
  :config
  (~add-setup-hook 'chatblade-mode
    (setq-local truncate-lines nil)
    (setq-local truncate-partial-width-windows nil)))

(defun my:chatblade-open-without-prompt ()
  (interactive)
  (chatblade-query-open nil))

(defun my:chatblade-fix-error ()
  (interactive)
  (let* ((flycheck-display-errors-function 'flycheck-help-echo-all-error-messages)
         (errors (when flycheck-mode
                   (flycheck-overlay-errors-at (point))))
         (message (or (when errors (flycheck-display-errors errors))
                      (read-string "Input the error: "))))
    (chatblade-start (concat (chatblade-region-string)
                             "\nI got the following error from this codes.\n\n"
                             message "\n\n"
                             "How can I fix?"))))

(defun my:chatblade-fix-bug ()
  (interactive)
  (let ((message (read-string "Input the bug detail: ")))
    (chatblade-start (concat (chatblade-region-string)
                             (format "\nThis codes looks having a bug that %s.\n" message)
                             "Can you figure out how to fix?"))))

(defun my:chatblade-what ()
  (interactive)
  (chatblade-start (concat (chatblade-region-string)
                           "\nCan you figure out what this codes do?")))

(defun my:chatblade-make-doc ()
  (interactive)
  (chatblade-start (concat (chatblade-region-string)
                           "\nPlease write a document for this codes")))

(defun my:chatblade-browse-document ()
  (interactive)
  (let* ((default (thing-at-point 'symbol t))
         (thing (read-string (format "Input the thing (%s): " default) nil nil default))
         (query (format "/doc %s" thing))
         (res (chatblade-request query)))
    (if (s-starts-with? "http" res) (browse-url res) (chatblade-start query))))

(defun my:chatblade-make-prompt-template (thing)
  (mapconcat
   'identity
   `(
     ,(format "Please you act as assistant of %s programming." thing)
     ,(format "\"codes\" means %s codes." thing)
     "If I start with \"/\", the word has special meaning."
     "Here is the list of it and their means."
     "- \"/samp\" is requesting only codes that do the behavior of the given message without any other informations."
     "- \"/comp\" is requesting only codes that you predict and should follow on the given codes without any other informations."
     "- \"/lint\" is requesting only codes that's right for the given codes without any other informations."
     "- \"/doc\" is requesting only a url of official api reference that corresponds to the given message without any other informations."
     "- \"/ggl\" is requesting only a list of url and the short summary that looks useful to the conversation without any other informations."
     )
   "\n"))

(defhydra my:chatblade-hydra (:exit t :hint nil)
  "
^Open Chat^            ^Request^         ^Config^             ^Copilot^
^^^^^^^^-------------------------------------------------------------------------------
_s_: start             _e_: fix error    _p_: find prompt     _n_: notify project files
_g_: start no-prompt   _f_: fix bug      _u_: update prompt
_b_: buffer            _w_: ask what
_r_: resume            _d_: write doc
^^                     _o_: browse doc
"
  ("s" chatblade-query-open)
  ("g" my:chatblade-open-without-prompt)
  ("b" chatblade-switch-to-buffer)
  ("r" chatblade-resume)
  ("e" my:chatblade-fix-error)
  ("f" my:chatblade-fix-bug)
  ("w" my:chatblade-what)
  ("d" my:chatblade-make-doc)
  ("o" my:chatblade-browse-document)
  ("p" chatblade-find-prompt-file)
  ("u" chatblade-update-prompt-file)
  ("n" my:copilot-notify-project-files))

(setq-default ~action-at-point-function 'my:chatblade-hydra/body)

(require 'chatblade)
