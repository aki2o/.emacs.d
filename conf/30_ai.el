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


(use-package chatblade
  :defer t
  :init
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("OPENAI_API_KEY")))
  :custom ((chatblade-default-model "4t")
           (chatblade-default-switched-model "4")
           (chatblade-prompt-alist '((rust-mode           . "rust")
                                     (typescript-mode     . "ts")
                                     (typescript-tsx-mode . "ts")
                                     (emacs-lisp-mode     . "elisp")))
           (chatblade-query-template-alist '(("completion to follow curr buf/reg" . "req:comp %s")
                                             ("sample code"                       . my:chatblade-make-samp-query)
                                             ("open reference url"                . my:chatblade-make-ref-query)
                                             ("url list for curr buf/reg"         . my:chatblade-make-ggl-query)
                                             ("fix syntax of curr buf/reg"        . "req:lint %s")
                                             ("fix error caused by curr buf/reg"  . my:chatblade-make-err-query)
                                             ("find bug in curr buf/reg"          . my:chatblade-make-bug-query)
                                             ("what's curr buf/reg"               . "Can you figure out what this codes do? ```\n%s\n```")
                                             ("write document for curr buf/reg"   . "Please write a document for this codes ```\n%s\n```")))
           (chatblade-start-function-alist '(("open reference url" . my:chatblade-open-reference)))
           (chatblade-prompt-template-function 'my:chatblade-make-prompt-template))
  :config
  (~add-setup-hook 'chatblade-mode
    (setq-local truncate-lines nil)
    (setq-local truncate-partial-width-windows nil)))

(defun my:chatblade-make-samp-query ()
  (let* ((default (when (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))))
         (text (read-string "Input the behaviour (default is current region): " nil nil default)))
    (concat "req:samp " text)))

(defun my:chatblade-make-ref-query ()
  (let ((thing (read-string "Input the thing: " nil nil (word-at-point))))
    (format "req:ref %s" thing)))

(defun my:chatblade-make-ggl-query ()
  (let ((text (read-string "Input the description: ")))
    (concat "req:ggl " text " %s")))

(defun my:chatblade-make-err-query ()
  (let ((message (read-string "Input the error: ")))
    (concat "```\n%s\n```\n"
            "I got the following error from this codes.\n\n"
            message "\n\n"
            "How can I fix?")))

(defun my:chatblade-make-bug-query ()
  (let ((message (read-string "Input the bug detail: ")))
    (concat "```\n%s\n```\n"
            (format "This codes looks having a bug that %s." message)
            "Can you figure out how to fix?")))

(defun my:chatblade-open-document (query)
  (interactive (list (my:chatblade-make-ref-query)))
  (let ((res (chatblade-request query)))
    (if (s-starts-with? "http" res)
        (browse-url res)
      (chatblade-open-interactive query))))

(defun my:chatblade-start-without-prompt ()
  (interactive)
  (chatblade-start nil))

(defun my:chatblade-make-prompt-template (thing)
  (mapconcat
   'identity
   `(
     ,(format "Please act as an assistant of %s programming and be compliant with the following rules." thing)
     ,(format "- A word \"codes\" means %s codes." thing)
     "- If my message starts with \"req:samp\", reply only codes that do the behavior of the given message without any other informations."
     "- If my message starts with \"req:comp\", reply only codes that you predict and should follow on the given codes without any other informations."
     "- If my message starts with \"req:lint\", reply only codes that's right for the given codes without any other informations."
     "- If my message starts with \"req:ref\", reply only a url of official document that corresponds to the given message without any other informations."
     "- If my message starts with \"req:ggl\", reply only a list of url and the short summary that lools useful for this case without any other informations."
     "- If my message starts with else, reply normally."
     )
   "\n"))

(defhydra my:chatblade-hydra (:exit t)
  "Chatblade"
  ("s" chatblade-start "start")
  ("g" my:chatblade-start-without-prompt "start without prompt")
  ("b" chatblade-switch-to-buffer "list buffer")
  ("d" my:chatblade-open-document "docment")
  ;; ("r" chatblade-resume "resume")
  ("f" chatblade-find-prompt-file "find prompt")
  ("e" chatblade-update-prompt-file "update prompt"))

(setq-default ~action-at-point-function 'my:chatblade-hydra/body)

(require 'chatblade)

;; (use-package openai
;;   :defer t
;;   :init
;;   (with-eval-after-load 'exec-path-from-shell
;;     (exec-path-from-shell-copy-envs '("OPENAI_API_KEY")))
;;   :config
;;   (setq openai-key (getenv "OPENAI_API_KEY")))


;; (use-package chatgpt
;;   :defer t
;;   :custom ((chatgpt-max-tokens 2048)
;;            (chatgpt-max-history 100)
;;            (chatgpt-input-method 'minibuffer)
;;            (chatgpt-display-tokens-info t)))
