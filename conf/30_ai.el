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
  :custom ((chatblade-prompt-alist '((rust-mode . "rust")
                                     (typescript-mode . "ts")
                                     (emacs-lisp-mode . "elisp")))
           (chatblade-query-template-alist '(("comp" . "req:comp %s")
                                             ("samp" . ~chatblade-make-samp-query)
                                             ("ref"  . ~chatblade-make-ref-query)
                                             ("err"  . ~chatblade-make-err-query)
                                             ("bug"  . ~chatblade-make-bug-query)
                                             ("ask"  . "Can you figure out what this codes do? ```\n%s\n```")
                                             ("doc"  . "Please write a document for this codes ```\n%s\n```")))
           (chatblade-start-function-alist '(("ref" . ~chatblade-open-reference)))
           (chatblade-prompt-template-function '~chatblade-make-prompt-template)))

(defun ~chatblade-make-samp-query ()
  (let ((text (read-string "Input the behaviour: " nil nil (~dwim-thing-at-point))))
    (format "req:samp %s" text)))

(defun ~chatblade-make-ref-query ()
  (let ((thing (read-string "Input the thing: " nil nil (word-at-point))))
    (format "req:ref %s" thing)))

(defun ~chatblade-make-err-query ()
  (let ((message (read-string "Input the error: ")))
    (concat "```\n%s\n```\n"
            "I got the following error from this codes.\n"
            (format "```\n%s\n```\n" message)
            "How can I fix?")))

(defun ~chatblade-make-bug-query ()
  (let ((message (read-string "Input the bug: ")))
    (concat "```\n%s\n```\n"
            (format "This codes looks having a bug that %s." message)
            "Can you figure out how to fix?")))

(defun ~chatblade-open-document (query)
  (interactive (list (~chatblade-make-ref-query)))
  (let ((res (chatblade-request query :with-prompt t)))
    (if (s-starts-with? "http" res)
        (browse-url res)
      (chatblade-open-interactive query))))

(defun ~chatblade-make-prompt-template (thing)
  (mapconcat
   'identity
   `(
     ,(format "Please act as an assistant of %s programming and be compliant with the following rules." thing)
     ,(format "- A word \"codes\" means %s codes." thing)
     "- If my message starts with \"req:samp\", reply only codes that do the behavior of the given message without any other informations."
     "- If my message starts with \"req:comp\", reply only codes that's predicted to follow on the given codes without any other informations."
     "- If my message starts with \"req:ref\", reply only a url of a referenct that corresponds to the given message without any other informations."
     )
   "\n"))

(defhydra ~chatblade-hydra (:exit t)
  "Chatblade"
  ("s" chatblade-start "start")
  ("b" chatblade-switch-to-buffer "buffer")
  ("d" ~chatblade-open-document "docment")
  ("r" chatblade-resume "resume")
  ("f" chatblade-find-prompt-file "find prompt")
  ("e" chatblade-update-prompt-file "update prompt"))

(setq-default ~action-at-point-function '~chatblade-hydra/body)

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
