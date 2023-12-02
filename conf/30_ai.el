(use-package copilot
  :defer t
  :custom ((copilot-idle-delay 3))
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


(use-package openai
  :defer t
  :init
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("OPENAI_API_KEY")))
  :config
  (setq openai-key (getenv "OPENAI_API_KEY")))


(use-package chatgpt
  :defer t
  :custom ((chatgpt-max-tokens 2048)
           (chatgpt-max-history 100)
           (chatgpt-input-method 'minibuffer)
           (chatgpt-display-tokens-info t)))
