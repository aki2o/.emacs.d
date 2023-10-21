(bundle copilot :type github :pkgname "zerolfx/copilot.el" :branch "main")
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


(bundle magit-popup :type github :pkgname "magit/magit-popup" :branch "master")
(bundle tablist :type github :pkgname "politza/tablist" :branch "master")
(bundle tblui :type github :pkgname "Yuki-Inoue/tblui.el" :branch "master")
(bundle openai :type github :pkgname "emacs-openai/openai" :branch "master")
(use-package openai
  :defer t
  :init
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("OPENAI_API_KEY")))
  :config
  (setq openai-key (getenv "OPENAI_API_KEY")))


(bundle chatgpt :type github :pkgname "emacs-openai/chatgpt" :branch "master")
(use-package chatgpt
  :defer t
  :custom ((chatgpt-max-tokens 2048)
           (chatgpt-max-history 100)
           (chatgpt-input-method 'minibuffer)
           (chatgpt-display-tokens-info t)))
