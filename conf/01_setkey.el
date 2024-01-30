;;;;;;;;;;;;;
;; General

(defvar ~custom-key-plists
  `(
    ;; 移動
    (:key "C-h"     :cmd backward-char             :jack t)
    (:key "C-l"     :cmd forward-char              :jack t)
    (:key "<left>"  :cmd backward-char             :jack t)
    (:key "<right>" :cmd forward-char              :jack t)
    (:key "C-S-h"   :cmd backward-word             :jack t)
    (:key "C-S-l"   :cmd forward-word              :jack t)
    (:key "M-h"     :cmd backward-sexp             :jack t)
    (:key "M-l"     :cmd forward-sexp              :jack t)
    (:key "C-M-h"   :cmd pophint-region:backward   :jack t)
    (:key "C-M-l"   :cmd pophint-region:forward    :jack t)
    (:key "M-S-h"   :cmd ~scroll-right             :jack t)
    (:key "M-S-l"   :cmd ~scroll-left              :jack t)
    (:key "C-j"     :cmd next-line                 :jack t)
    (:key "C-k"     :cmd previous-line             :jack t)
    (:key "C-S-j"   :cmd scroll-up-command         :jack t)
    (:key "C-S-k"   :cmd scroll-down-command       :jack t)
    (:key "<next>"  :cmd scroll-up-command         :jack t)
    (:key "<prior>" :cmd scroll-down-command       :jack t)
    (:key "M-j"     :cmd ~end-of-block             :jack t)
    (:key "M-k"     :cmd ~beginning-of-block       :jack t)
    (:key "C-M-j"   :cmd ~pophint:forward          :jack t)
    (:key "C-M-k"   :cmd ~pophint:backward         :jack t)
    (:key "C-M-S-j" :cmd end-of-buffer             :jack t)
    (:key "C-M-S-k" :cmd beginning-of-buffer       :jack t)
    (:key "C-a"     :cmd ~beginning-of-line        :jack t)
    (:key "C-S-a"   :cmd goto-line                 :jack t)
    (:key "C-o"     :cmd owdriver-start            :jack t)
    (:key "C-S-o"   :cmd other-window              :jack t)
    (:key "C-,"     :cmd point-undo                :jack t)
    (:key "C-."     :cmd point-redo                :jack t)
    (:key "M-,"     :cmd goto-last-change          :jack t)
    (:key "M-."     :cmd goto-last-change-reverse  :jack t)
    (:key "C-M-."   :cmd pop-global-mark           :jack t)
    (:key "C-<"     :cmd ~pop-marker-stack         :jack t)
    (:key "C->"     :cmd ~find-definition          :jack t)
    (:key "C-M->"   :cmd ~find-references          :jack t)
    (:key "M-b"     :cmd next-error                :jack t)
    (:key "M-S-b"   :cmd previous-error            :jack t)
    ;; 表示
    (:key "C-p"     :cmd seq-recenter                                   :jack t)
    (:key "C-S-z"   :cmd delete-other-windows                           :jack t)
    (:key "M-z"     :cmd delete-window                                  :jack t)
    (:key "H-h"     :cmd owdriver-do-scroll-right                       :jack t)
    (:key "H-j"     :cmd owdriver-do-scroll-up-command                  :jack t)
    (:key "H-k"     :cmd owdriver-do-scroll-down-command                :jack t)
    (:key "H-l"     :cmd owdriver-do-scroll-left                        :jack t)
    (:key "H-M-h"   :cmd owdriver-do-scroll-right-on-next-window        :jack t)
    (:key "H-M-j"   :cmd owdriver-do-scroll-up-command-on-next-window   :jack t)
    (:key "H-M-k"   :cmd owdriver-do-scroll-down-command-on-next-window :jack t)
    (:key "H-M-l"   :cmd owdriver-do-scroll-left-on-next-window         :jack t)
    (:key "M-o"     :cmd yaol-config-hydra/body                         :jack t)
    (:key "H-o"     :cmd yaol-config-hydra-other-window/body            :jack t)
    (:key "C-|"     :cmd ~split-window-horizontally-and-select)
    (:key "C--"     :cmd ~split-window-vertically-and-select)
    ;; 編集
    (:key "C-S-m"     :cmd ~next-line-with-insert         :jack t :kind edit)
    (:key "C-d"       :cmd backward-delete-char-untabify  :jack t :kind edit)
    (:key "C-f"       :cmd delete-char                    :jack t :kind edit)
    (:key "C-S-d"     :cmd ~backward-kill-line            :jack t :kind edit)
    (:key "C-S-f"     :cmd kill-line                      :jack t :kind edit)
    (:key "H-d"       :cmd pophint-region:backward-delete :jack t :kind edit)
    (:key "H-f"       :cmd pophint-region:delete          :jack t :kind edit)
    (:key "H-M-d"     :cmd pophint-region:backward-kill   :jack t :kind edit)
    (:key "H-M-f"     :cmd pophint-region:kill            :jack t :kind edit)
    (:key "C-w"       :cmd clipboard-kill-ring-save       :jack t :kind edit)
    (:key "C-S-w"     :cmd clipboard-kill-region          :jack t :kind edit)
    (:key "C-S-y"     :cmd yank-pop                       :jack t :kind edit)
    (:key "M-y"       :cmd pophint:do-flexibly-yank       :jack t :kind edit)
    (:key "C-M-y"     :cmd pophint:do-rangeyank           :jack t :kind edit)
    (:key "C-r"       :cmd query-replace                  :jack t :kind edit)
    (:key "C-S-r"     :cmd query-replace-regexp           :jack t :kind edit)
    (:key "M-r"       :cmd read-only-mode                 :jack t :kind edit)
    (:key "C-M-r"     :cmd ~view-toggle-next-activate     :jack t :kind edit)
    (:key "C-z"       :cmd keyboard-escape-quit           :jack t :kind edit) ; C-gの弱い版
    (:key "C-M-z"     :cmd keyboard-quit                  :jack t :kind edit) ; C-g
    (:key "C-S-M-z"   :cmd abort-recursive-edit           :jack t :kind edit)
    (:key "C-'"       :cmd comment-dwim                   :jack t :kind edit)
    (:key "C-\""      :cmd comment-box                    :jack t :kind edit)
    (:key "C-?"       :cmd redo                           :jack t :kind edit)
    (:key "M-/"       :cmd vertico-repeat-last            :jack t :kind edit)
    (:key "C-M-/"     :cmd vertico-repeat-select          :jack t :kind edit)
    (:key "C-n"       :cmd align                          :jack t :kind edit)
    (:key "C-S-n"     :cmd align-regexp                   :jack t :kind edit)
    (:key "M-n"       :cmd ~tidy-code-current             :jack t :kind edit)
    (:key "C-M-n"     :cmd ~tidy-code-diff-files          :jack t :kind edit)
    (:key "C-S-i"     :cmd toggle-input-method            :jack t :kind edit)
    (:key "C-x i"     :cmd indent-region                  :jack t :kind edit)
    (:key "C-S-SPC"   :cmd ~set-mark-only                 :jack t :kind edit)
    (:key "C-x C-SPC" :cmd cua-rectangle-mark-mode        :jack t :kind edit)
    ;; 検索・参照
    (:key "C-S-s" :cmd isearch-backward                  :jack t)
    (:key "M-i"   :cmd ~imenu                            :jack t)
    (:key "H-i"   :cmd owdriver-do-~imenu                :jack t)
    (:key "H-M-i" :cmd owdriver-do-~imenu-on-next-window :jack t)
    (:key "C-;"   :cmd ~popup-document-frame             :jack t)
    (:key "C-:"   :cmd ~focus-document-frame             :jack t)
    (:key "C-M-;" :cmd ~popup-document-buffer            :jack t)
    (:key "C-M-:" :cmd ~hydra-browse/body                :jack t)
    (:key "M-;"   :cmd ~dwim-at-point                    :jack t)
    (:key "H-;"   :cmd ~action-at-point                  :jack t)
    (:key "C-v"   :cmd ~hydra-git/body                   :jack t)
    (:key "C-S-v" :cmd ~hydra-git-gutter/body            :jack t)
    ;; ファイル・バッファ
    (:key "C-x f"   :cmd recentf-open-files     :jack t)
    (:key "C-x F"   :cmd find-file-at-point     :jack t)
    (:key "C-x C-d" :cmd ~direx:jump-to-smartly :jack t)
    ;; (:key "C-x d"   :cmd ffap-list-directory)
    ;; (:key "C-x d"   :cmd dired-at-point)
    ;; (:key "M-f"     :cmd follow-delete-other-windows-and-split)
    (:key "C-x C-S-f" :cmd revert-buffer            :jack t)
    (:key "C-x C-S-r" :cmd ~revert-buffer-with-sudo :jack t)
    (:key "C-x C-b"   :cmd switch-to-buffer         :jack t)
    (:key "C-x b"     :cmd ~ibuffer-other-window    :jack t)
    (:key "C-b"       :cmd persp-switch-to-buffer   :jack t)
    (:key "C-x l"     :cmd find-library             :jack t)
    ;; マクロ
    (:key "C-("       :cmd kmacro-start-macro        :jack t :kind edit)
    (:key "C-)"       :cmd kmacro-end-macro          :jack t :kind edit)
    (:key "C-0"       :cmd kmacro-end-and-call-macro :jack t :kind edit)
    (:key "C-x C-k s" :cmd ~kmacro-save              :jack t)
    ;; emacsclient
    ;; (:key "C-x C-z" :cmd server-edit :jack t)
    ))

(dolist (e ~custom-key-plists)
  (let ((~keyjack-define-with-global-set-key (plist-get e :jack))
        (cmd (plist-get e :cmd))
        (key (plist-get e :key)))
    (if cmd
        (global-set-key (kbd key) cmd)
      (global-unset-key (kbd key)))))


;;;;;;;;;;;;;
;; Utility

(defhydra ~hydra-insert (:exit t)
  "insert"
  ("d" ~insert-date "date")
  ("f" ~insert-file-name "filename")
  ("p" ~insert-programmatic-ident-from-file-name "objectname"))

(defhydra ~hydra-kill (:exit t)
  "kill"
  ("f" ~kill-ring-save-file-path "path")
  ("n" ~kill-ring-save-file-name "filename")
  ("p" ~kill-ring-save-file-path-in-project "path in project"))

(defhydra ~hydra-echo (:exit t)
  "echo"
  ("p" ~echo-file-path "path")
  ("f" ~echo-face-at-point "face")
  ("g" ~echo-git-diff-file-path-list "git diff path list"))

(defhydra ~hydra-text ()
  "text"
  ("S" text-scale-increase "scale inc")
  ("s" text-scale-decrease "scale dec")
  ("q" nil "quit"))

(defhydra ~hydra-case ()
   "case"
   ("u" ~case-upper "upper")
   ("l" ~case-lower "lower")
   ("c" ~case-capitalize-from-snake "capitalize from snake")
   ("C" ~case-capitalize "capitalize")
   ("s" ~case-snake "snake")
   ("q" nil "quit"))

(global-unset-key (kbd "M-u"))
(global-set-key
 (kbd "M-u")
 (defhydra ~hydra-util (:exit t :idle ~hydra-help-delay)
   "util"
   ("i" ~hydra-insert/body "hydra insert")
   ("k" ~hydra-kill/body "hydra kill")
   ("e" ~hydra-echo/body "hydra echo")
   ("t" ~hydra-text/body "hydra text")
   ("c" ~hydra-case/body "hydra case")
   ("w" ~window-resizer "window resize")))


;;;;;;;;;;;;
;; Search

(defhydra ~hydra-isearch (:exit t :idle ~hydra-help-delay)
  "isearch"
   ("w" isearch-forward-word "word")
   ("s" isearch-forward-symbol "symbol")
   ("r" isearch-forward-regexp "regexp")
   ("R" isearch-backward-regexp "regexp to backward"))

(defhydra ~hydra-grep (:exit t :idle ~hydra-help-delay)
  "grep"
  ("l" lgrep "lgrep")
  ("r" rgrep "rgrep")
  ("c" ~consult-grep "consult")
  ("n" pophint-thing:just-~consult-grep "consult no-hint")
  ("g" ~consult-git-grep "git by consult")
  ("G" ~grep-by-git "git"))

(defhydra ~hydra-ag (:exit t :idle ~hydra-help-delay)
  "ag"
  ("a" ~ag "normal")
  ("n" pophint-thing:just-~ag "normal no-hint"))

(defhydra ~hydra-rg (:exit t :idle ~hydra-help-delay)
  "rg"
  ("r" rg "normal")
  ("l" rg-literal "non-regexp")
  ("d" rg-dwim "dwim")
  ("p" rg-project "project")
  ("c" ~consult-ripgrep "consult")
  ("n" pophint-thing:just-~consult-ripgrep "consult no-hint"))

(defhydra ~hydra-moccur (:exit t :idle ~hydra-help-delay)
  "moccur"
  ("m" moccur "normal")
  ("g" ~moccur-grep "grep")
  ("f" ~moccur-grep-find "grep find")
  ("d" ~dmoccur-recursive "dir recursive")
  ("D" ~dmoccur "dir"))

(global-unset-key (kbd "H-s"))
(global-set-key
 (kbd "H-s")
 (defhydra ~hydra-search (:hint nil :exit t :idle ~hydra-help-delay)
   "
_I_: hydra isearch   _i_: by hint
_g_: hydra grep      _o_: other window
_m_: hydra moccur    _f_: find name
_a_: hydra ag        _w_: by www
_r_: hydra rg        _s_: swiper
"
   ("I" ~hydra-isearch/body)
   ("g" ~hydra-grep/body)
   ("m" ~hydra-moccur/body)
   ("a" ~hydra-ag/body)
   ("r" ~hydra-rg/body)
   ("i" pophint:do-flexibly-isearch)
   ("o" owdriver-do-isearch-forward)
   ("f" find-name-dired)
   ("w" pophint:do-flexibly-search)
   ("s" ~swiper)))


;;;;;;;;;;;;;
;; Project

(defhydra ~hydra-projectile-file (:exit t :hint nil)
  ("g" projectile-find-file-dwim "dwim")
  ("f" projectile-find-file-in-known-projects "in known projects")
  ("l" projectile-find-file-in-directory "in directory")
  ("o" projectile-find-file-other-window "to other window")
  ("a" projectile-find-other-file "other file")
  ("A" projectile-find-other-file-other-window "other file to other window")
  ("G" projectile-find-file-dwim-other-window "dwim to other window")
  ("r" projectile-recentf "recentf"))

(defhydra ~hydra-projectile-dir (:exit t :hint nil)
  ("o" projectile-find-dir-other-window "to other window")
  ("d" projectile-dired "dired")
  ("e" projectile-edit-dir-locals "edit dir locals"))

(defhydra ~hydra-projectile-buffer (:exit t :hint nil)
  ("o" projectile-switch-to-buffer-other-window "to other window")
  ("p" projectile-display-buffer "display")
  ("k" projectile-kill-buffers "kill")
  ("K" projectile-kill-buffers--in-other-project "kill in other project")
  ("s" projectile-save-project-buffers "save")
  ("A" projectile-project-buffers-other-buffer "other")
  ("i" projectile-ibuffer "ibuffer"))

(defhydra ~hydra-projectile-search-rails (:exit t :hint nil :idle ~hydra-help-delay)
  ("v" ~projectile-rails-ag-current-partial-view "ag current partial view"))

(defhydra ~hydra-projectile-search-other (:exit t :hint nil :idle ~hydra-help-delay)
  ("a" projectile-ag--in-other-project "ag")
  ("g" ~projectile-consult-ripgrep--in-other-project "rg")
  ("m" projectile-multi-occur--in-other-project "occur")
  ("A" ~projectile-ag-with-directory-select--in-other-project "ag in dir")
  ("G" ~projectile-consult-ripgrep-with-directory-select--in-other-project "rg in dir"))

(defhydra ~hydra-projectile-search (:exit t :hint nil :idle ~hydra-help-delay)
  ("a" projectile-ag "ag")
  ("g" ~projectile-consult-ripgrep "rg")
  ("m" projectile-multi-occur "occur")
  ("A" ~projectile-ag-with-directory-select "ag in dir")
  ("G" ~projectile-consult-ripgrep-with-directory-select "rg in dir")
  ("r" ~hydra-projectile-search-rails/body "hydra rails"))

(defhydra ~hydra-projectile-shell (:exit t :hint nil)
  ("x" projectile-run-shell-command-in-root "shell command")
  ("a" projectile-run-async-shell-command-in-root "shell command async")
  ("s" projectile-run-shell "shell")
  ("e" projectile-run-eshell "eshell")
  ("t" projectile-run-term "term"))

(defhydra ~hydra-projectile-manage (:exit t :hint nil)
  ("r" projectile-replace "replace")
  ("t" projectile-regenerate-tags "regenerate tags")
  ("c" projectile-compile-project "compile")
  ("x" projectile-run-project "run")
  ("s" projectile-switch-project "switch project")
  ("S" projectile-switch-open-project "switch open project"))

(defhydra ~hydra-projectile-rails-goto (:exit t :hint nil)
  ("a" projectile-rails-goto-file-at-point "file at point")
  ("g" projectile-rails-goto-gemfile "gemfile")
  ("r" projectile-rails-goto-routes "routes")
  ("d" projectile-rails-goto-schema "schema")
  ("s" projectile-rails-goto-seeds "seeds")
  ("h" projectile-rails-goto-spec-helper "spec helper"))

(defhydra ~hydra-projectile-rails-manage (:exit t :hint nil)
  ("c" projectile-rails-console "console")
  ("s" projectile-rails-server "server")
  ("r" projectile-rails-rake "rake")
  ("g" projectile-rails-generate "generate")
  ("d" projectile-rails-destroy "destroy")
  ("b" projectile-rails-dbconsole "dbconsole")
  ("e" projectile-rails-extract-region "extract region"))

(defhydra ~hydra-projectile-rails-other (:exit t :hint nil :idle ~hydra-help-delay)
  "
_c_: find controller   _@_: find mailer
_m_: find modle        _l_: find locale
_v_: find view         _i_: find initializer
_j_: find javascript   _b_: find job
_s_: find stylesheet   _a_: find migration
_h_: find helper       _e_: find environment
_p_: find spec
_t_: find test
_u_: find fixture
"
  ("c" projectile-rails-find-controller--in-other-project)
  ("m" projectile-rails-find-model--in-other-project)
  ("v" projectile-rails-find-view--in-other-project)
  ("j" projectile-rails-find-javascript--in-other-project)
  ("s" projectile-rails-find-stylesheet--in-other-project)
  ("h" projectile-rails-find-helper--in-other-project)
  ("p" projectile-rails-find-spec--in-other-project)
  ("t" projectile-rails-find-test--in-other-project)
  ("u" projectile-rails-find-fixture--in-other-project)
  ("@" projectile-rails-find-mailer--in-other-project)
  ("l" projectile-rails-find-locale--in-other-project)
  ("i" projectile-rails-find-initializer--in-other-project)
  ("b" projectile-rails-find-job--in-other-project)
  ("a" projectile-rails-find-migration--in-other-project)
  ("e" projectile-rails-find-environment--in-other-project))

(defhydra ~hydra-projectile-rails (:exit t :hint nil :idle ~hydra-help-delay)
  "
_c_: find controller   _C-c_: find current controller   _@_: find mailer
_m_: find model        _C-m_: find current model        _l_: find locale
_v_: find view         _C-v_: find current view         _i_: find initializer
_j_: find javascript   _C-j_: find current javascript   _b_: find job
_s_: find stylesheet   _C-s_: find current stylesheet   _a_: find migration
_h_: find helper       _C-h_: find current helper       _e_: find environment
_p_: find spec         _C-p_: find current spec
_t_: find test         _C-t_: find current test         _x_: hydra manage
_u_: find fixture      _C-u_: find current fixture      _g_: hydra goto
"
  ("c"   projectile-rails-find-controller)
  ("C-c" projectile-rails-find-current-controller)
  
  ("m"   projectile-rails-find-model)
  ("C-m" projectile-rails-find-current-model)
  
  ("v"   projectile-rails-find-view)
  ("C-v" projectile-rails-find-current-view)
  
  ("j"   projectile-rails-find-javascript)
  ("C-j" projectile-rails-find-current-javascript)
  
  ("s"   projectile-rails-find-stylesheet)
  ("C-s" projectile-rails-find-current-stylesheet)
  
  ("h"   projectile-rails-find-helper)
  ("C-h" projectile-rails-find-current-helper)
  
  ("p"   projectile-rails-find-spec)
  ("C-p" projectile-rails-find-current-spec)
  
  ("t"   projectile-rails-find-test)
  ("C-t" projectile-rails-find-current-test)
  
  ("u"   projectile-rails-find-fixture)
  ("C-u" projectile-rails-find-current-fixture)
  
  ("@"   projectile-rails-find-mailer)
  ("l"   projectile-rails-find-locale)
  ("i"   projectile-rails-find-initializer)
  ("b"   projectile-rails-find-job)
  ("a"   projectile-rails-find-migration)
  ("e"   projectile-rails-find-environment)
  
  ("x"   ~hydra-projectile-rails-manage/body)
  ("g"   ~hydra-projectile-rails-goto/body))

(defhydra ~hydra-projectile-other (:exit t :hint nil :idle ~hydra-help-delay)
  ("f" projectile-find-file--in-other-project "find file")
  ("d" projectile-find-dir--in-other-project "find dir")
  ("b" projectile-switch-to-buffer--in-other-project "switch buffer")
  ("t" projectile-find-test-file--in-other-project "find test")
  ("s" ~hydra-projectile-search-other/body "hydra search")
  ("^" ~projectile-find-root-dir--in-other-project "root dir")
  ("r" ~hydra-projectile-rails-other/body "hydra rails")
  ("c" projectile-invalidate-cache--in-other-project "invalidate cache"))

(global-set-key
  (kbd "M-p")
  (defhydra ~hydra-projectile (:exit t :hint nil :idle ~hydra-help-delay)
    "
_f_: find file    _d_: find dir    _b_: switch buffer   _t_: find test
_F_: hydra file   _D_: hydra dir   _B_: hydra buffer    _T_: hydra test

_s_: hydra search    _x_: hydra shell   _m_: hydra manage   _^_: root dir
_o_: other project   _r_: hydra rails

_c_: invalidate cache   _z_: cache current file   _p_: browse dirty projects
"
    ("f" projectile-find-file)
    ("F" ~hydra-projectile-file/body)
    ("d" projectile-find-dir)
    ("D" ~hydra-projectile-dir/body)
    ("b" projectile-switch-to-buffer)
    ("B" ~hydra-projectile-buffer/body)
    ("t" projectile-find-test-file)
    ("T" projectile-toggle-between-implementation-and-test)
    ("s" ~hydra-projectile-search/body)
    ("x" ~hydra-projectile-shell/body)
    ("m" ~hydra-projectile-manage/body)
    ("^" ~projectile-find-root-dir)
    ("o" ~hydra-projectile-other/body)
    ("r" ~hydra-projectile-rails/body)
    ("c" projectile-invalidate-cache)
    ("z" projectile-cache-current-file)
    ("p" projectile-browse-dirty-projects)))


;;;;;;;;;;;;;;;;;;;;
;; Window Manager

(global-set-key
 (kbd "H-p")
 (defhydra ~hydra-e2wm (:exit t :idle ~hydra-help-delay)
   "e2wm"
   ("a" e2wm:pst-change-command "select")
   ("c" e2wm:dp-code "code")
   ("t" e2wm-transcribe:dp "transcribe")
   ("g" e2wm:dp-magit "git")
   ("S" e2wm:dp-svn "svn")
   ("e" e2wm-term:dp "term")
   ("d" e2wm:dp-edbi "db")
   ("s" e2wm-slack:dp "slack")
   ("T" e2wm:dp-two "two")
   ("D" e2wm:dp-doc "doc")))

(global-set-key (kbd "C-x C-S-b") 'e2wm:dp-array)


;;;;;;;;;;;
;; Shell

(global-set-key
 (kbd "C-x x")
 (defhydra ~hydra-shell (:exit t :hint nil :idle ~hydra-help-delay)
   "
_x_: run                    _c_: compile      _s_: open shell buffer
_a_: async                  _I_: interpret
_p_: with region            _b_: background
_l_: each of region lines
_r_: replace region
_i_: insert result
"
   ("x" shell-command)
   ("a" async-shell-command)
   ("p" shell-command-on-region)
   ("l" ~shell-command-on-region-each-line)
   ("r" ~shell-command-on-region-with-replace)
   ("i" ~shell-command-insert-result)
   ("c" compile)
   ("I" executable-interpret)
   ("b" background)
   ("s" shell)))


;;;;;;;;;;;;;
;; Snippet

(global-set-key
 (kbd "C-x y")
 (defhydra ~hydra-snippet (:exit t :hint nil :idle ~hydra-help-delay)
   "
_f_: find     _r_: regist oneshot               _t_: on/off
_i_: insert   _e_: expand oneshot
_n_: create   _E_: expand oneshot with region
_R_: reload
"
   ("f" yas-visit-snippet-file)
   ("i" yas-insert-snippet)
   ("n" yas-new-snippet)
   ("R" yas-reload-all)
   ("r" ~yas-register-oneshot-snippet)
   ("e" ~yas-expand-oneshot-snippet)
   ("E" ~yas-expand-oneshot-snippet-with-region)
   ("t" yas-minor-mode)))


;;;;;;;;;;;;;;
;; Browsing

(defhydra ~hydra-browse (:exit t :idle ~hydra-help-delay)
  "browse"
  ("j"     browse-url-at-point "open at point")
  ("J"     browse-url "open")
  ("o"     ~browse-search "search external")
  ("o"     pophint-thing:just-~browse-search "search external no-hint")
  ("C-o"   ~browse-search-internally "search internal")
  ("C-S-o" pophint-thing:just-~browse-search-internally "search internal no-hint")
  ("v"     github-browse-file "github file")
  ("C-v"   github-browse-file-blame "github blame")
  ("M-S-v" github-browse-commit "github commit")
  (";"     ~browse-document "document")
  (":"     ~browse-github-code "github code")
  ("u"     ~google-translate-dwim "translate"))


;;;;;;;;;;;;;;;;;;;;;
;; Version Control

(defhydra ~hydra-git-blame (:pre (when (not magit-blame-mode) (magit-blame))
                                 :post (when magit-blame-mode (magit-blame-quit))
                                 :foreign-keys run)
  "git blame"
  ("j" magit-blame-next-chunk "next chunk")
  ("k" magit-blame-previous-chunk "prev chunk")
  ("J" magit-blame-next-chunk-same-commit "next same commit chunk")
  ("K" magit-blame-previous-chunk-same-commit "prev same commit chunk")
  ("o" magit-blame-visit-file "visit file")
  ("O" magit-blame-visit-other-file "visit other file")
  ("h" magit-blame-copy-hash "copy commit hash")
  ("t" magit-blame-cycle-style "cycle style")
  ("q" nil "quit"))

(defhydra ~hydra-git (:exit t :idle ~hydra-help-delay)
  "git"
  ("d" magit-diff-buffer-file "diff")
  ("l" magit-log-buffer-file "log")
  ("L" counsel-git-log "log with counsel")
  ("a" vc-annotate "annotate")
  ("b" ~hydra-git-blame/body "blame")
  ("B" github-browse-file-blame "browse blame")
  ("o" github-browse-file "browse file"))

(defhydra ~hydra-git-gutter (:pre (when (not (ignore-errors git-gutter-mode)) (git-gutter-mode +1))
                                  :post (when git-gutter-mode (git-gutter-mode -1))
                                  :foreign-keys run
                                  :idle ~hydra-help-delay)
  "git gutter"
  ("e" git-gutter "show gutter")
  ("j" git-gutter:next-hunk "next hunk")
  ("k" git-gutter:previous-hunk "prev hunk")
  ("p" git-gutter:popup-hunk "popup hunk")
  ("s" git-gutter:stage-hunk "stage hunk")
  ("r" git-gutter:revert-hunk "revert hunk")
  ("q" nil "quit"))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bug Tracking System

(defhydra ~hydra-bts-project (:exit t)
  "bts project"
  ("n" bts:project-new "create")
  ("u" bts:project-update "update")
  ("d" bts:project-remove "remove")
  ("D" bts:project-remove-all "remove all"))

(defhydra ~hydra-bts-query (:exit t)
  "bts query"
  ("n" bts:query-new "create")
  ("u" bts:query-update "update")
  ("d" bts:query-remove "remove")
  ("D" bts:query-remove-all "remove all"))

(global-unset-key (kbd "M-b"))
(global-set-key
 (kbd "M-b")
 (defhydra ~hydra-bts (:exit t :idle ~hydra-help-delay)
   "bts"
   ("n" bts:ticket-new "new ticket")
   ("s" bts:summary-open "open summary")
   ("p" ~hydra-bts-project/body "hydra bts project")
   ("q" ~hydra-bts-query/body "hydra bts query")))


;;;;;;;;;
;; Tag

(global-set-key
 (kbd "H-t")
 (defhydra ~hydra-tag (:exit t :idle ~hydra-help-delay)
   "tag"
   ("t" visit-tags-table "visit")
   ("l" list-tags "list")
   ("j" find-tag "find")
   ("b" pop-tag-mark "pop")
   ("s" tags-search "search")
   ("r" tags-query-replace "query replace")))


;;;;;;;;;;;;
;; Abbrev

(global-unset-key (kbd "M-a"))
(global-set-key
 (kbd "M-a")
 (defhydra ~hydra-abbrev (:exit t :idle ~hydra-help-delay)
   "abbrev"
   ("e" edit-abbrevs "edit abbrev table")
   ("t" global-abbrev-mode "toggle abbrev activate")
   ("m" define-global-abbrev "add global abbrev")
   ("M" define-mode-abbrev "add local abbrev")
   ("a" ~add-typo-resolve "add typo")))


;;;;;;;;;;
;; Note

(global-set-key
 (kbd "H-n")
 (defhydra ~hydra-note (:exit t :idle ~hydra-help-delay)
   "geeknote"
   ("n" geeknote-create "create")
   ("e" geeknote-edit "edit")
   ("s" geeknote-find "find")
   ("f" geeknote-show "show")
   ("m" geeknote-move "move")))

