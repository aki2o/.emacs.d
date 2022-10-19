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
    (:key "C-S-j"   :cmd ~scroll-up                :jack t)
    (:key "C-S-k"   :cmd ~scroll-down              :jack t)
    (:key "<next>"  :cmd ~scroll-up                :jack t)
    (:key "<prior>" :cmd ~scroll-down              :jack t)
    (:key "M-j"     :cmd ~end-of-block             :jack t)
    (:key "M-k"     :cmd ~beginning-of-block       :jack t)
    (:key "C-M-j"   :cmd ~pophint:forward          :jack t)
    (:key "C-M-k"   :cmd ~pophint:backward         :jack t)
    (:key "C-M-S-j" :cmd end-of-buffer             :jack t)
    (:key "C-M-S-k" :cmd beginning-of-buffer       :jack t)
    (:key "C-a"     :cmd ~beginning-of-line        :jack t)
    (:key "C-S-a"   :cmd goto-line                 :jack t)
    (:key "C-S-o"   :cmd other-window              :jack t)
    (:key "C-,"     :cmd point-undo                :jack t)
    (:key "C-."     :cmd point-redo                :jack t)
    (:key "M-,"     :cmd goto-last-change          :jack t)
    (:key "M-."     :cmd goto-last-change-reverse  :jack t)
    (:key "C-M-."   :cmd pop-global-mark           :jack t)
    (:key "C-<"     :cmd ~pop-marker-stack         :jack t)
    (:key "C->"     :cmd ~find-definition          :jack t)
    (:key "C-M->"   :cmd ~find-references          :jack t)
    (:key "M->"     :cmd helm-lsp-workspace-symbol :jack t)
    (:key "M-b"     :cmd next-error                :jack t)
    (:key "M-S-b"   :cmd previous-error            :jack t)
    ;; 表示
    (:key "C-p"   :cmd seq-recenter                            :jack t)
    (:key "C-z"   :cmd delete-other-windows                    :jack t)
    (:key "C-S-z" :cmd delete-window                           :jack t)
    (:key "H-h"   :cmd owdriver-do-scroll-right                :jack t)
    (:key "H-j"   :cmd owdriver-do-~scroll-up                  :jack t)
    (:key "H-k"   :cmd owdriver-do-~scroll-down                :jack t)
    (:key "H-l"   :cmd owdriver-do-scroll-left                 :jack t)
    (:key "H-M-h" :cmd owdriver-do-scroll-right-on-next-window :jack t)
    (:key "H-M-j" :cmd owdriver-do-~scroll-up-on-next-window   :jack t)
    (:key "H-M-k" :cmd owdriver-do-~scroll-down-on-next-window :jack t)
    (:key "H-M-l" :cmd owdriver-do-scroll-left-on-next-window  :jack t)
    (:key "C-|"   :cmd ~split-window-horizontally-and-select)
    (:key "C--"   :cmd ~split-window-vertically-and-select)
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
    (:key "C-?"       :cmd redo                           :jack t :kind edit)
    (:key "C-r"       :cmd query-replace                  :jack t :kind edit)
    (:key "C-S-r"     :cmd query-replace-regexp           :jack t :kind edit)
    (:key "M-r"       :cmd toggle-read-only               :jack t :kind edit)
    (:key "C-M-r"     :cmd ~view-toggle-next-activate     :jack t :kind edit)
    (:key "C-S-g"     :cmd keyboard-escape-quit           :jack t :kind edit) ; C-gの弱い版
    (:key "M-g"       :cmd abort-recursive-edit           :jack t :kind edit)
    (:key "C-'"       :cmd comment-dwim                   :jack t :kind edit)
    (:key "C-\""      :cmd comment-box                    :jack t :kind edit)
    (:key "M-/"       :cmd ~tidy-code-current             :jack t :kind edit)
    (:key "C-M-/"     :cmd ~tidy-code-diff-files          :jack t :kind edit)
    (:key "C-n"       :cmd align                          :jack t :kind edit)
    (:key "C-S-n"     :cmd align-regexp                   :jack t :kind edit)
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
_I_: hydra isearch _i_: by hint
_g_: hydra grep    _o_: other window
_m_: hydra moccur  _f_: find name
_a_: hydra ag      _w_: by www
_r_: hydra rg      _s_: swiper
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
  "
_f_: in other project   _o_: other window             _r_: recent
_F_: in known projects  _a_: other file
_l_: in dir             _A_: other file other window
_g_: dwim               _G_: dwim other window
"
  ("g" projectile-find-file-dwim)
  ("f" projectile-find-file--in-other-project)
  ("F" projectile-find-file-in-known-projects)
  ("l" projectile-find-file-in-directory)
  ("o" projectile-find-file-other-window)
  ("a" projectile-find-other-file)
  ("A" projectile-find-other-file-other-window)
  ("G" projectile-find-file-dwim-other-window)
  ("r" projectile-recentf))

(defhydra ~hydra-projectile-dir (:exit t :hint nil)
  "
_d_: in other project       _D_: dired
_o_: other window           _E_: edit dir locals
_r_: root
_R_: root in other project
"
  ("d" projectile-find-dir--in-other-project)
  ("r" ~projectile-find-root-dir)
  ("R" ~projectile-find-root-dir--in-other-project)
  ("o" projectile-find-dir-other-window)
  ("D" projectile-dired)
  ("E" projectile-edit-dir-locals))

(defhydra ~hydra-projectile-buffer (:exit t :hint nil)
  "
_b_: switch in other project  _k_: kill                   _i_: ibuffer
_o_: switch other window      _K_: kill in other project
_p_: display                  _s_: save
_A_: all other buffers
"
  ("b" projectile-switch-to-buffer--in-other-project)
  ("o" projectile-switch-to-buffer-other-window)
  ("p" projectile-display-buffer)
  ("k" projectile-kill-buffers)
  ("K" projectile-kill-buffers--in-other-project)
  ("s" projectile-save-project-buffers)
  ("A" projectile-project-buffers-other-buffer)
  ("i" projectile-ibuffer))

(defhydra ~hydra-projectile-test (:exit t :hint nil)
  "
_f_: in other project  _t_: toggle
_o_: to other window   _x_: run
"
  ("f" projectile-find-test-file--in-other-project)
  ("o" projectile-find-implementation-or-test-other-window)
  ("t" projectile-toggle-between-implementation-and-test)
  ("x" projectile-test-project))

(defhydra ~hydra-projectile-search-rails (:exit t :hint nil :idle ~hydra-help-delay)
  "
_v_: ag in current view
"
  ("v" ~projectile-rails-ag-current-partial-view))

(defhydra ~hydra-projectile-search-other (:exit t :hint nil :idle ~hydra-help-delay)
  ("a" projectile-ag--in-other-project "ag")
  ("g" ~projectile-consult-ripgrep--in-other-project "rg")
  ("m" projectile-multi-occur--in-other-project "occur")
  ("A" ~projectile-ag-with-directory-select--in-other-project "ag in dir")
  ("G" ~projectile-consult-ripgrep-with-directory-select--in-other-project "rg in dir"))

(defhydra ~hydra-projectile-search (:exit t :hint nil :idle ~hydra-help-delay)
  ("a" projectile-ag "ag")
  ("g" ~projectile-consult-ripgrep "rg")
  ("A" ~projectile-ag-with-directory-select "ag in dir")
  ("G" ~projectile-consult-ripgrep-with-directory-select "rg in dir")
  ("m" projectile-multi-occur "occur")
  ("o" ~hydra-projectile-search-other "hydra other projects")
  ("r" ~hydra-projectile-search-rails/body "hydra rails"))

(defhydra ~hydra-projectile-shell (:exit t :hint nil)
  "
_x_: run in root    _s_: open shell
_a_: async in root  _e_: open eshell
                  _t_: oepn term
"
  ("x" projectile-run-shell-command-in-root)
  ("a" projectile-run-async-shell-command-in-root)
  ("s" projectile-run-shell)
  ("e" projectile-run-eshell)
  ("t" projectile-run-term))

(defhydra ~hydra-projectile-manage (:exit t :hint nil)
  "
_i_: invalidate cache  _c_: compile
_r_: replace           _x_: run
_t_: regenerate tag    _s_: switch
                     _S_: switch open project
"
  ("i" projectile-invalidate-cache)
  ("r" projectile-replace)
  ("t" projectile-regenerate-tags)
  ("c" projectile-compile-project)
  ("x" projectile-run-project)
  ("s" projectile-switch-project)
  ("S" projectile-switch-open-project))

(defhydra ~hydra-projectile-rails-find-misc (:exit t :hint nil)
  "
_m_: find migration    _M_: find migration in other project    _C-m_: find current migration   _g_: find gemfile
_e_: find environment  _E_: find environment in other project                                _r_: find routes
_s_: find serializer   _S_: find serializer in other project   _C-s_: find current serializer  _d_: find schema
_l_: find lib          _L_: find lib in other project                                        _D_: find seeds
_f_: find feature      _F_: find feature in other project                                    _h_: find spec helper
_o_: find log          _O_: find log in other project
_y_: find layout       _Y_: find layout in other project
_t_: find rake task    _T_: find rake task in other project
_v_: find validator    _V_: find validator in other project
"
  ("m"   projectile-rails-find-migration)
  ("M"   projectile-rails-find-migration--in-other-project)
  ("C-m" projectile-rails-find-current-migration)
  
  ("e"   projectile-rails-find-environment)
  ("E"   projectile-rails-find-environment--in-other-project)
  
  ("s"   projectile-rails-find-serializer)
  ("S"   projectile-rails-find-serializer--in-other-project)
  ("C-s" projectile-rails-find-current-serializer)
  
  ("l"   projectile-rails-find-lib)
  ("L"   projectile-rails-find-lib--in-other-project)
  
  ("f"   projectile-rails-find-feature)
  ("F"   projectile-rails-find-feature--in-other-project)
  
  ("o"   projectile-rails-find-log)
  ("O"   projectile-rails-find-log--in-other-project)
  
  ("y"   projectile-rails-find-layout)
  ("Y"   projectile-rails-find-layout--in-other-project)
  
  ("t"   projectile-rails-find-rake-task)
  ("T"   projectile-rails-find-rake-task--in-other-project)
  
  ("v"   projectile-rails-find-validator)
  ("V"   projectile-rails-find-validator--in-other-project)

  ("g"   projectile-rails-goto-gemfile)
  ("r"   projectile-rails-goto-routes)
  ("d"   projectile-rails-goto-schema)
  ("D"   projectile-rails-goto-seeds)
  ("h"   projectile-rails-goto-spec-helper))

(defhydra ~hydra-projectile-rails-manage (:exit t :hint nil)
  "
_c_: console  _g_: generate   _e_: extract region
_s_: server   _d_: destroy
_r_: rake     _b_: dbconsole
"
  ("c" projectile-rails-console)
  ("s" projectile-rails-server)
  ("r" projectile-rails-rake)
  ("g" projectile-rails-generate)
  ("d" projectile-rails-destroy)
  ("b" projectile-rails-dbconsole)
  ("e" projectile-rails-extract-region))

(defhydra ~hydra-projectile-rails (:exit t :hint nil :idle ~hydra-help-delay)
  "
_c_: find controller  _C_: find controller in other project  _C-c_: find current controller  _@_: find mailer       _C-@_: find mailer in other project
_m_: find model       _M_: find modle in other project       _C-m_: find current model       _a_: find locale       _A_:   find locale in other project
_v_: find view        _V_: find view in other project        _C-v_: find current view        _i_: find initializer  _I_:   find initializer in other project
_j_: find javascript  _J_: find javascript in other project  _C-j_: find current javascript  _b_: find job          _B_:   find job in other project
_s_: find stylesheet  _S_: find stylesheet in other project  _C-s_: find current stylesheet
_h_: find helper      _H_: find helper in other project      _C-h_: find current helper
_p_: find spec        _P_: find spec in other project        _C-p_: find current spec        _>_: goto file at point
_t_: find test        _T_: find test in other project        _C-t_: find current test        _._: hydra find misc
_u_: find fixture     _U_: find fixture in other project     _C-u_: find current fixture     _x_: hydra manage
"
  ("c"   projectile-rails-find-controller)
  ("C"   projectile-rails-find-controller--in-other-project)
  ("C-c" projectile-rails-find-current-controller)
  
  ("m"   projectile-rails-find-model)
  ("M"   projectile-rails-find-model--in-other-project)
  ("C-m" projectile-rails-find-current-model)
  
  ("v"   projectile-rails-find-view)
  ("V"   projectile-rails-find-view--in-other-project)
  ("C-v" projectile-rails-find-current-view)
  
  ("j"   projectile-rails-find-javascript)
  ("J"   projectile-rails-find-javascript--in-other-project)
  ("C-j" projectile-rails-find-current-javascript)
  
  ("s"   projectile-rails-find-stylesheet)
  ("S"   projectile-rails-find-stylesheet--in-other-project)
  ("C-s" projectile-rails-find-current-stylesheet)
  
  ("h"   projectile-rails-find-helper)
  ("H"   projectile-rails-find-helper--in-other-project)
  ("C-h" projectile-rails-find-current-helper)
  
  ("p"   projectile-rails-find-spec)
  ("P"   projectile-rails-find-spec--in-other-project)
  ("C-p" projectile-rails-find-current-spec)
  
  ("t"   projectile-rails-find-test)
  ("T"   projectile-rails-find-test--in-other-project)
  ("C-t" projectile-rails-find-current-test)
  
  ("u"   projectile-rails-find-fixture)
  ("U"   projectile-rails-find-fixture--in-other-project)
  ("C-u" projectile-rails-find-current-fixture)
  
  ("@"   projectile-rails-find-mailer)
  ("C-@" projectile-rails-find-mailer--in-other-project)
  
  ("a"   projectile-rails-find-locale)
  ("A"   projectile-rails-find-locale--in-other-project)
  
  ("i"   projectile-rails-find-initializer)
  ("I"   projectile-rails-find-initializer--in-other-project)
  
  ("b"   projectile-rails-find-job)
  ("B"   projectile-rails-find-job--in-other-project)
  
  (">"   projectile-rails-goto-file-at-point)
  ("."   ~hydra-projectile-rails-find-misc/body)
  ("x"   ~hydra-projectile-rails-manage/body))

(global-set-key
  (kbd "M-p")
  (defhydra ~hydra-projectile (:exit t :hint nil :idle ~hydra-help-delay)
    "
_f_: find file   _d_: find dir   _b_: switch buffer  _t_: find test
_F_: hydra file  _D_: hydra dir  _B_: hydra buffer   _T_: hydra test

_s_: hydra search  _m_: hydra manage  _r_: hydra rails  _x_: hydra shell

_j_: tag jump            _v_: version control
_z_: cache current file  _V_: browse dirty
"
    ("f" projectile-find-file)
    ("F" ~hydra-projectile-file/body)
    ("d" projectile-find-dir)
    ("D" ~hydra-projectile-dir/body)
    ("b" projectile-switch-to-buffer)
    ("B" ~hydra-projectile-buffer/body)
    ("t" projectile-find-test-file)
    ("T" ~hydra-projectile-test/body)
    ("s" ~hydra-projectile-search/body)
    ("x" ~hydra-projectile-shell/body)
    ("m" ~hydra-projectile-manage/body)
    ("r" ~hydra-projectile-rails/body)
    ("j" projectile-find-tag)
    ("v" projectile-vc)
    ("V" projectile-browse-dirty-projects)
    ("z" projectile-cache-current-file)))


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


;;;;;;;;;;;;;
;; Outline

(global-set-key
 (kbd "M-o")
 (defhydra ~hydra-outline (:hint nil :idle ~hydra-help-delay
                                 :post (yaol-fold-clear-all))
   "
^Toggle^                      ^Move^
^^^^^^--------------------------------------------
_s_: cycle headings           _h_: go to parent
_f_: cycle current headings   _j_: go to next
_c_: show current             _k_: go to prev
_a_: show all                 _l_: go to child
^ ^                           _J_: scroll up
^ ^                           _K_: scroll down
^ ^                           _;_: pophint
"
   ("s" seq-yaol-heads)
   ("f" seq-yaol-current-heads)
   ("c" yaol-fold-clear-current)
   ("a" yaol-fold-clear-all)
   ("h" yaol-up-head)
   ("j" yaol-next-sibling-head)
   ("k" yaol-previous-sibling-head)
   ("l" yaol-down-head)
   ("J" ~scroll-up)
   ("K" ~scroll-down)
   (";" pophint:do-yaol-head)
   ("q" nil "quit")))

(global-set-key
 (kbd "H-o")
 (defhydra ~hydra-outline-other-window (:hint nil :idle ~hydra-help-delay
                                              :post (owdriver-do-yaol-fold-clear-all))
   "
^Toggle^                      ^Move^              ^Window^
^^^^^^---------------------------------------------------------------
_s_: cycle headings           _h_: go to parent   _n_: next window
_f_: cycle current headings   _j_: go to next
_c_: show current             _k_: go to prev
_a_: show all                 _l_: go to child
^ ^                           _J_: scroll up
^ ^                           _K_: scroll down
^ ^                           _;_: pophint
"
   ("s" owdriver-do-seq-yaol-heads)
   ("f" owdriver-do-seq-yaol-current-heads)
   ("c" owdriver-do-yaol-fold-clear-current)
   ("a" owdriver-do-yaol-fold-clear-all)
   ("h" owdriver-do-yaol-up-head)
   ("j" owdriver-do-yaol-next-sibling-head)
   ("k" owdriver-do-yaol-previous-sibling-head)
   ("l" owdriver-do-yaol-down-head)
   ("J" owdriver-do-~scroll-up)
   ("K" owdriver-do-~scroll-down)
   (";" owdriver-do-pophint:do-yaol-head)
   ("n" owdriver-next-window)
   ("q" nil "quit")))


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
 (kbd "M-n")
 (defhydra ~hydra-note (:exit t :idle ~hydra-help-delay)
   "geeknote"
   ("n" geeknote-create "create")
   ("e" geeknote-edit "edit")
   ("s" geeknote-find "find")
   ("f" geeknote-show "show")
   ("m" geeknote-move "move")))

