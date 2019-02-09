;;;;;;;;;;;;;
;; General

(defvar ~custom-key-plists
  `(
    (:key "M-x" :cmd helm-M-x :jack t)
    ;; (:key "M-x" :cmd counsel-M-x :jack t)
    ;; 移動
    (:key "C-h"     :cmd backward-char          :jack t)
    (:key "C-j"     :cmd next-line              :jack t)
    (:key "C-k"     :cmd previous-line          :jack t)
    (:key "C-l"     :cmd forward-char           :jack t)
    (:key "C-S-h"   :cmd backward-word          :jack t)
    (:key "C-S-j"   :cmd inertias-up            :jack t)
    (:key "C-S-k"   :cmd inertias-down          :jack t)
    (:key "C-S-l"   :cmd forward-word           :jack t)
    (:key "C-S-a"   :cmd ~scroll-right          :jack t)
    (:key "C-S-e"   :cmd ~scroll-left           :jack t)
    (:key "C-M-n"   :cmd forward-list           :jack t)
    (:key "C-M-S-n" :cmd backward-list          :jack t)
    (:key "C-{"     :cmd beginning-of-buffer    :jack t)
    (:key "C-}"     :cmd end-of-buffer          :jack t)
    (:key "M-i"     :cmd helm-semantic-or-imenu :jack t) ; helm-imenu
    ;; (:key "M-i"     :cmd counsel-imenu          :jack t)
    (:key "C-M-'"   :cmd helm-all-mark-rings    :jack t)
    ;; ウィンドウ
    (:key "C-z"   :cmd delete-window        :jack t)
    (:key "C-S-z" :cmd delete-other-windows :jack t)
    (:key "C-S-o" :cmd other-window         :jack t)
    (:key "C-p"   :cmd recenter             :jack t)
    (:key "C-|"   :cmd ~split-window-horizontally-and-select)
    (:key "C--"   :cmd ~split-window-vertically-and-select)
    (:key "M-h"   :cmd owdriver-do-scroll-right           :jack t)
    (:key "M-j"   :cmd owdriver-do-inertias-up            :jack t)
    (:key "M-k"   :cmd owdriver-do-inertias-down          :jack t)
    (:key "M-l"   :cmd owdriver-do-scroll-left            :jack t)
    (:key "H-i"   :cmd owdriver-do-helm-semantic-or-imenu :jack t)
    ;; (:key "H-i"   :cmd owdriver-do-counsel-imenu          :jack t)
    ;; 編集
    (:key "C-S-m"   :cmd ~next-line-with-insert        :jack t :kind edit)
    (:key "C-d"     :cmd backward-delete-char-untabify :jack t :kind edit)
    (:key "C-f"     :cmd delete-char                   :jack t :kind edit)
    (:key "C-S-d"   :cmd ~backward-kill-line           :jack t :kind edit)
    (:key "C-S-f"   :cmd kill-line                     :jack t :kind edit)
    (:key "C-H-d"   :cmd backward-kill-word            :jack t :kind edit)
    (:key "C-H-f"   :cmd kill-word                     :jack t :kind edit)
    (:key "C-w"     :cmd kill-ring-save                :jack t :kind edit)
    (:key "C-S-w"   :cmd kill-region                   :jack t :kind edit)
    (:key "C-S-y"   :cmd helm-show-kill-ring           :jack t :kind edit)
    ;; (:key "C-S-y"   :cmd counsel-yank-pop              :jack t :kind edit)
    ;; (:key "C-S-y"   :cmd anything-show-kill-ring       :jack t :kind edit)
    (:key "C-?"     :cmd redo                          :jack t :kind edit)
    (:key "C-r"     :cmd query-replace                 :jack t :kind edit)
    (:key "C-S-r"   :cmd query-replace-regexp          :jack t :kind edit)
    (:key "C-S-g"   :cmd keyboard-escape-quit          :jack t :kind edit) ; C-gの弱い版
    (:key "M-g"     :cmd abort-recursive-edit          :jack t :kind edit)
    (:key "C-v"     :cmd comment-dwim                  :jack t :kind edit)
    (:key "C-S-v"   :cmd comment-box                   :jack t :kind edit)
    (:key "C-n"     :cmd align                         :jack t :kind edit)
    (:key "C-S-n"   :cmd align-regexp                  :jack t :kind edit)
    (:key "C-S-i"   :cmd toggle-input-method           :jack t :kind edit)
    (:key "C-x i"   :cmd indent-region                 :jack t :kind edit)
    (:key "C-S-SPC" :cmd ~set-mark-only                :jack t :kint edit)
    ;; 検索
    (:key "C-S-s" :cmd isearch-backward :jack t)
    ;; ファイル
    (:key "C-x C-f" :cmd helm-find-files    :jack t)
    ;; (:key "C-x C-f" :cmd counsel-find-file  :jack t)
    ;; (:key "C-x C-f" :cmd anything-find-file  :jack t)
    (:key "C-x f"   :cmd helm-recentf       :jack t)
    ;; (:key "C-x f"   :cmd anything-recentf   :jack t)
    (:key "C-x F"   :cmd find-file-at-point :jack t)
    (:key "C-x C-d" :cmd dired              :jack t)
    ;; (:key "C-x d"   :cmd ffap-list-directory)
    ;; (:key "C-x d"   :cmd dired-at-point)
    ;; (:key "M-f"     :cmd follow-delete-other-windows-and-split)
    ;; バッファ
    (:key "C-x C-S-f" :cmd revert-buffer         :jack t)
    (:key "C-x C-b"   :cmd helm-buffers-list     :jack t)
    ;; (:key "C-x C-b"   :cmd anything-buffers-list :jack t)
    (:key "C-x b"     :cmd ~ibuffer-other-window :jack t)
    ;; 行ジャンプ
    (:key "C-x j" :cmd goto-line :jack t)
    ;; マクロ
    (:key "C-("       :cmd kmacro-start-macro        :jack t :kind edit)
    (:key "C-)"       :cmd kmacro-end-macro          :jack t :kind edit)
    (:key "C-0"       :cmd kmacro-end-and-call-macro :jack t :kind edit)
    (:key "C-x C-k s" :cmd ~kmacro-save              :jack t)
    ;; タグジャンプ
    (:key "C-<"     :cmd pop-tag-mark)
    (:key "C->"     :cmd find-tag)
    ;; register
    ;; (:key "M-y" :cmd ~register-paste :jack t)
    ;; エラー移動
    (:key "C-b"   :cmd next-error     :jack t)
    (:key "C-S-b" :cmd previous-error :jack t)
    ;; emacsclient
    ;; (:key "C-x C-z" :cmd server-edit :jack t)
    ;; help
    ;; (:key "<f1> a" :cmd helm-apropos)
    ))

(dolist (e ~custom-key-plists)
  (let ((~keyjack-define-with-global-set-key (plist-get e :jack)))
    (global-set-key (kbd (plist-get e :key)) (plist-get e :cmd))))


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
  "
_l_: lgrep _g_: git grep
_r_: rgrep _h_: git grep by helm
             _c_: git grep by counsel
"
  ("l" lgrep)
  ("r" rgrep)
  ("g" ~grep-by-git)
  ("h" helm-git-grep)
  ("c" ~counsel-git-grep))

(defhydra ~hydra-ag (:exit t :idle ~hydra-help-delay)
  "ag"
  ("a" ~ag "normal")
  ("A" pophint-config:thing-do-~ag-with-toggle-effect "normal without hint")
  ("h" ~helm-ag "by helm")
  ("H" pophint-config:thing-do-~helm-ag-with-toggle-effect "by helm without hint")
  ("c" ~counsel-ag "by counsel"))

(defhydra ~hydra-rg (:exit t :idle ~hydra-help-delay)
  "rg"
  ("r" rg "normal")
  ("l" rg-literal "non-regexp")
  ("d" rg-dwim "dwim")
  ("p" rg-project "project"))

(defhydra ~hydra-moccur (:exit t :idle ~hydra-help-delay)
  "
_m_: moccur            _o_: occur by anything
_g_: grep              _b_: buffer by anything
_f_: grep find         _k_: dmoccur by anything
_d_: dmoccur recursive _R_: resume anything
_D_: dmoccur
"
  ("m" moccur)
  ("g" ~moccur-grep)
  ("f" ~moccur-grep-find)
  ("d" ~dmoccur-recursive)
  ("D" ~dmoccur)
  ("o" anything-c-moccur-occur-by-moccur)
  ("b" anything-c-moccur-buffer-list)
  ("k" anything-c-moccur-dmoccur)
  ("R" anything-c-moccur-resume))

(global-unset-key (kbd "M-s"))
(global-set-key
 (kbd "M-s")
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

(defhydra ~hydra-projectile-search (:exit t :hint nil :idle ~hydra-help-delay)
  "
_a_: ag                              _d_:     ag in dir                              _o_: occur
_A_: ag in other project             _D_:     ag in other project dir                _O_: occur in other project
_h_: ag by helm                      _C-d_:   ag by counsel in dir                   _g_: grep
_H_: ag by helm in other project     _C-S-d_: ag by counsel in dir in other project  _r_: hydra rails
_c_: ag by counsel
_C_: ag by counsel in other project
"
  ("a"     projectile-ag)
  ("A"     projectile-ag--in-other-project)
  ("h"     helm-projectile-ag)
  ("H"     helm-projectile-ag--in-other-project)
  ("c"     ~projectile-counsel-ag)
  ("C"     ~projectile-counsel-ag--in-other-project)
  ("d"     ~projectile-ag-with-directory-select)
  ("D"     ~projectile-ag-with-directory-select--in-other-project)
  ("C-d"   ~projectile-counsel-ag-with-directory-select)
  ("C-S-d" ~projectile-counsel-ag-with-directory-select--in-other-project)
  ("o"     projectile-multi-occur)
  ("O"     projectile-multi-occur--in-other-project)
  ("g"     projectile-grep)
  ("r"     ~hydra-projectile-search-rails/body))

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

(define-key
  projectile-mode-map
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
 (kbd "C-x p")
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
_x_: run                   _c_: compile     _s_: open shell buffer
_a_: async                 _I_: interpret
_p_: with region           _b_: background
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
_f_: find    _r_: regist oneshot              _t_: on/off
_i_: insert  _e_: expand oneshot
_n_: create  _E_: expand oneshot with region
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

(global-unset-key (kbd "M-'"))
(global-set-key
 (kbd "M-'")
 (defhydra ~hydra-browse (:exit t)
   "browse"
   ("g" ~browse-url "url")
   ("b" ~browse-bookmark "bookmark")
   ("s" ~eww-search-manualy "eww manualy")
   ("S" ~eww-search "eww")
   ("a" browse-url-at-point "at point")))


;;;;;;;;;;;;;;;;;;;;;
;; Version Control

(defhydra ~hydra-git-gutter ()
  "git gutter"
  ("t" git-gutter:toggle "toggle")
  ("s" git-gutter:popup-hunk "popup")
  ("p" git-gutter:previous-hunk "prev hunk")
  ("n" git-gutter:next-hunk "next hunk")
  ("S" git-gutter:stage-hunk "stage")
  ("r" git-gutter:revert-hunk "revert")
  ("q" nil "quit"))

(defhydra ~hydra-browse-github (:exit t)
  "browse github"
  ("f" github-browse-file)
  ("b" github-browse-file-blame))

(global-unset-key (kbd "M-v"))
(global-set-key
 (kbd "M-v")
 (defhydra ~hydra-vc (:exit t)
   "version control"
   ("m" ~vc-minor-mode "mode")
   ("g" ~hydra-git-gutter/body "hydra git gutter")
   ("b" ~hydra-browse-github/body "hydra browse github")
   ("l" counsel-git-log "log")))


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
 (defhydra ~hydra-bts (:exit t)
   "bts"
   ("n" bts:ticket-new "new ticket")
   ("s" bts:summary-open "open summary")
   ("p" ~hydra-bts-project/body "hydra bts project")
   ("q" ~hydra-bts-query/body "hydra bts query")))


;;;;;;;;;
;; Tag

(global-set-key
 (kbd "C-x t")
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


;;;;;;;;;;;;;;
;; Anything

;; abbrevのキーバインドをanything用に空ける
(dolist (s '("C-a" "'" "+" "-" "e" "g" "l" "n" "p" "i g" "i l"))
  (global-unset-key (kbd (concat "C-x a" s))))

(global-set-key
 (kbd "C-x a")
 (defhydra ~hydra-anything (:exit t :help nil :idle ~hydra-help-delay)
   "
_x_: M-x        _B_: bm                    _r_: resume
_f_: find file  _q_: replace string        _?_: help
_F_: for files  _h_: hatena bookmark
_a_: apropos    _H_: hatena bookmark dump
_b_: buffers
_k_: kill ring
_i_: imenu
_m_: mark ring
"
   ("x" anything-execute-anything-command)
   ("f" anything-find-files)
   ("F" anything-for-files)
   ("a" anything-apropos)
   ("b" anything-buffers-list)
   ("k" anything-show-kill-ring)
   ("i" anything-imenu)
   ("m" anything-all-mark-rings)
   ("B" anything-bm-list)
   ("q" anything-replace-string)
   ("h" anything-hatena-bookmark)
   ("H" anything-hatena-bookmark-get-dump)
   ("r" anything-resume)
   ("?" anything-help)))


;;;;;;;;;;;;;
;; Counsel

(global-unset-key (kbd "C-x i"))
(global-set-key
 (kbd "C-x i")
 (defhydra ~hydra-counsel (:exit t :help nil :idle ~hydra-help-delay)
   "
_x_: M-x           _g_: git grep  _b_: descbinds          _r_: resume
_f_: find file     _a_: ag        _d_: describe function
_i_: imenu                        _D_: describe variable
_y_: yank pop
_l_: load library
_L_: locate
"
   ("x" counsel-M-x)
   ("f" counsel-find-file)
   ("i" counsel-imenu)
   ("y" counsel-yank-pop)
   ("l" counsel-load-library)
   ("L" counsel-locate)
   ("g" counsel-git-grep)
   ("a" counsel-ag)
   ("b" counsel-descbinds)
   ("d" counsel-describe-function)
   ("D" counsel-describe-variable)
   ("r" ivy-resume)))

