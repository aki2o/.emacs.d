
(require 'bts)
(require 'elmine)

(defgroup bts-redmine nil
  "Pop-up the hint tip of candidates for doing something"
  :group 'bts
  :prefix "bts-redmine:")


(defvar bts-redmine::current-host-url nil)
(defvar bts-redmine::current-api-key nil)

(defsubst bts-redmine::api-get (target &rest params)
  (bts--trace "start api-get. target:%s params:%s" target params)
  (if (or (not bts-redmine::current-host-url)
          (not bts-redmine::current-api-key))
      (bts--fatal "Invalid setting for Rest API : url[%s] api-key[%s]"
                    bts-redmine::current-host-url bts-redmine::current-api-key)
    (let* ((redmine-host bts-redmine::current-host-url)
           (redmine-api-key bts-redmine::current-api-key)
           (res (elmine/api-get nil (format "/%s.json" target) params)))
      (if (not res)
          (bts--warn "Rest API response is not exists : host:[%s] target:[%s]" redmine-host target)
        res))))

(defun bts-redmine::api-get-projects ()
  "Example of returned value:
 ((:name \"MyProject\"
   :identifier \"myproj\"
   :id 2
   :updated_on \"2012-02-05T00:11:41Z\"
   :description \"\"
   :created_on \"2012-02-04T08:13:55Z\")
  (...))"
  (let ((res (bts-redmine::api-get "projects")))
    (when res (plist-get res :projects))))



(defun bts-redmine:regist-project ()
  (let* ((bts-redmine::current-host-url (read-string "Redmine URL: " ))
         (bts-redmine::current-api-key (read-string "API Key: "))
         (proj-defs (bts-redmine::api-get-projects))
         (projnm-list (loop for e in proj-defs
                            collect (plist-get e :name)))
         (projnm (completing-read "Project Name: " projnm-list nil t nil '()))
         (projid (loop for e in proj-defs
                       if (string= (plist-get e :name) projnm)
                       return (plist-get e :id))))
    `(:url ,bts-redmine::current-host-url
      :api-key ,bts-redmine::current-api-key
      :projid ,projid)))


(defstruct bts-redmine:host name url api-key)

(defvar bts-redmine::host-hash (make-hash-table :test 'equal))

(defun bts-redmine:regist-host (host)
  ""
  (when (not (bts-redmine:host-p host))
    (error ""))
  (let ((hostnm (bts-redmine:host-name host)))
    (when (or (not (stringp hostnm))
              (not (string-match "\\`[^ \t\r\n]+\\'" hostnm)))
      (error ""))
    (puthash hostnm host bts-redmine::host-hash)))

;; (let ((redmine-host "http://redmine.aki-docoro.net")
;;       (redmine-api-key (~auth-source-get-property 'aki-key :app "my-redmine")))
;;   (elmine/api-get nil "/projects.json"))
;;   ;; (elmine/get-issues))




(defun bts-redmine:make-project-view (project)
  "Function for `bts:system-project-view'."
  (let* ((lo `((:type text :name url :label "URL" :size 40 :require t :leave on-update)
               BR (:type text :name apikey :label "API-Key" :size 40 :require t :leave on-update)
               BR ))
         (defs))
    ))



(bts:system-regist
 (make-bts:system
  :name                 'redmine
  :project-view         'bts-redmine:make-project-view
  :query-view           'bts-redmine:make-query-view
  :ticket-single-view   'bts-redmine:make-ticket-single-view
  :ticket-fetcher       'bts-redmine:fetch-issue
  :ticket-register      'bts-redmine:regist-issue
  :ticket-unique-string 'bts-redmine:make-ticket-unique-string
  :ticket-latest        'bts-redmine:fetch-latest-issue
  :summary-format       'bts-redmine:make-summary-format
  :conflict-checker     'any))


(provide 'bts-redmine)
;;; bts-redmine.el ends here
