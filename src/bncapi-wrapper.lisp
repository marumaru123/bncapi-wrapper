(in-package :cl-user)
(defpackage bncapi-wrapper
  (:use :cl
        :drakma
        :ironclad
        :cl-json)
  (:export :order-book :trade-fee))
(in-package :bncapi-wrapper)

(defparameter *endpoint-url*     "https://api.binance.com")
(defparameter *api-content-type* "application/json")

(defun hex (bytes)
  (ironclad:byte-array-to-hex-string bytes))

(defun hmac_sha256 (secret text)
  (let ((hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array secret) :sha256)))
    (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array text))
    (ironclad:hmac-digest hmac)))

(defun get-timestamp ()
  (multiple-value-bind (time1 ms1) (sb-unix::system-real-time-values)
    (parse-integer (concatenate 'string (princ-to-string time1) (princ-to-string ms1)))))

(defun create-extra-headers (key)
  (list (cons "X-MBX-APIKEY" key)
        (cons "Content-Type" *api-content-type*)))

(defun get-public-api (path)
  (let* ((drakma:*drakma-default-external-format* :utf-8)
	 (drakma:*text-content-types* '(("application" . "json")))
	 (url           (concatenate 'string *endpoint-url* path))
         (extra-headers (list (cons "Content-Type" *api-content-type*))))
    (drakma:http-request url
                         :user-agent          :explorer
	                 :method              :get
	                 :content-type        *api-content-type*
	                 :external-format-out :utf-8
                         :external-format-in  :utf-8
                         :additional-headers  extra-headers)))

(defun get-private-api (key path)
  (let* ((drakma:*drakma-default-external-format* :utf-8)
	 (drakma:*text-content-types* '(("application" . "json")))
	 (url           (concatenate 'string *endpoint-url* path))
	 (extra-headers (create-extra-headers key)))
    (drakma:http-request url
                         :user-agent          :explorer
	                 :method              :get
	                 :content-type        *api-content-type*
	                 :external-format-out :utf-8
                         :external-format-in  :utf-8
                         :additional-headers  extra-headers)))

(defun post-private-api (key path body)
  (let* ((drakma:*drakma-default-external-format* :utf-8)
	 (drakma:*text-content-types* '(("application" . "json")))
	 (url           (concatenate 'string *endpoint-url* path))
	 (extra-headers (create-extra-headers key)))
    (drakma:http-request url
                         :user-agent          :explorer
	                 :method              :post
	                 :content-type        *api-content-type*
	                 :external-format-out :utf-8
                         :external-format-in  :utf-8
                         :content             body
                         :additional-headers  extra-headers)))

(defun order-book (symbol)
  (let ((path (concatenate 'string "/api/v3/depth?symbol=" symbol)))
    (get-public-api path)))

(defun new-order (key secret symbol side type quantity price)
  (let ((timestamp    (get-timestamp))
	(query-string (concatenate 'string "symbol=" symbol "&side=" side "&type=" type "&timeInForce=GTC&quantity=" quantity "&price=" price "&recvWindow=5000&timestamp=" (princ-to-string timestamp)))
	(signature (hex (hmac_sha256 secret query-string)))
	(body      (concatenate 'string query-string "&signature=" signature)))
    (post-private-api key "/api/v3/order" body)))

(defun trade-fee (key secret)
  (let* ((timestamp    (get-timestamp))
         (query-string (concatenate 'string "timestamp=" (princ-to-string timestamp)))
	 (signature (hex (hmac_sha256 secret query-string)))
	 (path      (concatenate 'string "/wapi/v3/tradeFee.html?" query-string "&signature=" signature)))
    (get-private-api key path)))

