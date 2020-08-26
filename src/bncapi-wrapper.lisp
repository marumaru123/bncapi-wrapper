(in-package :cl-user)
(defpackage bncapi-wrapper
  (:use :cl
        :drakma
        :ironclad
        :cl-json)
  (:export :exchange-info :order-book :new-order :cancel-order :cancel-all-open-orders :query-order :account-information :account-trade-list :trade-fee))
(in-package :bncapi-wrapper)

(defparameter *endpoint-url*     "https://api.binance.com")
;(defparameter *endpoint-url*     "https://testnet.binance.vision")
(defparameter *api-content-type* "application/json")

(defun hex (bytes)
  (ironclad:byte-array-to-hex-string bytes))

(defun hmac_sha256 (secret text)
  (let ((hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array secret) :sha256)))
    (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array text))
    (ironclad:hmac-digest hmac)))

(defun get-timestamp ()
  (multiple-value-bind (time ms) (sb-unix::system-real-time-values)
    (let* ((str-time (princ-to-string time))
           (str-ms   (princ-to-string ms))
           (len-ms   (length str-ms)))
      (if (> 3 len-ms)
        (parse-integer (concatenate 'string str-time (format nil "~V@{~A~:*~}" (- 3 len-ms) "0") str-ms))
        (parse-integer (concatenate 'string str-time str-ms))))))

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

(defun delete-private-api (key path body)
  (let* ((drakma:*drakma-default-external-format* :utf-8)
	 (drakma:*text-content-types* '(("application" . "json")))
	 (url           (concatenate 'string *endpoint-url* path))
	 (extra-headers (create-extra-headers key)))
    (drakma:http-request url
                         :user-agent          :explorer
	                 :method              :delete
	                 :content-type        *api-content-type*
	                 :external-format-out :utf-8
                         :external-format-in  :utf-8
                         :content             body
                         :additional-headers  extra-headers)))

(defun exchange-info ()
  (get-public-api "/api/v3/exchangeInfo"))

(defun order-book (symbol)
  (let ((path (concatenate 'string "/api/v3/depth?symbol=" symbol)))
    (get-public-api path)))

(defun new-order (key secret symbol side type quantity price &optional (timeInForce "GTC") (recvWindow 5000))
  (let* ((timestamp    (get-timestamp))
         (query-string1 (concatenate 'string "symbol=" symbol "&side=" side "&type=" type))
	 (query-string2 (if (string= type "MARKET")
			  (concatenate 'string "&quantity=" (princ-to-string quantity))
			  (concatenate 'string "&timeInForce=" timeInForce "&quantity=" (princ-to-string quantity) "&price=" (princ-to-string price))))
	 (query-string3 (concatenate 'string "&recvWindow=" (princ-to-string recvWindow) "&timestamp=" (princ-to-string timestamp)))
	 (query-string (concatenate 'string query-string1 query-string2 query-string3))
	 (signature (hex (hmac_sha256 secret query-string)))
	 (body      (concatenate 'string query-string "&signature=" signature)))
    (print query-string)
    (post-private-api key "/api/v3/order" body)))

(defun cancel-order (key secret symbol order-id &optional (recvWindow 5000))
  (let* ((timestamp    (get-timestamp))
         (query-string (concatenate 'string "symbol=" symbol "&orderId=" (princ-to-string order-id) "&recvWindow=" (princ-to-string recvWindow) "&timestamp=" (princ-to-string timestamp)))
	 (signature (hex (hmac_sha256 secret query-string)))
	 (body      (concatenate 'string query-string "&signature=" signature)))
    (delete-private-api key "/api/v3/order" body)))

(defun cancel-all-open-orders (key secret symbol &optional (recvWindow 5000))
  (let* ((timestamp    (get-timestamp))
         (query-string (concatenate 'string "symbol=" symbol "&recvWindow=" (princ-to-string recvWindow) "&timestamp=" (princ-to-string timestamp)))
	 (signature (hex (hmac_sha256 secret query-string)))
	 (body      (concatenate 'string query-string "&signature=" signature)))
    (delete-private-api key "/api/v3/openOrders" body)))

(defun query-order (key secret symbol order-id &optional (recvWindow 5000))
  (let* ((timestamp    (get-timestamp))
         (query-string (concatenate 'string "symbol=" symbol "&orderId=" (princ-to-string order-id) "&recvWindow=" (princ-to-string recvWindow) "&timestamp=" (princ-to-string timestamp)))
	 (signature (hex (hmac_sha256 secret query-string)))
	 (parameter (concatenate 'string "?" query-string "&signature=" signature))
	 (url       (concatenate 'string "/api/v3/order" parameter)))
    (get-private-api key url)))

(defun account-information (key secret &optional (recvWindow 5000))
  (let* ((timestamp    (get-timestamp))
         (query-string (concatenate 'string "recvWindow=" (princ-to-string recvWindow) "&timestamp=" (princ-to-string timestamp)))
	 (signature (hex (hmac_sha256 secret query-string)))
	 (parameter (concatenate 'string "?" query-string "&signature=" signature))
	 (url       (concatenate 'string "/api/v3/account" parameter)))
    (get-private-api key url)))

(defun account-trade-list (key secret symbol &optional (from-id nil from-id-supplied-p) (limit nil limit-supplied-p)(recvWindow 5000))
  (let* ((timestamp    (get-timestamp))
         (query-string1 (concatenate 'string "symbol=" symbol))
	 (from-id-qs (if from-id-supplied-p (concatenate 'string "&fromId=" (princ-to-string from-id)) ""))
	 (limit-qs   (if from-id-supplied-p (concatenate 'string "&limit="  (princ-to-string limit)) ""))
         (query-string3 (concatenate 'string "&recvWindow=" (princ-to-string recvWindow) "&timestamp=" (princ-to-string timestamp))) 
         (query-string  (concatenate 'string query-string1 from-id-qs limit-qs query-string3))
	 (signature (hex (hmac_sha256 secret query-string)))
	 (parameter (concatenate 'string "?" query-string "&signature=" signature))
	 (url       (concatenate 'string "/api/v3/myTrades" parameter)))
    (get-private-api key url)))

(defun trade-fee (key secret)
  (let* ((timestamp    (get-timestamp))
         (query-string (concatenate 'string "timestamp=" (princ-to-string timestamp)))
	 (signature (hex (hmac_sha256 secret query-string)))
	 (path      (concatenate 'string "/wapi/v3/tradeFee.html?" query-string "&signature=" signature)))
    (get-private-api key path)))
