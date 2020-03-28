;;----------------------------------------------------------------------------------
;;get_from_web function.
;;Conect to a url as a API service and get the response
;;Params:
;;       -str_url (string): Url of API service
;;       -lst_headers (string): list of lists of the form (("HeaderParam" "ValueOfHeaderParam"))
;;       -str_method (string): HTTP methods 'GET', 'POST', 'PUT', etc
;;       -str_post_data: Data to send to the server (json text, for example)
;;
;;For example:
;;In an DRF tokenized authentication -> GET things from server:
;;  get_from_web("http://urlofservice/" "GET" nil (("Authorization" "Token YourUserToken")))
;;
;;In an DRF tokenized authentication -> POST json to the server:
;;  get_from_web(
;;      "http://urlofservice/"
;;      "POST"
;;      "{tag: value, tag1: value1}"
;;      (("Authorization" "Token YourUserToken") ("Content-Type" "application/json")))
;;
;;RETURN: A list of dotted lists:
;;        (
;;          ('Status . "Status Code")
;;          ('StatusText . "Status Text")
;;          ('ResponseText . "response_text")
;;          ('ResponseAnsiText . "response_text")
;;          ('ErrorValue . "err_obj")
;;        )
;;If 'ErrorValue == nil -> The server response some valid thing
;;Else 'ErrorValue contain the error
;;
;; Original Code posted by user: BazzaCAD, 2010/03/29, from site:
;; http://opendcl.com/forum/index.php?topic=1244.0
;; Obtained From https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/make-an-api-call-with-autolisp-visuallisp/td-p/9083202
;; Posted by CodeDing, 2019/12/10
;;
;; Modified by (me ;-) ) diegomcas@gmail.com 2020/03/08
;;----------------------------------------------------------------------------------
(defun get_from_web (str_url str_method str_post_data lst_headers
                     / web_obj server_response err_obj response_text)

  (if (not str_method)
    (setq str_method "GET")
  )

  ;; Create a new reference to the WinHttp object
  (setq web_obj
    (vlax-invoke-method
      (vlax-get-acad-object)
      'GetInterfaceObject
      "WinHttp.WinHttpRequest.5.1"
    )
  )

  ;; Fetch web page
  (vlax-invoke-method web_obj 'Open str_method str_url :vlax-false)

  ;; Add/replace a request header.
  (foreach header_tup lst_headers
    (vlax-invoke-method web_obj 'SetRequestHeader (car header_tup) (cadr header_tup))
  )

  ;; Sending request
  (if str_post_data
    (setq err_obj (vl-catch-all-apply 'vlax-invoke-method (list web_obj 'Send str_post_data)))
    (setq err_obj (vl-catch-all-apply 'vlax-invoke-method (list web_obj 'Send)))
  )

  (if (null (vl-catch-all-error-p err_obj))
    (progn
      ;; Applying corrections to characters utf codes
      (setq response_text
        (vl-list->string
          (list_latin_utf_to_ascii
            (vlax-safearray->list
              (vlax-variant-value
                (vlax-get-property web_obj 'ResponseBody)
              )
            )
          )
        )
      )

      (setq server_response
        (list
          (cons 'Status (vlax-get-property web_obj 'Status))
          (cons 'StatusText (vlax-get-property web_obj 'StatusText))
          (cons 'ResponseText (vlax-get-property web_obj 'ResponseText))
          (cons 'ResponseAnsiText response_text)
          (cons 'Content-Type (vlax-invoke-method web_obj 'GetResponseHeader "Content-Type"))
          (cons 'ErrorValue nil)
        )
      )
    )
    (progn
      (setq server_response
        (list
          (cons 'Status nil)
          (cons 'StatusText nil)
          (cons 'ResponseText nil)
          (cons 'ResponseAnsiText nil)
          (cons 'Content-Type nil)
          (cons 'ErrorValue (vl-catch-all-error-message err_obj))
        )
      )
    )
  )
  server_response 
)

;;----------------------------------------------------------------------------------
;;list_latin_utf_to_ascii function.
;;From a list of characters integer values return a new list with the utfs codes translates
;;Params:
;;       -lst_code: The list with the original codes
;;
;;RETURN: A list with the ansi latin codes.
;;
;;Original Code by: diegomcas, 2020/03/25
;;----------------------------------------------------------------------------------
(defun list_latin_utf_to_ascii (lst_code / lst_translate cnt code_ext new_code)
  ; lst_code = Lista con los codigos del string
  (setq cnt 0)
  (while (< cnt (length lst_code))
    (setq code (nth cnt lst_code))
    (if (> code 127) ;Wait a new integer code
      (progn
        (setq cnt (1+ cnt))
        (setq code_ext (nth cnt lst_code))
        (if (eq 194 code) ;Without displacement, otherwise displacement +64
          (setq new_code code_ext)
          (setq new_code (+ code_ext 64))
        )
      )
      (progn
        (setq new_code code)
      )
    )
    (setq lst_translate (append lst_translate (list new_code)))
    (setq cnt (1+ cnt))
  )
  lst_translate
)

;;----------------------------------------------------------------------------------
;;get_DRF_token function.
;;Only this :-) Get the DRF (Django Rest Full) Token. Use the (get_from_web) function
;;Params:
;;       -url_to_api-token-auth: The url to get the token
;;       -username: The Username
;;       -password: The Password
;;
;;RETURN: A list with the complete response of the function (get_from_web) (car element)
;;        and the TOKEN (cadr element)
;;
;;Original Code by: diegomcas, 2020/03/25
;;----------------------------------------------------------------------------------
(defun get_DRF_token(url_to_api-token-auth username password / response_list token)
  (setq response_list
    (get_from_web url_to_api-token-auth
      "POST"
      (list_to_json
        (list
          (list "username" username)
          (list "password" password)
        )
      )
      (list (list "Content-Type" "application/json"))
    )
  )
  
  (if (null (cdr (assoc 'ErrorValue response_list)))
    (progn
      (if (equal (cdr (assoc 'Status response_list)) 200)
        (progn
          (setq token (cadr (car (json_to_list (cdr (assoc 'ResponseText response_list))))))
         )
      )
    )
  )

  (list response_list token)
)

