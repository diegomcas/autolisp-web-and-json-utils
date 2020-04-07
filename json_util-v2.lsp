;;-----------------------------------------------------------------------------------------
;;str_replace function.
;;Replace all aparisions of "patt" by "repl_to"
;;
;;Params:
;;  -str: The original string
;;  -patt: The pattern which will be replaced
;;  -repl_to: The string which will replace the pattern
;;
;;Original Code by: diegomcas, 2020/03/25
;-----------------------------------------------------------------------------------------
(defun str_replace(str patt repl_to / pos inc init_search)

  (if (not str) (setq str ""))
  (if (and (not patt) (< (strlen patt) 1)) (setq patt " "))
  (if (and (not repl_to) (< (strlen repl_to) 1)) (setq repl_to " "))

  (setq pos (vl-string-search patt str))
  (setq inc (1+ (- (strlen repl_to) (strlen patt))))
  (princ "pos= ") (princ pos) (princ "\n")
  (princ "inc= ") (princ inc) (princ "\n")
  (while pos
    (setq str (vl-string-subst repl_to patt str pos))
    (setq init_search (+ pos inc))
    (if (< init_search 0)
      (setq init_search 0)
    )
    (setq pos (vl-string-search patt str init_search))
  )
  str
)

;;-----------------------------------------------------------------------------------------
;;json_parser function (To be executed by json_to_list)
;;Make a list of list with all data of the json string.
;;One list for each of:
;;  -pairs name:value
;;  -arrays
;;  -objets
;;
;;Params:
;;  -lst_json: The list prepared by (json_to_list) function
;;  -state:  for the use of the States Machine
;;
;;Original Code by: diegomcas, 2020/03/25
;;-----------------------------------------------------------------------------------------
(defun json_parser(lst_json state array_lvl quote_array / lst str_name res)

  (setq lst '())
  (setq lst_pair '())

  (if (not state)
    (setq state 'Obj)
  )
  
  (if (not array_lvl)
    (setq array_lvl 0)
  )

  (foreach res lst_json
    ; (princ "res= ") (princ res) (princ "\n")
    ; (princ "state= ") (princ state) (princ "\n")
    ; (princ "str_name= ") (princ str_name) (princ "\n")
    ; (princ "array_lvl= ") (princ array_lvl) (princ "\n")
    ; (princ "--------------------------------------------") (princ "\n")

    (cond
      ;Case -> reading '<ARRAY>
      ((eq res (quote <ARRAY>))
        (setq array_lvl (1+ array_lvl))
        (setq state 'Array)
        (if quote_array
          (setq lst (append lst (list res)))
        )
      )
      
      ;Case -> reading '</ARRAY>
      ((eq res (quote </ARRAY>))
        (setq array_lvl (1- array_lvl))
        (setq state 'Obj)
        (if quote_array
          (setq lst (append lst (list res)))
        )
      )

      ;Case -> { null/true/false }
      ((and (eq state 'Obj) (or (eq res 'null) (eq res 'false) (eq res 'true)))
        (setq lst (append lst (list res)))
      )

      ;Case -> {name : value} / value = 'null or 'false or 'true or 'STR or 'INT or 'REAL
      ((and (eq state 'Obj_Value) (or (eq res 'null) (eq res 'false) (eq res 'true) (eq (type res) 'STR) (eq (type res) 'INT) (eq (type res) 'REAL)))
        (if (eq 'STR (type res))
          (progn
            (setq res (str_replace res " , " "," ))
            (setq res (str_replace res " : " ":" ))
            (setq res (str_replace res "( <ARRAY> "  "["))
            (setq res (str_replace res " </ARRAY> )"  "]"))
          )
        )
        (setq lst_pair (list str_name res))
        (setq lst (append lst (list lst_pair)))
        (setq str_name nil)
      )
      
      ;Case -> finish reading VALUE
      ((and (eq state 'Obj_Value) (eq res ',))
        (setq state 'Obj)
      )
      
      ;Case -> Reading a name {NAME : value}
      ((and (eq state 'Obj) (eq (type res) 'STR))
        (setq res (str_replace res " , " "," ))
        (setq res (str_replace res " : " ":" ))
        (setq res (str_replace res "( <ARRAY> "  "["))
        (setq res (str_replace res " </ARRAY> )"  "]"))

        (setq str_name res)
        (setq state 'Obj_Name)
      )
      
      ;Case -> Reading a new Object
      ((and (eq state 'Obj) (eq (type res) 'LIST))
        (setq lst_temp (json_parser res state array_lvl quote_array))
        (setq lst_pair (append str_name lst_temp))
        (setq lst (append lst (list lst_pair)))
      )
      
      ;Case -> Reading a value = {Object}
      ((and (eq state 'Obj_Value) (eq (type res) 'LIST))
        (setq lst_temp (json_parser res 'Obj array_lvl quote_array))
        (if str_name
          (setq lst_pair (list str_name lst_temp))
          (setq lst_pair lst_temp)
        )
        
        (setq lst (append lst (list lst_pair)))
      )
      
      ;Case -> Reading a complete name {NAME : value} -> Preparing to read value
      ((and (eq state 'Obj_Name) (eq res ':))
        (setq state 'Obj_Value)
      )
      
      ;Case -> Reading an Array
      ((and (eq state 'Array) (eq (type res) 'LIST))
        (setq lst_temp (json_parser res nil array_lvl quote_array))
        (setq lst_pair (append str_name lst_temp))
        (setq lst (append lst (list lst_pair)))
      )

      ;Case -> ARRAY value = 'null or 'false or 'true or 'STR or 'INT or 'REAL without name
      ((and (and (> array_lvl 0) (eq state 'Array)) (or (eq res 'null) (eq res 'false) (eq res 'true) (eq (type res) 'STR) (eq (type res) 'INT) (eq (type res) 'REAL)))
        (princ res) (princ "\n")
        (setq lst (append lst (list res)))
      )

    )
  )
  lst
)

;;-----------------------------------------------------------------------------------------
;;json_to_list function
;;Make a list of list with all data of the json string.
;;One list for each of:
;;  -pairs name:value
;;  -arrays
;;  -objets
;;
;;Params:
;;  -json: The json string (Strings values must not contain "(" ")")
;;
;;Original Code by: diegomcas, 2020/03/25
;;-----------------------------------------------------------------------------------------
(defun json_to_list(json quote_array / strtransf stread)
  (if (not (eq 'STR (type json)))
    (setq json "{}")
  )

  ;Lists of lists for registers/arrays/objects of the json
  (setq strtransf (vl-string-translate "{}" "()" json))
  (setq strtransf (str_replace strtransf "[" "( <ARRAY> "))
  (setq strtransf (str_replace strtransf "]" " </ARRAY> )"))
  ;add spaces before "," / Repairing bad read of numbers
  (setq strtransf (str_replace strtransf "," " , "))
  (setq strtransf (str_replace strtransf ":" " : "))

  (setq stread (read strtransf))
  ;(princ "json -> ")(princ json) (princ "\n")
  ;(princ "stread -> ")(princ stread) (princ "\n")

  (json_parser stread nil 0 quote_array)
)

;;-----------------------------------------------------------------------------------------
;;list_to_json function
;;Make a json string with all data of the json list.
;;
;;Not complete testing!!! Testing for use it!!!
;;Params:
;;  -lst: The list of which will be written the json
;;
;;Autolisp don't have Arrays, so it is impossible to rebuild a json that contains them.
;;This function assume the list is Array Quote
;;
;;If you run (list_to_json (json_to_list "{your_json}"))
;;you probably lose data (type of data)
;;
;;Original Code by: diegomcas, 2020/03/25
;;-----------------------------------------------------------------------------------------
(defun list_to_json(lst / lst_element json reading_val init_state)

  ; (defun is_object(lst / )
    ; (and (eq (length lst) 2) (eq 'STR (type (car lst))) (not (is_array lst)))
  ; )
  
  (defun value_to_string (element / )
    (cond
      ((eq 'STR (type element))
        (strcat "\"" element "\"")
      )
      ((eq 'REAL (type element))
        (rtos element 2 8)
      )
      ((eq 'INT (type element))
        (itoa element)
      )
      ((eq 'NULL element)
        "null"
      )
      ((eq 'FALSE element)
        "false"
      )
      ((eq 'TRUE element)
        "true"
      )
    )
  )
  
  (defun is_attribute (lst / )
    (and (eq (length lst) 2) (eq 'STR (type (car lst))) (not (is_array lst)))
  )
  
  (defun is_array (lst / res)
    (eq (QUOTE <ARRAY>) (car lst))
  )

  (defun read_list (lst json state / )
    
    (foreach lst_element lst
      ; (princ "---------------------------------------------------------\n")
      ; (princ "IN state    -> ") (princ state) (princ "\n")
      ; (princ "lst_element -> ") (princ lst_element) (princ "\n")
      ; (princ "reading_val -> ") (princ reading_val) (princ "\n")
      
      (if (eq 'LIST (type lst_element)) ; Set of data
        (progn
          (cond
            ; ARRAY -> Read any value (Objects, Arrays, strings, numbers, 'true, 'false, 'null)
            ((is_array lst_element)
              (setq reading_val nil)
              (setq json (strcat json "["))
              (setq json (read_list lst_element json 'ARRAY))
              (setq json (strcat json "],"))
            )
            ; ATTRIBUTE IN AN OBJECT
            ((is_attribute lst_element)
              (setq reading_val nil)
              (setq json (read_list lst_element json 'ATTRIB))
            )
            ; NO ARRAY / NO ATTRIBUTE -> Read Object
            (T
              (setq reading_val nil)
              (setq json (strcat json "{"))
              (setq json (read_list lst_element json 'OBJECT))
              (setq json (strcat json "},"))
            )
          )
        )
        (progn ;not is list / is name or value
          (if (not (or (eq lst_element '<ARRAY>) (eq lst_element '</ARRAY>)))
            (cond
              ((eq state 'ARRAY)
                (setq json (strcat json (value_to_string lst_element) ","))
              )
              ((eq state 'ATTRIB)
                (if reading_val
                  (progn
                    (setq json (strcat json (value_to_string lst_element) ","))
                    (setq reading_val nil)
                  )
                  (progn
                    (setq json (strcat json (value_to_string lst_element) ":"))
                    (setq reading_val T)
                  )
                )
              )
              ((eq state 'OBJECT)
              )
            )
          )
        )
      )
      ; (princ "OUT state   -> ") (princ state) (princ "\n")
      ; (princ "JSON        -> ") (princ json) (princ "\n")
      ; (princ "---------------------------------------------------------\n")
    )

    json
  )
  
  (if (is_array lst)
    (progn
      (setq init_state 'ARRAY)
      (setq init_string "[")
    )
    (progn
      (setq init_state 'OBJECT)
      (setq init_string "{")
    )
  )
  
  (setq json (read_list lst init_string init_state))

  (if (eq "[" init_string)
    (setq json (strcat json "]"))
    (setq json (strcat json "}"))
  )
  
  ;Clean the string
  (setq json (str_replace json ",}" "}"))
  (setq json (str_replace json ",]" "]"))
)
