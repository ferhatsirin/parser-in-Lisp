(defvar *tree*)
(defvar *input*)
(defvar *token*)
(defvar *lexeme*)
(defvar *current*)

(defun parser (tokens)
  (setf *input* tokens)
  (parseInput))

(defun parseInput ()
  (if *input*
      (progn
        (setf *tree* nil)
        (setf *tree* (createTree))
        (push "input" *tree*)
        (push "start" *tree*) 
        (printTree)
        (parseInput))))

(defun createTree ()
  (let ((subList nil))
    (nextToken)
    (cond
      ((compare "identifier")
       (setf subList (list "id" *lexeme*))
       (push "EXPI" subList))
      ((compare "integer")
       (setf subList (list *token* *lexeme*))
       (push "EXPI" subList))
      ((compare "binaryValue")
       (setf subList (list *token* *lexeme*))
       (push "EXPB" subList))
      ((compare "operator" "'")
       (nextToken)
       (if (compare "operator" "(")
           (progn
             (setf subList (takeListValues nil))
             (nextToken)
             (if (not (compare "operator" ")"))
                 (error "Missing operator \")\" after ~a " subList))
             (if (not subList)
                 (push "'()" subList))
             (push "LISTVALUE" subList)
             (push "EXPLIST" subList))
           (progn
             (error "Missing operator \"(\" after operator ' "))))
      ((compare "keyword" "null")
       (push "null" subList)
       (push "LISTVALUE" subList)
       (push "EXPLIST" subList))
      ((compare "operator"  "(")
       (setf subList (determineExp))
       (nextToken)
       (if (not (compare "operator" ")"))
           (error "Missing operator \")\" after ~a " subList)))
      (t
       (error "Undefined argument ~a " *current* )))
    subList))

(defun determineExp ()
  (let ((mainList))
    (setf mainList (takeEXPI))
    (if (not mainList)
        (setf mainList (takeEXPB)))
    (if (not mainList)
          (setf mainList (takeEXPLIST)))
    (if (not mainList)
        (error "Undefined argument ~a " *input*))

    mainList))

(defun takeEXPI ()
  (let ((tempList) (subList) (mainList) (operator))
    (nextToken)
    (cond
      ((compare "operator" "+" "-" "/" "*") 
       (setf operator *lexeme*)
       (push (takeEXPITerm) subList) 
       (push (takeEXPITerm) subList)
       (nreverse subList)
       (push operator subList))
      ((compare "identifier")
       (undoToken)
       (push (takeEXPITerm) subList)
       (setf tempList (takeListExp nil))
       (if (not tempList)
           (progn
             (push "()" tempList)))
       (push "EXPLIST" tempList)
       (setf subList (append (reverse tempList) subList))
       (nreverse subList)
       (push "EXPI" subList))
      ((compare "keyword" "set" "defvar")
       (setf operator *lexeme*)
       (push (takeEXPITerm) subList)
       (if (not (compare "identifier"))
           (error "\"~a\" should take id first " operator))
       (push (takeEXPITerm) subList)
       (nreverse subList)
       (push operator subList))
      ((compare "keyword" "if")
       (setf operator *lexeme*)
       (push (takeEXPBTerm) subList)
       (push (createTree) subList)
       (nextToken)
       (if (compare "operator" ")")
           (progn 
             (undoToken))
           (progn
             (undoToken)
             (push (createTree) subList)))
       (nreverse subList)
       (push operator subList))
      ((compare "keyword" "while")
       (setf operator *lexeme*)
       (nextToken)
       (if (compare "operator" "(")
           (progn
             (push (takeEXPBTerm) subList)
             (nextToken)
             (if (compare "operator" ")")
                 (progn
                   (setf tempList (takeListExp nil))
                   (if (not tempList)
                       (progn
                         (push "()" tempList)))
                   (push "EXPLIST" tempList)
                   (setf subList (append (reverse tempList) subList))
                   (nreverse subList)
                   (push operator subList))
                 (progn
                   (error "Missing operator \"(\" after ~a " subList))))
           (progn
             (error "Missing operator \"(\" after while" ))))
      ((compare "keyword" "for")
       (setf operator *lexeme*)
       (nextToken)
       (if (compare "operator" "(")
           (progn
             (push (takeEXPITerm) subList)
             (if (not (compare "identifier"))
                 (error "\"~a\" should take id first " operator))
             (push (takeEXPITerm) subList)
             (push (takeEXPITerm) subList)
             (nextToken)
             (if (compare "operator" ")")
                 (progn
                   (setf tempList (takeListExp nil))
                   (if (not tempList)
                       (progn
                         (push "()" tempList)))
                   (push "EXPLIST" tempList)
                   (setf subList (append (reverse tempList) subList))
                   (nreverse subList)
                   (push operator subList))
                 (progn
                   (error "Missing operator \")\" after ~a " subList))))
           (progn
             (error "Missing operator \"(\" after for "))))
      ((compare "keyword" "deffun")
       (setf operator *lexeme*)
       (push (takeEXPITERM) subList)
       (if (not (compare "identifier"))
           (error "\"~a\" should take id first " operator))
       (nextToken)
       (if (compare "operator" "(")
           (progn
             (setf tempList (takeIDList nil))
             (if (not tempList)
                 (progn
                   (push "()" tempList)))
             (push "IDLIST" tempList)
             (push tempList subList)
             (nextToken)
             (if (not (compare "operator" ")"))
                 (error "Missing operator \")\" after ~a " subList))

             (setf tempList (takeListExp nil))
             (if (not tempList)
                 (progn
                   (push "()" tempList)))
             (push "EXPLIST" subList)
             (setf subList (append (reverse tempList) subList))
             (nreverse subList)
             (push operator subList))
           (progn
             (error "Missing operator \"(\" after deffum ~a " subList))))
       (t
        (undoToken)))
           
    (if subList
        (progn
          (push subList mainList)
          (push "EXPI" mainList)))
    mainList))

(defun takeEXPB ()
  (let ((tempList) (subList) (mainList) (operator))
    (nextToken)
    (if (compare "keyword" "and" "or" "not" "equal")
        (progn
          (setf operator *lexeme*)
          (setf tempList (takeEXPBTerm))
          (if tempList
              (progn
                (push tempList subList)
                (if (string-not-equal operator "not")
                    (push (takeEXPBTerm) subList)))
              (progn
                (if (string-equal operator "equal")
                    (progn
                      (if (compare "operator" )
                          (push '("operator" "(") *input*))
                      (push (takeEXPITerm) subList)
                      (push (takeEXPITerm) subList))
                    (progn
                      (error "EXPB should contain only EXPB argument, only exception is \"equal\"")))))
          (nreverse subList)
          (push operator subList)
          (push subList mainList)
          (push "EXPB" mainList))
        (progn
          (undoToken)))
    mainList))

(defun takeEXPLIST ()
  (let ((subList) (mainList) (operator))
    (nextToken)
    (if (compare "keyword" "concat" "append")
        (progn
          (setf operator *lexeme*)
          (if (string-equal operator "concat")
              (push (takeEXPLISTTerm) subList)
              (push (takeEXPITerm) subList))
          (push (takeEXPLISTTerm) subList)
          (nreverse subList)
          (push operator subList)
          (push subList mainList)
          (push "EXPLIST" mainList))
        (progn
          (undoToken)))
    mainList))


(defun takeEXPITerm()
  (let ((subList nil))
    (nextToken)
    (cond
      ((compare "identifier")
       (setf subList (list "id" *lexeme*))
       (push "EXPI" subList))
      ((compare "integer")
       (setf subList (list *token* *lexeme*))
       (push "EXPI" subList))
      ((compare "operator" "(")
       (setf subList (takeEXPI))
       (if subList
           (progn
             (nextToken)
             (if (not (compare "operator" ")"))
                 (error "Missing operator \")\" after ~a " subList )))))
      (t
       (undoToken)))
    subList))

(defun takeEXPBTerm()
  (let ((subList))
    (nextToken)
    (cond
      ((compare "binaryValue")
       (setf subList (list *token* *lexeme*))
       (push "EXPB" subList))
      ((compare "operator" "(")
       (setf subList (takeEXPB))
       (if subList
           (progn
             (nextToken)
             (if (not (compare "operator" ")"))
                 (error "Missing operator \")\" after ~a " subList )))))
      (t
       (undoToken)))
    subList))

(defun takeEXPLISTTerm()
  (let ((subList nil))
    (nextToken)
    (cond
      ((compare "operator" "'")
       (nextToken)
       (if (compare "operator" "(")
           (progn
             (setf subList (takeListValues nil))
             (nextToken)
             (if (not (compare "operator" ")"))
                 (error "Missing operator \")\" after ~a " subList))
             (if (not subList)
                 (progn
                   (push "'()" subList)))
             (push "LISTVALUE" subList)
             (push "EXPLIST" subList))
           (progn
             (error "Missing operator \"(\" after operator ' " ))))
      ((compare "keyword" "null")
       (push "null" subList)
       (push "LISTVALUE" subList)
       (push "EXPLIST" subList))
      ((compare "operator" "(")
       (setf subList (takeEXPLIST))
       (if subList
           (progn
             (nextToken)
             (if (not (compare "operator" ")"))
                 (error "Missing operator \")\" after ~a " subList )))))
      (t
       (undoToken)))
    subList))
      

(defun takeListValues (subList)
  (nextToken)
  (if (compare "operator" ")")
      (progn
        (undoToken)
        (pop subList)
        (nreverse subList))
      (progn
        (if (compare "integer")
            (progn 
              (push *token* subList)
              (push *lexeme* subList)
              (push "values" subList)
              (setf subList (takeListValues subList))
              subList)
            (progn
              (error "Wrong argument, expected only integer in ListValues "))))))

(defun takeIDList (subList)
  (nextToken)
  (if (compare "operator" ")")
      (progn
        (undoToken)
        (pop subList)
        (nreverse subList))
      (progn
        (if (compare "identifier")
            (progn
              (push "id" subList)
              (push *lexeme* subList)
              (push "IDLIST" subList)
              (setf subList (takeIDList subList))
              subList)
            (progn
              (error "Wrong argument, expected only identifier in IDLIST" ))))))

(defun takeListExp(subList)
  (nextToken)
  (if (compare "operator" ")")
      (progn
        (undoToken)
        (nreverse subList))
      (progn
        (undoToken)
        (push (createTree) subList)
        (setf subList (takeListExp subList))
        subList)))


(defun nextToken ()
  (let ((next))
    (setf next (pop *input*))
    (setf *current* next)
    (setf *token* (pop next))
    (setf *lexeme* (pop next))))

(defun undoToken()
  (push *current* *input*))

(defun compare(token &rest lexeme)
  (if lexeme
      (and (string-equal *token* token) (member *lexeme* lexeme :test #'string-equal))
      (string-equal *token* token)))

(defun printTree ()
  (with-open-file (stream "161044080.tree" :direction :output)
    (format stream "; DIRECTIVE: parse tree ~%")
    (printOutput stream)))

(defun printOutput(stream &optional (n 0))
  (if *tree*
      (let ((next))
        (setf next (pop *tree*))
        (if (listp next)
            (progn
              (format stream "~v@{~a~:*~}" (+ n 1) " ")
              (format stream "(~%" )
              (printList stream next (+ n 1))
              (format stream "~v@{~a~:*~}" (+ n 1) " ")
              (format stream ")~%"))
            (progn 
              (format stream "~v@{~a~:*~}" n " ")
              (format stream "~a~%" next)))
        (printOutput stream (+ n 1)))))

(defun printList (stream lst n)
  (if lst
      (let ((next))
        (setf next (pop lst))
        (if (listp next)
            (progn
              (format stream "~v@{~a~:*~}" (+ n 1) " ")
              (format stream "(~%" )
              (printList stream next (+ n 1))
              (format stream "~v@{~a~:*~}" (+ n 1) " ")
              (format stream ")~%" )
              (printList stream lst  n))
            (progn
              (format stream "~v@{~a~:*~}" n " ")
              (format stream "~a~%" next)
              (printList stream lst (+ n 1)))))))
  


(defun test ()
  (parser '(("operator" "(") ("keyword" "deffun") ("identifier" "myfunct") ("operator" "(") ("identifier" "W") ("identifier" "X") ("identifier" "Y") ("identifier" "Z") ("operator" ")") ("operator" "(") ("keyword" "while") ("operator" "(") ("operator" "(") ("keyword" "not") ("operator" "(") ("keyword" "equal") ("operator" "(") ("keyword" "and") ("binaryValue" "true") ("operator" "(") ("keyword" "or") ("binaryValue" "true") ("binaryValue" "false") ("operator" ")") ("operator" ")") ("binaryValue" "true") ("operator" ")") ("operator" ")") ("operator" ")") ("operator" "(") ("keyword" "for") ("operator" "(") ("identifier" "i") ("operator" "(") ("operator" "*") ("identifier" "X") ("identifier" "Y") ("operator" ")") ("operator" "(") ("operator" "+") ("identifier" "W") ("identifier" "Z") ("operator" ")") ("operator" ")") ("operator" "(") ("operator" "/") ("operator" "(") ("operator" "-") ("identifier" "Z") ("integer" "5") ("operator" ")") ("operator" "(") ("operator" "*") ("integer" "3") ("integer" "2") ("operator" ")") ("operator" ")") ("operator" "(") ("keyword" "append") ("identifier" "mlist") ("operator" "'") ("operator" "(") ("integer" "1") ("integer" "2") ("operator" ")") ("operator" ")") ("operator" "(") ("keyword" "concat") ("operator" "'") ("operator" "(") ("integer" "3") ("integer" "4") ("operator" ")") ("operator" "'") ("operator" "(") ("integer" "5") ("integer" "6") ("operator" ")") ("operator" ")")  ("operator" ")") ("operator" ")") ("operator" ")"))))
