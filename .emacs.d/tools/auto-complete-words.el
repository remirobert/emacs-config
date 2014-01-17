
(defvar c++-keywords
  (sort
   (list "and" "bool" "compl" "do" "export" "goto" "namespace" "or_eq" "return"
	 "struct" "try" "using" "xor" "and_eq" "break" "const" "double" "extern"
	 "if" "new" "private" "short" "switch" "typedef" "virtual" "xor_eq" "asm"
	 "case" "const_cast" "dynamic_cast" "false" "inline" "not" "protected"
	 "signed" "template" "typeid" "void" "auto" "catch" "continue" "else"
	 "float" "int" "not_eq" "public" "sizeof" "this" "typename" "volatile"
	 "bitand" "char" "default" "enum" "for" "long" "operator" "register"
	 "static" "throw" "union" "wchar_t" "bitor" "class" "delete" "explicit"
	 "friend" "mutable" "or" "reinterpret_cast" "static_cast" "true"
	 "unsigned" "while" ) #'(lambda (a b) (> (length a) (length b)))))

(defvar c-keywords
  (sort
   (list "and" "do" "goto" "return" "struct" "break" "const" "double" "extern"
	 "if" "short" "switch" "typedef" "asm" "case" "inline" "not"
	 "signed" "void" "auto" "continue" "else" "float" "int" "public" "sizeof"
	 "volatile" "char" "enum" "for" "long" "register"
	 "static" "union" "unsigned" "while" ) #'(lambda (a b) (> (length a) (length b)))))

(defvar d-keywords
  (sort
   (list "and" "bool" "compl" "do" "export" "goto" "namespace" "or_eq" "return"
	 "struct" "try" "using" "xor" "and_eq" "break" "const" "double" "extern"
	 "if" "new" "private" "short" "switch" "typedef" "virtual" "xor_eq" "asm"
	 "cast" "cast" "false" "inline" "not" "protected" "signed" "template"
	 "typeid" "void" "auto" "catch" "continue" "else" "float" "int" "not_eq"
	 "public" "sizeof" "this" "typename" "volatile" "bitand" "char" "default"
	 "enum" "for" "long" "operator" "register" "static" "throw" "union"
	 "wchar_t" "bitor" "class" "delete" "explicit" "friend" "mutable" "or"
	 "cast" "cast" "true" "unsigned" "while" ) #'(lambda (a b) (> (length a) (length b)))))

(defvar ruby-keywords
  (sort
   (list "and" "do" "end" "return" "break" "if" "new" "case" "false" "in" "not"
	 "catch" "else" "elsif" "self"  "def" "for" "throw" "class" "or"
	 "true" "yield" ) #'(lambda (a b) (> (length a) (length b)))))

(defvar python-keywords
  (sort
   (list
    "and" "del" "from" "not" "whileas" "elif" "try" "for"
    "global" "or" "withassert" "else" "if" "pass" "lambda"
    "yieldbreak" "except" "import" "printclass" "exec"
    "raisecontinue" "finally" "is" "return" "def" "in") #'(lambda (a b) (> (length a) (length b))))
  )

(defvar ac-source-c++
  '((candidates
     . (lambda ()
	 (all-completions ac-target c++-keywords))))
  "Source for c++ keywords.")

(defvar ac-source-c
  '((candidates
     . (lambda ()
	 (all-completions ac-target c-keywords))))
  "Source for c keywords.")

(defvar ac-source-d
  '((candidates
     . (lambda ()
	 (all-completions ac-target d-keywords))))
  "Source for d keywords.")

(defvar ac-source-ruby
  '((candidates
     . (lambda ()
	 (all-completions ac-target ruby-keywords))))
  "Source for ruby keywords.")

(defvar ac-source-python
  '((candidates
     . (lambda ()
	 (all-completions ac-target python-keywords))))
  "Source for python keywords.")

(provide 'auto-complete-words)
