# 


;; -- REGARDING (if expr then [else]) implementation:
;; TODO: desugar (if expr then-clause) to (if expr then-clause else nil)
;; TODO: assume AST transform already handled progn/expr and return.

;; PARSE (& REWRITE) AST
;; =================================================
;;
;; We (eventually) need a macro to capture the given AST
;;
;; We need a function, code-parse, which, given an AST and a plist
;; of SYMBOL -> REWRITE-FN entries, will process and rewrite the AST.
;;
;; Each REWRITE-FN receives the relevant sub-AST and a PARSE-FN, which
;; continues parsing. The REWRITE-FN then decides which nodes should be
;; treated as sub-AST's to be parsed on their own.
;;
;; In this way, we can chain multiple calls to code-parse to form a multi-pass
;; AST transformation.
;;
;; This way, complex forms can be described by a smaller sub-set of primitive forms.
;;
;; NEXT step - emit-ast
;; --------------------
;; Take processed AST, emitter (code-gen) and SOME idea of how to emit nodes, then
;; begin emitting.

## Testing

Need to test COMBINATIONS
I do not mean to restrict non-sensical JS from being emitted (can write it manually).
I mean to ensure each rewrite function correctly expands the lower layers.

One possible way to test is to expand a series of programs, ensuring equality between expanding
In the fewest number of passes (e.g. multiple rules at the same pass) and one rule per pass.

## EmitJS tasks


### Syntax Support
#### TODO
;; TODO: if-expr -- progn -- ensure last stmt in branch is wrapped in return

;; TODO: extend const/let to take plist and to define each.

;; TODO: extend const/let/args to support destructuring (?)

(@ foo bar baz)

(apply (. js console log) "hello, world")
-: js.console.log("hello, world")

(. js (+ "con" "sole") log)
-: js["con" + "sole"]

;; TODO array stx
;; TODO obj stx
;; TODO if-elif-else (cond)
;; TODO for (?)
https://www.tutorialrepublic.com/javascript-tutorial/javascript-loops.php
;; -- for (C-like)
;; -- for-in -- iter over props in obj
;; -- for-of -- iters over elems in iterable object
;; TODO while  -- `(while test & body)`
;; TODO labels, break
;; TODO switch case
;; await/async (?)

;; PROPERTY and METHOD access
;; TODO (.- obj prop) || obj.prop
;; TODO: (. obj method ...args) || obj.method(...args)

#### DONE
;; let assignment
;; const assignment
;; if-expr
;; one-armed if-expr
;; lambda (implicit return, last stmt)
;; defun -> let ident = lambda

-- binary operators
;;  * / %
;;  + -
;;  << >> >>>
;;  < > <= >=
;;  instanceof
;;  in
;;  ==
;;  !=
;;  ===
;;  !==
;;  & ^ |
;;  && ||

UNARY OPERATORS
https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Expressions_and_Operators#unary_operators
https://www.digitalocean.com/community/tutorials/javascript-unary-operators-simple-and-useful
-- unary postfix operators
++ (inc)
-- (dec)

-- unary prefix operators
~
delete
typeof
void
`+` (->num)
`-` (negate)

## BUGS
### Emitter
* top-level progn should be ignored
* progn further down MAY need to return an output
  -- safest transformation is to make an IIF...

## STATUS

rr/unary/post, rr/unary/pre 


# Logbook

## may 5 - trouble (keywords)
First real trouble, symbols generated are qualified to their package.
Either they need to be exported (unrealistic for user-provided symbols), or keywordized, as keywords
are symbols living in a single global package called KEYWORD.

(intern (symbol-name SYM) "KEYWORD")

May also need to split code further, getting requirements to export emit-js as well to test.
Should probably commit and then start slowly moving stuff out while expanding tests.

## may ~2 --- ininitial
