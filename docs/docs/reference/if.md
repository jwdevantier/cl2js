# if
works as a ternary-if, yielding the result of evaluating the expression of the `then` or (optional) `else` clause, depending on the condition.
Use `cond` to evaluate multiple conditions.

##### Definition
```
(if test-expr then-expr [else-expr])
```

##### Example
```lisp
;; lisp input
(if (> x 10) "x is big" "x is small")
```

```js
// javascript result
(() => {
   if (x > 10) {
      return "x is big";
   }
   else {
      return "x is small";
   }
})()
```
##### Notes
* use `(progn ...)` to group multiple expressions in the `then-expr` or `else-expr` clauses.
* if `else-expr` is not provided, the else-branch will return `null`
