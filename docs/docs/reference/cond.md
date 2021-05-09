# cond
Evaluate, one-by one the test-form(s) of the `cond`-form, evaluating the associated body of the first clause whose test succeeds.
Cond returns the expression of the last form of the evaluated body - if no conditions succeeded, `nil` is returned.

##### Definition
```
(cond cond-clause...)

cond-clause = (test-expr body...)
```

##### Example
```lisp
;; lisp input
(cond ((> x 1000)
       "big number")
      ((> x 100)
       "big'ish number")
      (t "small number"))
```

```js
// javascript result
(() => {
   if (x > 1000) {
      return "big number";
   }
   else if (x > 100) {
      return "big'ish number";
   }
   else {
      return "small number";
   }
})()
```