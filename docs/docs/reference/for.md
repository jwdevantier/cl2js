# for-loop
A basic for-loop - advisable to use only as a building-block for more lisp-like iteration constructs.
The for-loop supports multi

##### Definition
```
(for ([:let bindings]
      [:cond test-expr]
      [:iter iter-clause])
      body...)

bindings = binding
         | (bindings...+)

bindings = (identifier expr)

iter = expr
     | (expr...+)

```

##### Example
```lisp
;; lisp input
(for (:let  ((x 10) (y 20))
      :test (> x y)
      :iter ((inc x) (set y (+ y 2))))
  ...body)
```

```js
// javascript result
(() => {
   for (let x = 10, y = 20; x > y; x++, y = y + 2)
   {
      //...body
   }
   return null;
})()
```
