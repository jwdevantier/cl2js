# let-binding

Bindings introduced by `let` are local to the body of the `let`-form. 
The resulting javascript uses an IIFE and the bindings effectively works
as if variables were declared by `let`, re-assignment is possible and
bindings in nested `let`-forms may shadow the outer bindings.

##### Definition
```
(let ((id val-expr)...) body...+)
```

##### Example
```lisp
(let ((x 10)
      (y (* 2 4)))
   (+ x y))
```

```js
((x, y) => {
   return x + y;
})(10, 2 * 4)
```
