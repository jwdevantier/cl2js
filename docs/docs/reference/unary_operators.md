# Unary operators

Support for the various [unary operators](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Expressions_and_Operators#unary_operators) of Javascript exists.

## delete
Delete a property from an object.

**Grammar**
```
(delete object-property)
```

**Example**
```lisp
;; in lisp
(delete (@ obj someprop))
```

```js
// in javascript
delete obj.someProp
```

## typeof
Get type of `operand`.

**Grammar**
```
(typeof operand)
```

**Example**
```lisp
(typeof "hello")
```

```js
(typeof "hello")
// ... or
typeof("hello")
```

## void
Evaluate expression without returning its value. Generally discouraged.

**Grammar**
```
(void expr)
```

**Example**
```lisp
(void (+ 2 3))
```


## Aliased operators
Some operators are aliased. In some cases because distinguishing between pre- and postfix operators becomes difficult, in others because these functions exist in different lisp dialects under other names.

### increment (`++`)

```lisp
(inc x)
```

### decrement (`--`)
```lisp
(dec x)
```

### negate (`!`)

```lisp
(not x)
```

###  coerce to number (`+`)

**Example**
```lisp
(->num x)
```

```js
(+ x)
```

###  coerce to number and negate (`-`)

**Example**
```lisp
(negate x)
```

```js
(- x)
```

```js
void 2 + 3;
```
