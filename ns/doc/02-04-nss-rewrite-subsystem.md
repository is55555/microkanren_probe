### 📗 Subsystem: `rewrite`

#### Role

Walks expressions and rewrites **referenced symbols** to their mangled equivalents using `lookup-symbol`.

#### Behavior

| Type         | Behavior                                 |
|--------------|------------------------------------------|
| `symbol`     | Rewritten using `lookup-symbol`          |
| `pair`       | - If special form: preserves head, rewrites args<br>- Otherwise: rewrites entire structure |
| `vector`     | Rewritten element-wise                   |
| `number`, `string`, etc. | Returned as-is                       |

#### Special Form Awareness

A static list of built-in special forms is respected, including:

```scheme
'(if lambda define set! begin cond let let* quote quasiquote
  unquote unquote-splicing ns ns-inline ns-set)
```

Only their **arguments** are rewritten, preserving structure.

#### Example

```scheme
(rewrite '(if x y x))     ⇒ (if demo__x demo__y demo__x)
(rewrite '(foo x))        ⇒ (demo__foo demo__x)
```

---

Let me know if you’d like this documentation exported to a markdown file or left inline in the `.scm` source headers.