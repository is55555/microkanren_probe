**# microKanren Documentation**

## **1. Overview**

**microKanren** is a minimal implementation of a logic programming system, inspired by miniKanren. It allows unification-based constraint solving and goal-directed computation.

### **Basic Example**

```scheme
(run 1 (== '(var . x) 42))
;; Expected output: ((((var . x) . 42)))
```

This query finds one solution where `(var . x)` is unified with `42`.

---

## **2. Data Structures & Representation**

### **Substitutions**

A substitution is an association list that maps logic variables to values or other variables.

```scheme
(((var . x) . 42) ((var . y) . (var . x)))
```

This represents a state where `x` is bound to `42`, and `y` is bound to `x`.

### **Logic Variables**

Logic variables are represented as **pairs** where the `car` is `'var` and the `cdr` is the variable name:

```scheme
'(var . x)  ;; Represents the variable x
```

### **Goals**

Goals are **functions** that modify the state. They take a substitution (`s`) as input and return a list of possible substitutions.

### **Unification**

Unification attempts to make two terms equal by updating the substitution state.

---

## **3. Core Functions & Their Roles**

### **walk**

```scheme
(define (walk v s)
  (let ((a (assp (lambda (x) (var=? v x)) s)))
    (if a (walk (cdr a) s) v)))
```

#### **Purpose:**

- Recursively resolves a logic variable to its final bound value.

#### **Example Execution:**

```scheme
(let ((s '(((var . x) . 42) ((var . y) . (var . x)))))
  (walk '(var . y) s))
;; Output: 42
```

### **ext-s**

```scheme
(define (ext-s v x s)
  (cons (cons v x) s))
```

#### **Purpose:**

- Extends the substitution with a new binding.
- Does **not** perform occurs-check.

#### **Example Execution:**

```scheme
(ext-s '(var . x) 42 '())
;; Output: (((var . x) . 42))
```

### **unify**

```scheme
(define (unify u v s)
  (let ((u (walk u s))
        (v (walk v s)))
    (cond
      ((eq? u v) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (if s (unify (cdr u) (cdr v) s) #f)))
      (else #f))))
```

#### **Purpose:**

- Unifies two terms and updates the substitution.

#### **Example Execution:**

```scheme
(unify '(var . x) 42 '())
;; Output: (((var . x) . 42))
```

### **== (Equality Constraint)**

```scheme
(define (== u v)
  (lambda (s) (let ((s (unify u v s)))
                (if s (list s) '()))))
```

#### **Purpose:**

- Creates a goal that unifies two terms.

### **disj (Logical OR)**

```scheme
(define (disj . clauses)
  (lambda (s) (apply append (map (lambda (g) (g s)) clauses))))
```

#### **Purpose:**

- Represents a logical OR between goals.

#### **Example Execution:**

```scheme
(run 2 (disj (== '(var . x) 1) (== '(var . x) 2)))
;; Output: ((((var . x) . 1)) (((var . x) . 2)))
```

### **conj (Logical AND)**

```scheme
(define (conj . goals)
  (lambda (s) (foldl (lambda (g acc) (apply append (map g acc))) (list s) goals)))
```

#### **Purpose:**

- Represents a logical AND between multiple goals.

#### **Example Execution:**

```scheme
(run 1 (all (== '(var . x) 1) (== '(var . y) '(var . x))))
;; Output: ((((var . x) . 1) ((var . y) . 1)))
```

### **run (Execution Function)**

```scheme
(define (run n goal)
  (take n (goal '())))
```

#### **Purpose:**

- Executes the logic program and returns up to `n` results.

---

## **4. Control Flow**

1. **Goals are applied in sequence** (through `all`).
2. **Substitutions propagate** through unification.
3. **Logical OR (**\`\`**) explores multiple branches**.
4. **Queries return solutions as lists of substitutions**.

---

## **5. Diagrams & Step-by-Step Execution**

### **Example: Substitution Propagation**

1. **Initial state:** `s = ()`
2. \*\*Apply \*\*\`\` → `s1 = (((var . x) . 42))`
3. \*\*Apply \*\*\`\` → `s2 = (((var . x) . 42) ((var . y) . (var . x)))`
4. \*\*Resolve \*\*`** using **` → `42`

```
Initial:  (var . x)  (var . y)
Step 1:   (var . x) → 42
Step 2:   (var . y) → (var . x)
Step 3:   (var . y) → 42 (after walk)
```

---

## **6. Potential Issues & Debugging**

- **Circular Unification:** Prevented using occurs-check.
- **Non-function Goals in ****\`\`****:** Ensuring all elements are valid procedures.
- **Handling Empty Goals:** Ensuring `all` processes empty cases correctly.

---

## **7. Next Steps**

- Verify the test suite for completeness.
- Optimize debugging output for better readability.
- Refactor large functions for maintainability.

---

This document should give us a **clear and structured** understanding of microKanren before diving back into fixing the tests. Let me know if you want **more details or diagrams** for any section!

