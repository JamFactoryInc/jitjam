# JitJam

---

## Overview

---

## Important Notes
You should read this section before embedding JitJam


### Operator definitions

#### Addition / Subtraction (`+` / `-`):
> [!NOTE]
> Integer overflow/underflow is permitted by default. Constant folding is more powerful if this is disabled.

Integer and floating-point addition/subtraction work as expected

#### Multiplication (`*`):
> [!NOTE]
> Integer overflow/underflow is permitted by default. Constant folding is more powerful if this is disabled.

Integer and floating-point multiplication work as expected

#### Division (`/`):
> [!WARNING]
> Division by zero is undefined behaviour and up to the implementor (you) to handle
> 
> This may cause a compile error if a constant expression cannot be evaluated due to this restriction

Beyond this divide-by-zero consideration, integer and floating-point division work as expected
```
 // this pseudocode would fail jitjam compilation
 const x = 1 / 0;
```

#### Modulus (`%`):
> [!WARNING]
> Modulo by zero is undefined behaviour and up to the implementor (you) to handle
>
> This may cause a compile error if a constant expression cannot be evaluated due to this restriction
 
The remainder of a modulo operation is signed. The sign is determined by the sign of the numerator, as shown below.
```
 -1 %  5  -> -1
 -1 % -5  -> -1
  1 %  5  ->  1
  1 % -5  ->  1
```

#### Left Shift (`<<`):
> [!WARNING]
> left-shifting by a negative value is undefined behaviour and up to the implementor (you) to handle

Left shift is implemented be non-wrapping, so bit shifted beyond the range of the value type will be truncated.

> #### Right Shift (`>>`):
> [!WARNING]
> right-shifting by a negative value is undefined behaviour and up to the implementor (you) to handle

Right shift is implemented be non-wrapping, so bit shifted beyond the range of the value type will be truncated.
    
     

