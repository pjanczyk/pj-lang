# pjlang


```
/* Calculates factorial (iterative implementation) */
factorial = n -> {
    i = 1;
    result = 1;
    while i <= n {
        result *= i;
        i += 1;
    };
    result;
};

/* Calculates factorial (recursive implementation) */
factorial' = n -> if n == 0 then 1 else n * factorial'(n - 1);

/* Calculates Fibonacci number (iterative implementation) */
fibonacci = n -> {
    a = 0;
    b = 1;
    i = 0;
    while i < n {
        c = a + b;
        a = b;
        b = c;
        i += 1;
    };
    a;
};

/* Calculates Fibonacci number (recursive implementation) */
fibonacci' = n -> {
    f = (i, a, b) -> if i >= n then a else f(i + 1, b, a + b);
    f(0, 0, 1);
};

/* Prints values of a function for arguments in range [a, b) */
printFuncRange = (func, a, b) -> {
    i = a;
    while i < b {
        printLine(toString(i) + ": " + toString(func(i)));
        i += 1;
    };
};

printLine("factorial");
printFuncRange(factorial, 0, 5);

printLine("factorial'");
printFuncRange(factorial', 0, 5);

printLine("fibonacci");
printFuncRange(fibonacci, 0, 5);

printLine("fibonacci'");
printFuncRange(fibonacci', 0, 5);
```