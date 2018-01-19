# PJ programming language

### Example 1
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


### Example 2
```
/* Creates a generator of sequence */
range = (a, b) -> {
    state = a;

    -> {
        if (state < b) {
            val = state;
            state += 1;
            val;
        }    
    };
};

printSeq = seq -> {
    while { elem = seq(); elem != null } {
        print(elem);
        print(" ");
    };
    printLine("");
};

map = (seq, mapping) -> {
    -> {
        elem = seq();
        if elem != null {
            mapping(elem);
        };
    };
};   

filter = (seq, cond) -> {
    -> {
        while { elem = seq(); elem != null && !cond(elem) } {
        };
        elem;
    };
};

printSeq(range(1, 10));

printSeq(map(range(1, 10), x -> 2 * x));

printSeq(filter(range(1, 10), x -> x % 2 == 1));
```