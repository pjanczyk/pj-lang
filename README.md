# pjlang


```

factorial = n -> {
    i = 0;
    result = 1;
    while i <= n {
        i += 1;
        result *= i;
    }
    result;
}

factorial' = n -> if n == 0 then 1 else n * factorial(n - 1);


fibonacci = n -> {
    a = 0;
    b = 1;
    i = 0;
    while i < n {
        c = a + b;
        a = b;
        b = c;        
    }
    a;
}

fibonacci' = n -> {
    f = (i, a, b) -> if i >= n then a else f(i + 1, b, a + b);
    f(0, 0, 1);
}
