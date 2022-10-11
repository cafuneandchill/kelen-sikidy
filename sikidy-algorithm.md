# Kelen Sikidy

1. Generate randomly a 4x4 matrix of 0s and 1s -- the mother seed

    | d | c | b | a |   |
    |---|---|---|---|---|
    |   |   |   |   | 5 |
    |   |   |   |   | 6 |
    |   |   |   |   | 7 |
    |   |   |   |   | 8 |

2. Generate a new 8x4 matrix based on the previous one -- the daughter seed

    | i | m | j | p | k | n | l | q |   |
    |---|---|---|---|---|---|---|---|---|
    |   |   |   |   |   |   |   |   | 1 |
    |   |   |   |   |   |   |   |   | 2 |
    |   |   |   |   |   |   |   |   | 3 |
    |   |   |   |   |   |   |   |   | 4 |
    
    ```
    i = 8 + 7
    j = 6 + 5
    k = d + c
    l = b + a
    m = i + j
    n = k + l
    p = m + n
    q = p + a

    0 + 0 = 0
    0 + 1 = 1
    1 + 0 = 1
    1 + 1 = 0
    ```

3. Generate the third 4x3 matrix; used for checking that the columns are correct

    | 1 | 2 | 3 | 4 |   |
    |---|---|---|---|---|
    |   |   |   |   | x |
    |   |   |   |   | y |
    |   |   |   |   | z |

    ```
    x = n + a
    y = k + b
    z = m + q
    ```

    *x*, *y* and *z* should be the same

4. Interpret direction

    ```haskell
    direction
    | x == "0000" || x == "1111" = "South-east"
    | x == "0001" || x == "1110" = "South"
    | x == "0010" || x == "1101" = "South-west"
    | x == "0011" || x == "1100" = "West"
    | x == "0100" || x == "1011" = "East"
    | x == "0101" || x == "1010" = "North-east"
    | x == "0110" || x == "1001" = "North"
    | x == "0111" || x == "1000" = "North-west"
    | otherwise                  = error
    ```

5. Find an art to study

    ```haskell
    artSeed = [p1, p2]

    art
        | artSeed == "00" = "anālte"
        | artSeed == "01" = "ansāla"
        | artSeed == "10" = "antēnnara"
        | artSeed == "11" = "ankeīlke"
    ```

6. For how many weeks

    ```haskell
    t1 = mod(i1 + m2 + m3 + i4, 2)
    t2 = mod(p1 + j2 + j3 + p4, 2)
    t3 = mod(k1 + n2 + n3 + k4, 2)
    t4 = mod(q1 + l2 + l3 + q4, 2)

    twk = [t1, t2, t3 ,t4]

    weeks
        | twk == "0000" || twk == "1111" = 2
        | twk == "0001" || twk == "1110" = 3
        | twk == "0010" || twk == "1101" = 4
        | twk == "0011" || twk == "1100" = 5
        | twk == "0100" || twk == "1011" = 6
        | twk == "0101" || twk == "1010" = 7
        | twk == "0110" || twk == "1001" = 8
        | twk == "0111" || twk == "1000" = 9
        | otherwise                      = error
    ```
