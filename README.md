## Prolog Quine-Mccluskey Algorithm Implementation

Just call the python function. It already has good UX.
If you want to call it directly, use:
```prolog
quine(3, "1,3,4", Output).
quine(3, "0,1,4,7", Output).
```

in Python, this is the sample interaction
```
Enter number of variables: 4
Enter minterms (0 - 15)(separated by comma ','):  0,1,3,7,8,9,11,15
quine(4, '0,1,3,7,8,9,11,15', Result)
B'C'+B'D+CD
```