Prints the first 6 factorial numbers

+++++++++++++++++++++++++++++++++   ASCII code of !
>++++++++++++++++++++++++++++++
+++++++++++++++++++++++++++++++     ASCII code of =
>++++++++++                       ASCII code of EOL
>+++++++       quantity of numbers to be calculated
>                        current number (one digit)
>+  current value of factorial (up to three digits)
<<                                     loop counter
[         loop to print one line and calculate next

print current number
>++++++++++++++++++++++++++++++++++++++++++++++++.

back from ASCII to number
------------------------------------------------

<<<<.-.>.<.+                             print !_=_

>>>>>                    print cell 6 (preserve it)
>                                      service zero
>++++++++++                                 divisor
<<                                 back to dividend

results in 0 n d_n%d n%d n/d
[->+>-[>+>>]>[+[-<+>]>+>>]<<<<<<]

>[<+>-]       move dividend back to c6 and clear c7
>[-]                                       clear c8

>>  c10 can have two digits; divide it by ten again
>++++++++++                                 divisor
<                                  back to dividend

results in 0 d_n%d n%d n/d
[->-[>+>>]>[+[-<+>]>+>>]<<<<<]

>[-]                                      clear c11
print nonzero n/d (first digit) and clear c13
>>[++++++++++++++++++++++++++++++++++++++++++++++++.[-]]

print nonzero n%d (second digit) and clear c12
<[++++++++++++++++++++++++++++++++++++++++++++++++.[-]]

print any n%d (last digit) and clear c9
<<<++++++++++++++++++++++++++++++++++++++++++++++++.[-]

<<<<<<.                                         EOL
>>+                        increment current number
              multiply c6 by c5 (don't preserve c6)
>[>>+<<-]                             move c6 to c8
>>                                  repeat c8 times
[
<<<[>+>+<<-]                   move c5 to c6 and c7
>>[<<+>>-]                       move c7 back to c5
>-
]
<<<<-                        decrement loop counter
]