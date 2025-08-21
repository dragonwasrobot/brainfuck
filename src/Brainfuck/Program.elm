module Brainfuck.Program exposing
    ( Code
    , boundsCheckingLeft
    , boundsCheckingRight
    , helloWorld
    , ioTest
    , memorySizeTest
    , obscureTest
    , quine
    , rot13
    )


type alias Code =
    String


{-| Prints 'Hello world' all-around test of the interpreter.
-}
helloWorld : Code
helloWorld =
    "Print 'Hello world!'\n+++++ +++++             initialize counter (cell #0) to 10\n[                       use loop to set the next four cells to 70/100/30/10\n> +++++ ++              add  7 to cell #1\n> +++++ +++++           add 10 to cell #2\n> +++                   add  3 to cell #3\n> +                     add  1 to cell #4\n<<<< -                  decrement counter (cell #0)\n]\n> ++ .                  print 'H'\n> + .                   print 'e'\n+++++ ++ .              print 'l'\n.                       print 'l'\n+++ .                   print 'o'\n> ++ .                  print ' '\n<< +++++ +++++ +++++ .  print 'W'\n> .                     print 'o'\n+++ .                   print 'r'\n----- - .               print 'l'\n----- --- .             print 'd'\n> + .                   print '!'"


{-| These next two test the array bounds checking. Bounds checking is not
essential, and in a high-level implementation it is likely to introduce
extra overhead. In a low-level implementation you can get bounds checking
for free by using the OS's own memory protections; this is the best
solution, which may require making the array size a multiple of the page
size.

Anyway. These two programs measure the "real" size of the array, in some
sense, in cells left and right of the initial cell respectively. They
output the result in unary; the easiest thing is to direct them to a file
and measure its size, or (on Unix) pipe the output to wc. If bounds
checking is present and working, the left should measure 0 and the right
should be the array size minus one.

-}
boundsCheckingLeft : Code
boundsCheckingLeft =
    "+[<+++++++++++++++++++++++++++++++++.]"


boundsCheckingRight : Code
boundsCheckingRight =
    -- ERROR: Throws a too much recursion error!
    "+[>+++++++++++++++++++++++++++++++++.]"


{-| Outputs itself, general testing of the interpreter.
-}
quine : Code
quine =
    ">>+++++++>>++>>++++>>+++++++>>+>>++++>>+>>+++>>+>>+++++>>+>>++>>+>>++++++>>++>>++++>>+++++++>>+>>+++++>>++>>+>>+>>++++>>+++++++>>+>>+++++>>+>>+>>+>>++++>>+++++++>>+>>+++++>>++++++++++++++>>+>>+>>++++>>+++++++>>+>>+++++>>++>>+>>+>>++++>>+++++++>>+>>+++++>>+++++++++++++++++++++++++++++>>+>>+>>++++>>+++++++>>+>>+++++>>++>>+>>+>>+++++>>+>>++++++>>+>>++>>+>>++++++>>+>>++>>+>>++++++>>+>>++>>+>>++++++>>+>>++>>+>>++++++>>+>>++>>+>>++++++>>+>>++>>+>>++++++>>++>>++++>>+++++++>>+>>+++++>>+++++++>>+>>+++++>>+>>+>>+>>++++>>+>>++>>+>>++++++>>+>>+++++>>+++++++>>+>>++++>>+>>+>>++>>+++++>>+>>+++>>+>>++++>>+>>++>>+>>++++++>>+>>+++++>>+++++++++++++++++++>>++>>++>>+++>>++>>+>>++>>++++>>+++++++>>++>>+++++>>++++++++++>>+>>++>>++++>>+>>++>>+>>++++++>>++++++>>+>>+>>+++++>>+>>++++++>>++>>+++++>>+++++++>>++>>++++>>+>>++++++[<<]>>[>++++++[-<<++++++++++>>]<<++..------------------->[-<.>>+<]>[-<+>]>]<<[-[-[-[-[-[-[>++>]<+++++++++++++++++++++++++++++>]<++>]<++++++++++++++>]<+>]<++>]<<[->.<]<<"


{-| This is for testing i/o; give it a return followed by an EOF. (Try it both
with file input--a file consisting only of one blank line--and with
keyboard input, i.e. hit return and then ctrl-d (Unix) or ctrl-z
(Windows).)

It should give two lines of output; the two lines should be identical, and
should be lined up one over the other. If that doesn't happen, ten is not
coming through as newline on output.

The content of the lines tells how input is being processed; each line
should be two uppercase letters.

Anything with O in it means newline is not coming through as ten on input.
LK means newline input is working fine, and EOF leaves the cell unchanged
(which I recommend).

LB means newline input is working fine, and EOF translates as 0.
LA means newline input is working fine, and EOF translates as -1.
Anything else is fairly unexpected.

-}
ioTest : Code
ioTest =
    ">,>+++++++++,>+++++++++++[<++++++<++++++<+>>>-]<<.>.<<-.>.>.<<."


{-| Goes to cell 30000 and reports from there with a #. (Verifies that the
array is big enough.)
-}
memorySizeTest : Code
memorySizeTest =
    "++++[>++++++<-]>[>+++++>+++++++<<-]>>++++<[[>[[>>+<<-]<]>>>-]>-[>+>+<<-]>]\n+++++[>+++++++<<++>-]>.<<."


{-| Tests for several obscure problems. Should output an H.
-}
obscureTest : Code
obscureTest =
    "[]++++++++++[>>+>+>++++++[<<+<+++>>>-]<<<<-]\n\"A*$\";?@![#>>+<<]>[>>]<<<<[>++<[-]]>.>."


{-| rot13.b program from brainfuck.org; good for testing the response to deep
brackets; the input "~mlk zyx" should produce the output "~zyx mlk".
-}
rot13 : Code
rot13 =
    ",\n[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-\n[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-\n[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-\n[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-\n[>++++++++++++++<-\n[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-\n[>>+++++[<----->-]<<-\n[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-\n[>++++++++++++++<-\n[>+<-[>+<-[>+<-[>+<-[>+<-\n[>++++++++++++++<-\n[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-\n[>>+++++[<----->-]<<-\n[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-\n[>++++++++++++++<-\n[>+<-]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]\n]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]>.[-]<,]"


{-| For an overall stress test, and also to check whether the output is
monospaced as it ideally should be, I would run numwarp.b (below).

Enter a number using ()-./0123456789abcdef and space, and hit return.

-}
stressTest : Code
stressTest =
    ">>>>+>+++>+++>>>>>+++[\n  >,+>++++[>++++<-]>[<<[-[->]]>[<]>-]<<[\n    >+>+>>+>+[<<<<]<+>>[+<]<[>]>+[[>>>]>>+[<<<<]>-]+<+>>>-[\n      <<+[>]>>+<<<+<+<--------[\n        <<-<<+[>]>+<<-<<-[\n          <<<+<-[>>]<-<-<<<-<----[\n            <<<->>>>+<-[\n              <<<+[>]>+<<+<-<-[\n                <<+<-<+[>>]<+<<<<+<-[\n                  <<-[>]>>-<<<-<-<-[\n                    <<<+<-[>>]<+<<<+<+<-[\n                      <<<<+[>]<-<<-[\n                        <<+[>]>>-<<<<-<-[\n                          >>>>>+<-<<<+<-[\n                            >>+<<-[\n                              <<-<-[>]>+<<-<-<-[\n                                <<+<+[>]<+<+<-[\n                                  >>-<-<-[\n                                    <<-[>]<+<++++[<-------->-]++<[\n                                      <<+[>]>>-<-<<<<-[\n                                        <<-<<->>>>-[\n                                          <<<<+[>]>+<<<<-[\n                                            <<+<<-[>>]<+<<<<<-[\n                                              >>>>-<<<-<-\n  ]]]]]]]]]]]]]]]]]]]]]]>[>[[[<<<<]>+>>[>>>>>]<-]<]>>>+>>>>>>>+>]<\n]<[-]<<<<<<<++<+++<+++[\n  [>]>>>>>>++++++++[<<++++>++++++>-]<-<<[-[<+>>.<-]]<<<<[\n    -[-[>+<-]>]>>>>>[.[>]]<<[<+>-]>>>[<<++[<+>--]>>-]\n    <<[->+<[<++>-]]<<<[<+>-]<<<<\n  ]>>+>>>--[<+>---]<.>>[[-]<<]<\n]"
