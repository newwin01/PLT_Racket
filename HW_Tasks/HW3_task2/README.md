## ITP 30011-01 Programming Language Theory, Fall 2023
### Homework 3 - Task2: Task2 Implement FAE in your favoriate langauge

### *Contributer: Sechang Jang 21900628*

---
### Requirements

Gradle 7.6 <br>
Java 15+

Goals: By implementing RFAE in your favorite programming language, you can understand how a language can support a recursion better.

It works with following options

> *i: crashing input | p: only parse <br>*
'i' option is required to execute program

Example
> -i "{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 6} {f 4}}}}" 
> <br> (numV 7)

> -i "{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 6} {f 4}}}}" - p
> <br> (app (fun x (app (fun f (app (fun x (app (id f) (num 4))) (num 6))) (fun y (add (id x) (id y))))) (num 3))

In case of gradle execution, unzip the file and execute
>  gradle build

To execute the program, in case of Window(Powershell) <br>
Two quotation with escape character required 
<br> e.g) \\""

>  gradle run --args="-i \\""{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 6} {f 4}}}}\\"" "
> <br> (numV 7)

> gradle run --args="-i \\""{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 6} {f 4}}}}\\"" -p"
> <br> (app (fun x (app (fun f (app (fun x (app (id f) (num 4))) (num 6))) (fun y (add (id x) (id y))))) (num 3))

To execute the program, in case of Linux (Mac or Ubuntu) <br>
One quotation with escape character is enough
<br> e.g) \\" 

>  gradle run --args="-i \\"{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 6} {f 4}}}}\\" "
> <br> (numV 7)

> gradle run --args="-i \\"{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 6} {f 4}}}}\\" -p"
> <br> (app (fun x (app (fun f (app (fun x (app (id f) (num 4))) (num 6))) (fun y (add (id x) (id y))))) (num 3))


Please execute the program using the preview, not the written one in markdown file. There must be one escape character, and one or two quotation marks.

