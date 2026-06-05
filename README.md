V PRESENTATION: 
**Give an overview of the language: who designed it, how old is it, how popular is it, what is it good for?**
- Who: Alexander Medvednikov
- When: 2019
- Popularity: Not very popular. Hard to get a measure on this, but its most popular package has 300k downloads. 
- Goals: Be easy to use, understand, and read (achieved through extensive documentation). Be simple and fast (fast compile time, optional garbage collection (mark and sweep! Boehm GC))
- Why? Originally was a personal project for his desktop messaging client called Volt
**A description of the language’s values: does it have objects? closures? strings? symbols? numbers? Other weird things? Highlight things that are interesting or unusual.**
- Values are immutable by default
- Can be made mutable by declaring mut before
- E.g. mut v := 20
           v = 16
- No objects/classes, but has structs and structs can have methods
- Closures exist but must be explicitly defined
- For example, if you have a variable x, if you make a closure you must specify the closure includes x 
**A description of the language’s syntax and scoping: are there statements, distinct from expressions? How does the scoping of variables, functions, classes, and methods work? Again, highlight things that are interesting or unusual.**
- Global variables are not allowed, variables are only allowed within context of functions
- Variable shadowing is not allowed
- E.g. a: = 10
          {a := 20} can’t reinitialize new instance of a even if in different scope
- Field shadowing, however, is allowed. If you have a struct, with variable width int := 1, you can define a width variable outside the struct and have both exist
**If you have any insight into the language’s type system, you’re welcome to share that.**
- Types automatically inferred upon initialization
- E.g. age := 20 gives it type int
- Statically typed (can’t turn a variable int into a string)
- No symbols
**Finally, would you consider taking a job involving writing code in this language?**
I’d take any j*b
