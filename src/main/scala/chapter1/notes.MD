# Chapter 1

## The purpose of a Type system
 - ```A type system is a tractable syntactic method for proving the absence of certain program behaviors by classifying phrases according to the kinds of values they compute. ```
   -  Conservativity: a Type system can not prove the existence of bad behavior , rather the absence of it, therefore it is possible a Type system rule out certain programs
that would actually not cause an error at runtime.
   - There is a specific subset of errors that Type systems can help to remove. This implies that many other errors are undetectable by Type systems.
     - For example, a Type system can not verify a mathematical operation is mathematically valid, such as dividing by zero.
     - in building a Type system we must decide what errors we want to rule out and what errors we want to allow.
 - Abstraction
 - Documentation
 - Efficiency 
   - It is quite interesting to note that many high performance compilers will use information collected during th etype checking phase to optimize its output
   - 