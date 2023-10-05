# ForthRPL
A Forth (RPL) System like you have never seen... 

Written entirely in Lisp, using Lisp as its "Assembly Language" - means that memory is infinite, and you can do absolutely anything and everything.

This system was developed back around 2010 and served as our means for distributing an audio processing algorithm while it was under trade secret protection during the waiting period until a US Patent was awarded. 

Burying your secret sauce inside a custom built, randomized, token-threaded Forth system was quite effective obfuscation, while leaving it entirely functional. Of course the target embedded C code was written by the MetaCompiler, transcoding runnable Forth (RPL) code that represented the algorithm. We didn't have to write the C code ourselves - just teach Lisp how to do it for us.

This Forth lives entirely in the aether, unlike most Forth systems that reside at physical addresses in some real machine. We don't need any addresses here. Just references to symbolic arenas holding lists and vectors of elements. The system takes advantage of Lisp native data types, so you have strings, integers, bignums, rational numbers, floating point, lists, vectors, arrays, whatever you want. An entire vector takes just one stack slot on the eval stack.

To use, just (ASDF :FORTHRPL) and then type (INTERACTIVE). Now you are in Forth and can do the usual, except for absolute memory address fetch and store - no such thing. Drop into Assembly mode at any time using CODE and its brothers, where you now write native compiled Lisp code for yourself. If you really do want machine Assembly, then just go ahead and write your own Assembler. It's pretty easy. Typically takes only a few hours if you really need it.

Forth does have its uses...
