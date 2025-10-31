# ForthRPL
A Forth (RPL) System like you have never seen... 

Written entirely in Lisp, using Lisp as its "Assembly Language" - means that memory is infinite, and you can do absolutely anything and everything.

This system was developed back around 2010 and served as our means for distributing an audio processing algorithm while it was under trade secret protection during the waiting period until a US Patent was awarded. 

Burying your secret sauce inside a custom built, randomized, token-threaded Forth system was quite effective obfuscation, while leaving it entirely functional. Of course the target embedded C code was written by the MetaCompiler, transcoding runnable Forth (RPL) code that represented the algorithm. We didn't have to write the C code ourselves - just teach Lisp how to do it for us.

This Forth lives entirely in the aether, unlike most Forth systems that reside at physical addresses in some real machine. We don't need any addresses here. Just references to symbolic arenas holding lists and vectors of elements. The system takes advantage of Lisp native data types, so you have strings, integers, bignums, rational numbers, floating point, lists, vectors, arrays, whatever you want. An entire vector takes just one stack slot on the eval stack.

To use, just (ASDF :FORTHRPL) and then type (INTERACTIVE). Now you are in Forth and can do the usual, except for absolute memory address fetch and store - no such thing. Drop into Assembly mode at any time using CODE and its brothers, where you now write native compiled Lisp code for yourself. If you really do want machine Assembly, then just go ahead and write your own Assembler. It's pretty easy. Typically takes only a few hours if you really need it.

Forth does have its uses...

# Why use this (dead?) language?
Forth rose to prominence during a time of rapid scientific progress and when the mini-computers of the time had relatively limited memory resources. You could pack an entire major telescope control system, all the astronomical calculations for the star catalog (precession, nutation, aberration), perform equatorial to alt-az conversions, control the dome, and control an observing instrument attached to the back of the giant telescope, and collect and record and analyze data. All of this in less than 30 kWords (16-bit words) of Core Memory.

Today is a very different story. We have monster machines, in comparison, sitting on our desktops, or even in the palms of our hands. They are almost all 64-bit machines. Back in the day, only gigantic IBM Mainframes had that bus width, but none of the mainframes had anywhere near as much memory as you can now fit on a card smaller than a fingernail. And the speed of machines today? At the (then) world's largest telescope, in 1979, we had a DG Nova-800 minicomputer with 32 kWords of core memory, running the entire telescope and observatory. It was given model designation Nova-800 because it had an astounding 800 ns Read-Modify-Write cycle time - beating out the previous generation Nova-1200.

But with Forth you have essentially a very high-level Assembler language, and you can drop, interactively, all the way down to machine Assembly level at any time. That is kind of a blast to do from time to time. But to write code that is inherently reentrant in the face of timesliced multi-threading on a multi-core CPU, would be a painful chore to write in Forth. Since Forth is Turing Complete you can certainly write anything. But the pain of the process would become unbearable.

So why write Forth today? Because it teaches things about computer programming, much like Lisp and Scheme broaden your horizons. Forth is the only language that allows you to play with the Return Stack as readily as with the Data Stack. And it promotes a kind of threading simplification of code that is only possible using advanced control flow techniques that involve manipulating the contents of the Return Stack. This is halfway toward continuations in Scheme, and cannot be duplicated by either Lisp or Scheme.

Writing good code in Forth requires substantial thinking. I am most proficient in Lisp, and I can be massively productive with it. I cannot duplicate that productivity using Forth, even though I spent the better part of 20 years writing Forth. I can more easily read S-Expressions in Lisp, than I can read threaded code in Forth. And thinking about the most efficient, or most terse, statement of an algorithm in Forth takes 10x longer than in Lisp, for me. But going through the exercise in Forth, at least partway, imparts an appreciation and new insights about computer programming that I don't get from the insular environment provided by my Lisp system. 

So both Forth and Lisp could be called LFSP's, but they each have very different arenas of applicability.

Where Lisp and Scheme have automatic memory management and functional closures on offer, Forth has Return Stack manipulation, a zero address (stack) architecture, and interactive Assembly programming on offer.

In this ForthRPL system, our Assembly language is Lisp. That's quite a powerful inversion!
