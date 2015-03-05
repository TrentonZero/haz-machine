# haz-machine
[![Build
Status](https://travis-ci.org/TrentonZero/haz-machine.svg)](https://travis-ci.org/TrentonZero/haz-machine.svg)

A Z-Machine implementation in Haskell. 

Still in progress. Early, early stages and not even close to fulfilling all of
the Z-Machine spec, available
[here](http://inform-fiction.org/zmachine/standards/z1point1/index.html).

So far I have set up the basics of the memory map. Though without using
Haskell's Monad's, so it is certain to be rewritten later, because right now I'm
just creating an entire new memory array for every modification to memory, and
it's kinda nasty, and no way it is performant. Why am I knowingly doing it
wrong? 

Because I don't know how to use monad's yet, and since this is a haskell
learning exercise, I'm fine with implementing a few things the niave way before
moving onto the slightly more advanced features. 

Also, I've built a bit around ZSCII Strings.

I have some TDD scaffolding to give me some confidence as I move forward, and
every method has at least one or two test cases around. Not perfect coverage by
any means. 

