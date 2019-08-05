# Difference Lists in Haskell

[![Hackage](https://img.shields.io/hackage/v/dlist.svg?maxAge=3600)](https://hackage.haskell.org/package/dlist "dlist on Hackage")
[![Hackage dependencies](https://img.shields.io/hackage-deps/v/dlist.svg?maxAge=3600)](http://packdeps.haskellers.com/feed?needle=dlist "dlist updated Hackage dependencies")
[![Travis CI](https://img.shields.io/travis/spl/dlist.svg?maxAge=3600)](https://travis-ci.org/spl/dlist "dlist build history on Travis CI")

## Summary

The Haskell `dlist` package defines a list-like type supporting O(1) append and snoc operations.

See [ChangeLog.md](./ChangeLog.md) for recent changes.

## References

### Research

1. A novel representation of lists and its application to the function
   “reverse.” John Hughes. Information Processing Letters. Volume 22, Issue 3.
   1986-03. Pages 141-144.
  [[PDF](https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/lists.pdf)]

   This is the original source for a representation of lists as first-class functions.

### Basic Introduction

1. [Difference list](https://en.wikipedia.org/wiki/Difference_list). Wikipedia.

2. [Difference list](https://wiki.haskell.org/Difference_list). Haskell.org Wiki.

3. [What is a DList?](https://stackoverflow.com/questions/3352418/what-is-a-dlist)
   Stack Overflow.

### Blogs and Discussion

1. [Using Difference Lists](https://logicaltypes.blogspot.com/2008/08/using-difference-lists.html).
   Douglas M. Auclair. 2008-08-13.

2. [A Sort of Difference](https://web.archive.org/web/20080918101635/comonad.com/reader/2008/a-sort-of-difference/).
   Edward Kmett. 2008-09-18.

3. [Reference for technique wanted](https://www.mail-archive.com/haskell-cafe@haskell.org/msg83699.html).
   Richard O'Keefe, et al. 2010-10-31.

4. [24 Days of Hackage: dlist](https://ocharles.org.uk/blog/posts/2012-12-14-24-days-of-hackage-dlist.html).
   Oliver Charles. 2012-12-14.

5. [Constructing a list in a Monad](https://www.joachim-breitner.de/blog/620-Constructing_a_list_in_a_Monad).
   Joachim Breitner. 2013-11-13.

6. [Demystifying DList](http://h2.jaguarpaw.co.uk/posts/demystifying-dlist/).
   ([On Reddit](https://www.reddit.com/r/haskell/comments/1w5duf/demystifying_dlist/)).
   Tom Ellis. 2014-01-24.

7. [keepEquals with Difference Lists](https://logicaltypes.blogspot.com/2014/06/keepequals-with-difference-lists.html).
   Douglas M. Auclair. 2014-06-21.

### Books

1. [Chapter 13. Data Structures](http://book.realworldhaskell.org/read/data-structures.html).
   Real World Haskell. 2008-12-05.
