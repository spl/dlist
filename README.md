# Difference Lists in Haskell

[![Travis CI](https://img.shields.io/travis/spl/dlist.svg?maxAge=2592000)](https://travis-ci.org/spl/dlist)
[![Hackage](https://img.shields.io/hackage/v/dlist.svg?maxAge=2592000)](https://hackage.haskell.org/package/dlist)

## Summary

The Haskell `dlist` package defines a list-like type supporting O(1) append and snoc operations.

See [ChangeLog.md](./ChangeLog.md) for recent changes.

## References

### Research

1. A novel representation of lists and its application to the function
   “reverse.” John Hughes. Information Processing Letters. Volume 22, Issue 3.
   1986-03. Pages 141-144.
  [[PDF](http://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/lists.pdf)]

   This is the original source for a representation of lists as first-class functions.

### Basic Introduction

1. [Difference list](https://en.wikipedia.org/wiki/Difference_list). Wikipedia.

2. [Difference lists](https://wiki.haskell.org/Difference_list). Haskell.org Wiki.

3. [What is a DList?](https://stackoverflow.com/questions/3352418/what-is-a-dlist).
   Stack Overflow.

### Blogs and Discussion

1. [Using Difference Lists](http://logicaltypes.blogspot.com/2008/08/using-difference-lists.html).
   Douglas M. Auclair. 2008-08-13.

2. [A Sort of Difference](https://archive.is/20140131124629/http://web.archive.org/web/20080918101635/comonad.com/reader/2008/a-sort-of-difference/).
   Edward Kmett. 2008-09-18.

3. [Reference for technique wanted](http://thread.gmane.org/gmane.comp.lang.haskell.cafe/82827).
   Richard O'Keefe, et al. 2010-10-31.

4. [24 Days of Hackage: dlist](https://ocharles.org.uk/blog/posts/2012-12-14-24-days-of-hackage-dlist.html).
   Oliver Charles. 2012-12-14.

5. [Constructing a list in a Monad](https://www.joachim-breitner.de/blog/620-Constructing_a_list_in_a_Monad).
   Joachim Breitner. 2013-11-13.

6. [Demystifying DList](http://h2.jaguarpaw.co.uk/posts/demystifying-dlist/).
   ([On Reddit](https://www.reddit.com/r/haskell/comments/1w5duf/demystifying_dlist/)).
   Tom Ellis. 2014-01-24.

7. [keepEquals with Difference Lists](http://logicaltypes.blogspot.com/2014/06/keepequals-with-difference-lists.html),
   Douglas M. Auclair. 2014-06-21.

### Books

1. [Chapter 13. Data Structures](http://book.realworldhaskell.org/read/data-structures.html).
   Real World Haskell. 2008-12-05.
