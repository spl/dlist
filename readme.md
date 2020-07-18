# Difference Lists in Haskell

[![test-badge][]][test]
[![hackage-badge][]][hackage-dlist]
[![packdeps-badge][]][packdeps]

## Summary

The Haskell `dlist` package defines a list-like type supporting O(1) append and snoc operations.

See the [change log](./changelog.md) for recent changes.

## References

### Research

1. A novel representation of lists and its application to the function
   “reverse.” John Hughes. Information Processing Letters. Volume 22, Issue 3.
   1986-03. Pages 141-144.
   [PDF][hughes-pdf]

   This is the original source for a representation of lists as first-class functions.

### Basic Introduction

1. [Difference list][wikipedia]. Wikipedia.

2. [Difference list][wiki-haskell]. Haskell.org Wiki.

3. [What is a DList?][stack-overflow]
   Stack Overflow.

### Blogs and Discussion

1. [Using Difference Lists][blog-auclair-1].
   Douglas M. Auclair. 2008-08-13.

2. [A Sort of Difference][blog-kmett].
   Edward Kmett. 2008-09-18.

3. [Reference for technique wanted][mail-okeefe].
   Richard O'Keefe, et al. 2010-10-31.

4. [24 Days of Hackage: dlist][blog-charles].
   Oliver Charles. 2012-12-14.

5. [Constructing a list in a Monad][blog-breitner].
   Joachim Breitner. 2013-11-13.

6. [Demystifying DList][blog-ellis].
   ([On Reddit][blog-ellis-reddit]).
   Tom Ellis. 2014-01-24.

7. [keepEquals with Difference Lists][blog-auclair-2]. Douglas M. Auclair.
   2014-06-21.

### Books

1. [Chapter 13. Data Structures][book-real-world-haskell].
   Real World Haskell. 2008-12-05.

## License

[BSD 3-Clause “New” or “Revised” License][license] © Don Stewart, Sean Leather,
contributors

<!-- Keep these sorted. -->

[blog-auclair-1]: https://logicaltypes.blogspot.com/2008/08/using-difference-lists.html
[blog-auclair-2]: https://logicaltypes.blogspot.com/2014/06/keepequals-with-difference-lists.html
[blog-breitner]: https://www.joachim-breitner.de/blog/620-Constructing_a_list_in_a_Monad
[blog-charles]: https://ocharles.org.uk/blog/posts/2012-12-14-24-days-of-hackage-dlist.html
[blog-ellis-reddit]: https://www.reddit.com/r/haskell/comments/1w5duf/demystifying_dlist/
[blog-ellis]: http://h2.jaguarpaw.co.uk/posts/demystifying-dlist/
[blog-kmett]: https://web.archive.org/web/20080918101635/comonad.com/reader/2008/a-sort-of-difference/
[book-real-world-haskell]: http://book.realworldhaskell.org/read/data-structures.html
[hackage-badge]: https://img.shields.io/hackage/v/dlist.svg?maxAge=3600
[hackage-dlist]: https://hackage.haskell.org/package/dlist
[hughes-pdf]: https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/lists.pdf
[license]: ./license.md
[mail-okeefe]: https://www.mail-archive.com/haskell-cafe@haskell.org/msg83699.html
[packdeps-badge]: https://img.shields.io/hackage-deps/v/dlist.svg?maxAge=3600
[packdeps]: http://packdeps.haskellers.com/feed?needle=dlist
[stack-overflow]: https://stackoverflow.com/questions/3352418/what-is-a-dlist
[test-badge]: https://github.com/spl/dlist/workflows/test/badge.svg
[test]: https://github.com/spl/dlist/actions?query=workflow%3Atest
[wiki-haskell]: https://wiki.haskell.org/Difference_list
[wikipedia]: https://en.wikipedia.org/wiki/Difference_list
