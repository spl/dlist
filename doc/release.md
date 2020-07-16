The release process is as follows:

1. `git checkout master`

2. `git pull`

3. `git checkout -b version-bump`

4. Update [`changelog.md`](./changelog.md) with release notes for version `$VER`.

5. Update version to `$VER` in [dlist.cabal](./dlist.cabal).

6. `git commit -am 'Bump version to $VER'`

7. `git push -u origin`

8. Check release notes in [`changelog.md`](https://github.com/spl/dlist/blob/version-bump/changelog.md).

9. `git tag v$VER`

10. `git push --tags`

11. Check for tests passing on [Travis CI](https://travis-ci.org/spl/dlist/builds).

12. `git checkout master`

13. `git merge --ff-only version-bump`

14. `git push`

15. Publish tag as a release on [releases](https://github.com/spl/dlist/releases).

16. Delete branch `version-bump` on [branches](https://github.com/spl/dlist/branches).

17. `cabal sdist`

18. `cabal upload dist/dlist-$VER.tar.gz`

19. Check build on [Hackage](https://hackage.haskell.org/package/dlist).
