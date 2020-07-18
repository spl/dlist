# Release Instructions

These are the steps for making a new release of the `dlist` package.

1. Check out the latest `main` branch and make sure it is up to date.

   ```sh
   cd <dlist-directory>
   git checkout main
   git pull
   ```

2. Create a new branch for the final changes needed before the release.

   ```sh
   git checkout -b version
   ```

3. Update the [`changelog.md`][] and [`dlist.cabal`][] for the new version,
   `$VERSION`.

   ```sh
   VERSION=<new version number without 'v'>
   $EDITOR changelog.md
   $EDITOR dlist.cabal
   git commit -am "Bump version to $VERSION"
   ```

4. Push the branch and create a pull request on GitHub.

   ```sh
   git push -u origin
   ```

5. Check for problems on the pull request.

   1. Check the [new `changelog.md`][] and [new `dlist.cabal`][].

      If there's a problem, return to step 3.

   2. Check for tests passing.

      If there's a problem, revisit in another branch and pull request. Then,
      merge `main` in to `version` and continue with the next step.

6. Squash and merge the pull request on GitHub.

7. Tag the new version on the `main` branch.

   ```sh
   git checkout main
   git pull
   git branch -D version
   git tag v$VERSION
   git push --tags
   ```

8. Publish the [tag][tags] as a release.

   This will initiate the `upload` workflow, which will run `cabal upload` to
   upload the new version to Hackage.

9. Check [Hackage][] for the candidate documentation and build log.

   If there's a problem, revisit in another branch and pull request. Then,
   return to step 1 with a new `$VERSION`.

10. Publish the candidate on Hackage.

[Hackage]: https://hackage.haskell.org/package/dlist
[`changelog.md`]: ./changelog.md
[`dlist.cabal`]: ./dlist.cabal
[new `changelog.md`]: https://github.com/spl/dlist/blob/version/changelog.md
[new `dlist.cabal`]: https://github.com/spl/dlist/blob/version/dlist.cabal
[tags]: https://github.com/spl/dlist/tags
