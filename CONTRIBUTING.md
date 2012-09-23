# How to contribute

Contributions to Magit are highly welcome. Here are a few guidelines that will
help your patches hit upstream as soon as possible.

## Branching scheme

Magit uses 3 main branches for its lifecyle: `maint`, `master` and `next`.

* `maint` contains the set of patches that will definitely make it into the next
  minor release.
* `master` contains the set of patches that will highly likely make it into the
  next major release.
* `next` contains patches that necessitate some additional checks/improvements
  before they're integrating into a release.

## Making changes

A good practice is to create a topic branch off Magit, from the branch you
target for *final* inclusion. This should *always* be either `maint` or
`master`.

If you branch off `next`, you'll only put more overhead on the maintainer's
shoulders, and the integration will suffer from additional delays.

Please make sure your commits are well-organized and "atomic" (hitting a single
well-defined target each).

Please also make sure you check that byte-compilation completes without errors
of warnings, and that tests run without failures.

Bonus points if you add tests to cover the feature you're hacking.

## Submitting changes

The preferred way of submitting your changes is to open a pull request on the
central Magit GitHub repository (https://github.com/magit/magit).

Alternately, you can send your patches to the Magit mailing list
(magit@googlegroups.com), using `git send-email`.
