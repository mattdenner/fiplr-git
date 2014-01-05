# fiplr-git - Extension to fiplr for gitignore globs

An extension to the [fiplr](https://github.com/d11wtq/fiplr) Emacs package that
uses .gitignore files for the ignore glob patterns.

## Installation

Best to use [el-get](https://github.com/dimitri/el-get) and then use the
[recipe file](https://github.com/mattdenner/fiplr-git/blob/master/fiplr-git.rcp).

*NOTE:* this requires [a fix I've made](https://github.com/d11wtq/fiplr/pull/30) in
the fiplr codebase to allow for ignore glob changes.

## Usage

Just as with `fiplr` use the appropriate function:

    M-x fiplr-find-file-with-gitignore

## Future work

* `fiplr-find-directory-with-gitignore`
* use global .gitignore too
* support negative globs from .gitignore

## Copyright & Licensing

Copyright (c) Matthew Denner 2014, Licensed under the same terms as GNU Emacs.
