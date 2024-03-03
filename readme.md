# projectile-comphist #
A package to add per-project persistent compile command history to projectile

projectile-comphist requires [persist](https://elpa.gnu.org/packages/persist.html) and [projectile](https://github.com/bbatsov/projectile)

## Installation ##

Put `projectile-comphist.el` in the `load-path`.

## Usage ##

Add the following to your configuration file:
```lisp
    (require 'projectile-comphist)
    (advice-add 'projectile-compile-project :override 'projectile-comphist-compile)
```
Then when compiling using `C-x p c` it will prompt you for a compile command using persistent history
