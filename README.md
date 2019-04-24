# splitpath


## Synopsis

Safely split paths in many ways (Haskell)


## Description

The standard tools and techniques in bash for separating file paths into
directories, filenames and extensions are confusing. For example, these things:
`"${FOO##*.}"` and `"${BAR%.*}"` Also, quoting gets complicated when paths
contain spaces.

On the other hand, many programming language standard libraries have powerful
tools for breaking apart file paths.

This software wraps the functionality present in Haskell's System.FilePath and
System.Directory libraries, exposing these functions in a simple utility with
clearly-named switches.


## Getting source

Source code is available from github at the
[splitpath](https://github.com/dino-/splitpath) project page.


## Contact

Dino Morelli <dino@ui3.info>
