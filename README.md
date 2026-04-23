# splitpath

## !!!!! ATTENTION !!!!!

This project has been permanently moved to Codeberg
([splitpath](https://codeberg.org/dinofp/splitpath)) and is no longer actively
maintained on Github. Do not use the Issues system on Github to report to us.
Don't bother forking or getting source from here as it will not be updated.

Microsoft is not a friend of open-source and we do ourselves a disservice
giving them this impressive power over our work.

Never forget 2020 when Github (a Microsoft product) removed the popular
open-source `youtube-dl` project, sparking enormous controversy. The issue is
not that pushback eventually prompted reinstatement - Github can and will act
like this against us at any time.

## !!!!! ATTENTION !!!!!

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

Usage examples:

    $ splitpath --takedirectory  /foo/bar/baz.tar.gz    # /foo/bar
    $ splitpath --dropfilename   /foo/bar/baz.tar.gz    # /foo/bar/
    $ splitpath --takeextension  /foo/bar/baz.tar.gz    # .gz
    $ splitpath --dropextension  /foo/bar/baz.tar.gz    # /foo/bar/baz.tar
    $ splitpath --takeextensions /foo/bar/baz.tar.gz    # .tar.gz
    $ splitpath --dropextensions /foo/bar/baz.tar.gz    # /foo/bar/baz
    $ splitpath --takebasename   /foo/bar/baz.tar.gz    # baz.tar
    $ splitpath --takefilename   /foo/bar/baz.tar.gz    # baz.tar.gz
    $ splitpath --makeabsolute   somefile               # /current/dir/somefile
    $ splitpath --makerelative   /current/dir/somefile  # somefile


## Getting source

Source code is available from github at the
[splitpath](https://github.com/dino-/splitpath) project page.


## Contact

Dino Morelli <dino@ui3.info>
