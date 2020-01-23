## Test environments

* local: mingw32-3.6.1
* travis: 3.1, 3.2, 3.3, oldrel, release, devel
* r-hub: windows-x86_64-devel, ubuntu-gcc-release, fedora-clang-devel
* win-builder: windows-x86_64-devel

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* The note is regarding required functionality. Part of the point of this package is to present an easy way for researchers (some new to R) to get working with a number of related tables. There are functions for processing different files. These functions are also called by a one-off process_data() function which creates the same six data frames each time it is run. The user is warned and given the opportunity to abort if variables would be overwritten.
