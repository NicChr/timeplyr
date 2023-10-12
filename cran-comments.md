* This is a new release.

Resubmitted due to initially not passing automatic checks.

Tested on multiple machines, all pass except for Debian linux, which 
might be an issue with that test environment.

## Test environments

  - R-hub windows-x86_64-devel (r-devel)
  - R-hub ubuntu-gcc-release (r-release)
  - R-hub fedora-clang-devel (r-devel)
  - R-hub linux-x86_64-rocker-gcc-san

## R CMD check results

── timeplyr 0.2.0: NOTE

  Build ID:   timeplyr_0.2.0.tar.gz-a20bc86c5c924b79a48619ea481af5b6
  Platform:   Windows Server 2022, R-devel, 64 bit
  Submitted:  1h 4m 17.2s ago
  Build time: 9m 37.4s

❯ checking CRAN incoming feasibility ... [12s] NOTE
  Maintainer: 'Nick Christofides <nick.christofides.r@gmail.com>'
  
  New submission

❯ checking for non-standard things in the check directory ... NOTE
  

❯ checking for detritus in the temp directory ... NOTE
    ''NULL''
  Found the following files/directories:
    'lastMiKTeXException'

0 errors ✔ | 0 warnings ✔ | 3 notes ✖

── timeplyr 0.2.0: NOTE

  Build ID:   timeplyr_0.2.0.tar.gz-33687eed70b04a378c626c62f7050b7f
  Platform:   Ubuntu Linux 20.04.1 LTS, R-release, GCC
  Submitted:  1h 4m 17.3s ago
  Build time: 53m 32.2s

❯ checking CRAN incoming feasibility ... [6s/17s] NOTE
  Maintainer: ‘Nick Christofides <nick.christofides.r@gmail.com>’
  
  New submission

❯ checking installed package size ... NOTE
    installed size is  5.1Mb
    sub-directories of 1Mb or more:
      libs   4.0Mb

❯ checking examples ... [36s/57s] NOTE
  Examples with CPU (user + system) or elapsed time > 5s
                  user system elapsed
  time_summarise 3.098  0.001    5.35

❯ checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

0 errors ✔ | 0 warnings ✔ | 4 notes ✖

── timeplyr 0.2.0: NOTE

  Build ID:   timeplyr_0.2.0.tar.gz-d90a6ccaee2b4f65a902b82b26e85a31
  Platform:   Fedora Linux, R-devel, clang, gfortran
  Submitted:  1h 4m 17.5s ago
  Build time: 49m 53.7s

❯ checking CRAN incoming feasibility ... [7s/20s] NOTE
  Maintainer: ‘Nick Christofides <nick.christofides.r@gmail.com>’
  
  New submission

❯ checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

0 errors ✔ | 0 warnings ✔ | 2 notes ✖

── timeplyr 0.2.0: IN-PROGRESS

  Build ID:   timeplyr_0.2.0.tar.gz-2e55364ec02a48e99ef7436d6d586526
  Platform:   Debian Linux, R-devel, GCC ASAN/UBSAN
  Submitted:  1h 4m 17.5s ago

> ch$print()

── timeplyr 0.2.0: NOTE

  Build ID:   timeplyr_0.2.0.tar.gz-a20bc86c5c924b79a48619ea481af5b6
  Platform:   Windows Server 2022, R-devel, 64 bit
  Submitted:  1h 8m 21.2s ago
  Build time: 9m 37.4s

❯ checking CRAN incoming feasibility ... [12s] NOTE
  Maintainer: 'Nick Christofides <nick.christofides.r@gmail.com>'
  
  New submission

❯ checking for non-standard things in the check directory ... NOTE
  

❯ checking for detritus in the temp directory ... NOTE
    ''NULL''
  Found the following files/directories:
    'lastMiKTeXException'

0 errors ✔ | 0 warnings ✔ | 3 notes ✖

── timeplyr 0.2.0: NOTE

  Build ID:   timeplyr_0.2.0.tar.gz-33687eed70b04a378c626c62f7050b7f
  Platform:   Ubuntu Linux 20.04.1 LTS, R-release, GCC
  Submitted:  1h 8m 21.3s ago
  Build time: 53m 32.2s

❯ checking CRAN incoming feasibility ... [6s/17s] NOTE
  Maintainer: ‘Nick Christofides <nick.christofides.r@gmail.com>’
  
  New submission

❯ checking installed package size ... NOTE
    installed size is  5.1Mb
    sub-directories of 1Mb or more:
      libs   4.0Mb

❯ checking examples ... [36s/57s] NOTE
  Examples with CPU (user + system) or elapsed time > 5s
                  user system elapsed
  time_summarise 3.098  0.001    5.35

❯ checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

0 errors ✔ | 0 warnings ✔ | 4 notes ✖

── timeplyr 0.2.0: NOTE

  Build ID:   timeplyr_0.2.0.tar.gz-d90a6ccaee2b4f65a902b82b26e85a31
  Platform:   Fedora Linux, R-devel, clang, gfortran
  Submitted:  1h 8m 21.4s ago
  Build time: 49m 53.7s

❯ checking CRAN incoming feasibility ... [7s/20s] NOTE
  Maintainer: ‘Nick Christofides <nick.christofides.r@gmail.com>’
  
  New submission

❯ checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

0 errors ✔ | 0 warnings ✔ | 2 notes ✖

── timeplyr 0.2.0: PREPERROR

  Build ID:   timeplyr_0.2.0.tar.gz-2e55364ec02a48e99ef7436d6d586526
  Platform:   Debian Linux, R-devel, GCC ASAN/UBSAN
  Submitted:  1h 8m 21.5s ago
  Build time: 1h 7m 34.1s

❯ Build failed during preparation or aborted

[...]
    CRAN: https://cloud.r-project.org
** testing if installed package can be loaded from temporary location
'getOption("repos")' replaces Bioconductor standard repositories, see
'help("repositories", package = "BiocManager")' for details.
Replacement repositories:
    CRAN: https://cloud.r-project.org
** testing if installed package can be loaded from final location
'getOption("repos")' replaces Bioconductor standard repositories, see
'help("repositories", package = "BiocManager")' for details.
Replacement repositories:
    CRAN: https://cloud.r-project.org
** testing if installed package keeps a record of temporary installation path
* creating tarball
packaged installation of ‘waldo’ as ‘waldo_0.5.1_R_x86_64-pc-linux-gnu.tar.gz’
* DONE (waldo)
'getOption("repos")' replaces Bioconductor standard repositories, see
'help("repositories", package = "BiocManager")' for details.
Replacement repositories:
    CRAN: https://cloud.r-project.org
* installing *source* package ‘tidyr’ ...
** package ‘tidyr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Debian 12.2.0-14) 12.2.0’
using C++11
g++ -fsanitize=undefined,bounds-strict -fno-omit-frame-pointer -std=gnu++11 -I"/usr/local/lib/R/include" -DNDEBUG  -I'/home/docker/R/cpp11/include' -I/usr/local/include    -fpic  -g -O2 -Wall -pedantic -mtune=native  -c cpp11.cpp -o cpp11.o
g++ -fsanitize=undefined,bounds-strict -fno-omit-frame-pointer -std=gnu++11 -I"/usr/local/lib/R/include" -DNDEBUG  -I'/home/docker/R/cpp11/include' -I/usr/local/include    -fpic  -g -O2 -Wall -pedantic -mtune=native  -c melt.cpp -o melt.o
g++ -fsanitize=undefined,bounds-strict -fno-omit-frame-pointer -std=gnu++11 -I"/usr/local/lib/R/include" -DNDEBUG  -I'/home/docker/R/cpp11/include' -I/usr/local/include    -fpic  -g -O2 -Wall -pedantic -mtune=native  -c simplifyPieces.cpp -o simplifyPieces.o
g++ -fsanitize=undefined,bounds-strict -fno-omit-frame-pointer -std=gnu++11 -shared -L/usr/local/lib/R/lib -L/usr/local/lib -o tidyr.so cpp11.o melt.o simplifyPieces.o -L/usr/local/lib/R/lib -lR
installing to /home/docker/R/00LOCK-tidyr/00new/tidyr/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
'getOption("repos")' replaces Bioconductor standard repositories, see
'help("repositories", package = "BiocManager")' for details.
Replacement repositories:
    CRAN: https://cloud.r-project.org
** help
*** installing help indices
*** copying figures
** building package indices
'getOption("repos")' replaces Bioconductor standard repositories, see
'help("repositories", package = "BiocManager")' for details.
Replacement repositories:
    CRAN: https://cloud.r-project.org
** installing vignettes
** testing if installed package can be loaded from temporary location
'getOption("repos")' replaces Bioconductor standard repositories, see
'help("repositories", package = "BiocManager")' for details.
Replacement repositories:
    CRAN: https://cloud.r-project.org
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
'getOption("repos")' replaces Bioconductor standard repositories, see
'help("repositories", package = "BiocManager")' for details.
Replacement repositories:
    CRAN: https://cloud.r-project.org
** testing if installed package keeps a record of temporary installation path
* creating tarball
packaged installation of ‘tidyr’ as ‘tidyr_1.3.0_R_x86_64-pc-linux-gnu.tar.gz’
* DONE (tidyr)
'getOption("repos")' replaces Bioconductor standard repositories, see
'help("repositories", package = "BiocManager")' for details.
Replacement repositories:
    CRAN: https://cloud.r-project.org
ERROR: dependency ‘digest’ is not available for package ‘testthat’
* removing ‘/home/docker/R/testthat’

The downloaded source packages are in
	‘/tmp/RtmpRloLYl/downloaded_packages’
Running `R CMD build`...
* checking for file ‘/tmp/RtmpRloLYl/remotes1361c20623f/timeplyr/DESCRIPTION’ ... OK
* preparing ‘timeplyr’:
* checking DESCRIPTION meta-information ... OK
* cleaning src
* checking for LF line-endings in source and make files and shell scripts
* checking for empty or unneeded directories
* building ‘timeplyr_0.2.0.tar.gz’
Installing package into ‘/home/docker/R’
(as ‘lib’ is unspecified)
'getOption("repos")' replaces Bioconductor standard repositories, see
'help("repositories", package = "BiocManager")' for details.
Replacement repositories:
    CRAN: https://cloud.r-project.org
ERROR: dependency ‘collapse’ is not available for package ‘timeplyr’
* removing ‘/home/docker/R/timeplyr’
There were 13 warnings (use warnings() to see them)
> 
> 
'getOption("repos")' replaces Bioconductor standard repositories, see
'help("repositories", package = "BiocManager")' for details.
Replacement repositories:
    CRAN: https://cloud.r-project.org
> library(timeplyr)
Error in library(timeplyr) : there is no package called ‘timeplyr’
Execution halted
Build step 'Execute shell' marked build as failure
