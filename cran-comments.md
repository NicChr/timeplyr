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

  Build ID:   timeplyr_0.2.0.tar.gz-2b25710ff8d74b59a8d636e76ea6954a  
  Platform:   Windows Server 2022, R-devel, 64 bit  
  Submitted:  1h 12m 3.8s ago  
  Build time: 8m 37.2s  

❯ checking CRAN incoming feasibility ... NOTE  
  Maintainer: 'Nick Christofides <nick.christofides.r@gmail.com>'  
    
  New submission  
  
  Possibly misspelled words in DESCRIPTION:  
    Datetime (2:37)  
    tidyverse (8:49)  

❯ checking for non-standard things in the check directory ... NOTE  
  Found the following files/directories:  
    ''NULL''  

❯ checking for detritus in the temp directory ... NOTE  
  Found the following files/directories:  
    'lastMiKTeXException'  

0 errors ✔ | 0 warnings ✔ | 3 notes ✖   

── timeplyr 0.2.0: NOTE  

  Build ID:   timeplyr_0.2.0.tar.gz-0f384b23a3e44d18b31824447322292e  
  Platform:   Ubuntu Linux 20.04.1 LTS, R-release, GCC  
  Submitted:  1h 12m 3.9s ago  
  Build time: 53m 59.9s  

❯ checking CRAN incoming feasibility ... [6s/19s] NOTE  
  Maintainer: ‘Nick Christofides <nick.christofides.r@gmail.com>’  
  
  New submission  
  
  Possibly misspelled words in DESCRIPTION:  
    Datetime (2:37)  
    tidyverse (8:49)  

❯ checking installed package size ... NOTE  
    installed size is  5.1Mb  
    sub-directories of 1Mb or more:  
      libs   4.0Mb  

❯ checking examples ... [44s/80s] NOTE  
  Examples with CPU (user + system) or elapsed time > 5s  
                  user system elapsed  
  time_summarise 3.231  0.044   5.800  
  time_expand    2.973  0.065   5.585  
  time_mutate    2.882  0.034   5.218 
  fexpand        2.746  0.058   5.457  

❯ checking HTML version of manual ... NOTE  
  Skipping checking HTML validation: no command 'tidy' found   

0 errors ✔ | 0 warnings ✔ | 4 notes ✖  

── timeplyr 0.2.0: NOTE  

  Build ID:   timeplyr_0.2.0.tar.gz-8c1cd16fbd574ac399fc5e69cee02869  
  Platform:   Fedora Linux, R-devel, clang, gfortran  
  Submitted:  1h 12m 4s ago  
  Build time: 50m 7.2s  

❯ checking CRAN incoming feasibility ... [7s/20s] NOTE  
  Maintainer: ‘Nick Christofides <nick.christofides.r@gmail.com>’  
  
  New submission  
  
  Possibly misspelled words in DESCRIPTION:  
    Datetime (2:37)  
    tidyverse (8:49)  

❯ checking examples ... [39s/77s] NOTE  
  Examples with CPU (user + system) or elapsed time > 5s  
                  user system elapsed  
  time_expand    3.206  0.108   6.359  
  get_time_delay 2.489  0.094   5.296  

❯ checking HTML version of manual ... NOTE  
  Skipping checking HTML validation: no command 'tidy' found  

0 errors ✔ | 0 warnings ✔ | 3 notes ✖  

── timeplyr 0.2.0: PREPERROR  

  Build ID:   timeplyr_0.2.0.tar.gz-445fb3e235ab4e48ba85182a5afb5eb0  
  Platform:   Debian Linux, R-devel, GCC ASAN/UBSAN  
  Submitted:  1h 12m 4.1s ago  
  Build time: 1h 11m 52s  

❯ Build failed during preparation or aborted  
