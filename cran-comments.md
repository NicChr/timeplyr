* This is a new release.

Tested on multiple machines, all pass except for Debian linux, which 
might be an issue with that test environment.

## Test environments

  - R-hub windows-x86_64-devel (r-devel)
  - R-hub ubuntu-gcc-release (r-release)
  - R-hub fedora-clang-devel (r-devel)
  - R-hub linux-x86_64-rocker-gcc-san

## R CMD check results

  Build ID:   timeplyr_0.2.0.tar.gz-8a43e44291fa46dda7cccc915c492b43  
  Platform:   Windows Server 2022, R-devel, 64 bit  
  Submitted:  53m 25.3s ago  
  Build time: 7m 54.7s  

  checking CRAN incoming feasibility ... NOTE
Maintainer: 'Nick Christofides <nick.christofides.r@gmail.com>'

New submission

Possibly misspelled words in DESCRIPTION:  
  Datetime (2:37)  
  tidyverse (8:49)  
  
checking for non-standard things in the check directory ... NOTE  

Found the following files/directories:  

checking for detritus in the temp directory ... NOTE  

Found the following files/directories:  
  'lastMiKTeXException'  

0 errors v | 0 warnings v | 3 notes x  
 
-- timeplyr 0.2.0: NOTE  

  Build ID:   timeplyr_0.2.0.tar.gz-98dadc8f964c4a44b6bb644a8540994b  
  Platform:   Ubuntu Linux 20.04.1 LTS, R-release, GCC  
  Submitted:  53m 25.3s ago  
  Build time: 42m 7.1s  

 checking CRAN incoming feasibility ... [6s/14s] NOTE  
 
  Maintainer: ‘Nick Christofides <nick.christofides.r@gmail.com>’  
  
  New submission  
  
  Possibly misspelled words in DESCRIPTION:  
    Datetime (2:37)  
    tidyverse (8:49)  

 checking installed package size ... NOTE  
    installed size is  5.1Mb  
    sub-directories of 1Mb or more:  
      libs   4.0Mb  

 checking HTML version of manual ... NOTE  
  Skipping checking HTML validation: no command 'tidy' found  

0 errors v | 0 warnings v | 3 notes x  

-- timeplyr 0.2.0: NOTE  

  Build ID:   timeplyr_0.2.0.tar.gz-e8b1e5386d6c433c884139adeed6d8aa  
  Platform:   Fedora Linux, R-devel, clang, gfortran  
  Submitted:  53m 25.4s ago  
  Build time: 39m 30.2s  

 checking CRAN incoming feasibility ... [7s/17s] NOTE   
  Maintainer: ‘Nick Christofides <nick.christofides.r@gmail.com>’  
  
  New submission  
  
  Possibly misspelled words in DESCRIPTION:  
    Datetime (2:37)  
    tidyverse (8:49)  

 checking HTML version of manual ... NOTE  
  Skipping checking HTML validation: no command 'tidy' found  

0 errors v | 0 warnings v | 2 notes x  

-- timeplyr 0.2.0: PREPERROR  

  Build ID:   timeplyr_0.2.0.tar.gz-b179a46b247b48b69a583506cfd1947f  
  Platform:   Debian Linux, R-devel, GCC ASAN/UBSAN  
  Submitted:  53m 25.5s ago  
  Build time: 51m 54.2s  

 Build failed during preparation or aborted  
