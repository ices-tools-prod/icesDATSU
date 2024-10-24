## Test environments

*  Windows 11 x64 (build 22631)
  - R version 4.4.1 (2024-06-14)
* Windows Server 2022 x64 (build 20348)
  - R version 4.4.1 (2024-06-14)
  - R Under development (unstable) (2024-10-22 r87264)
  - R Under development (unstable) (2024-10-22 r87265)
  - R version 4.3.3 (2024-02-29)
* Ubuntu 24.04.1 LTS
  - R Under development (unstable) (2024-10-21 r87258)
* macOS Sonoma 14.7
  - R version 4.4.1 (2024-06-14)
  - R version 4.3.3 (2024-02-29)


## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Colin Millar <colin.millar@ices.dk>'

New submission

Package was archived on CRAN

Possibly misspelled words in DESCRIPTION:
  DATSU (4:10, 6:75)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2024-07-16 for policy violation.

* This package was archived due to \donttest examples relying on a web resource.
  This web resource suffered a cyber attack at the same time as rundonttest checks
  were being performed.
  All examples relying on a web resource have been changed to \dontrun
