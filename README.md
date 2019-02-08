`sedReview`
==========
# Overview
Package for USGS discrete sediment data review and box coefficient development. Currently in Beta testing.

# Package Status
|Linux|Windows| Test Coverage | USGS Status |
|----------|------------|------------|------------|
[![Travis](https://travis-ci.org/USGS-R/sedReview.svg?branch=master)](https://travis-ci.org/USGS-R/sedReview)|[![Windows](https://ci.appveyor.com/api/projects/status/7xfp7x17bpwbdvh2?svg=true)](https://ci.appveyor.com/project/cpenn-usgs/sedreview)|[![Coverage Status](https://coveralls.io/repos/github/USGS-R/sedReview/badge.svg?branch=master)](https://coveralls.io/github/USGS-R/sedReview?branch=master)|[![status](https://img.shields.io/badge/USGS-Support-yellow.svg)](https://owi.usgs.gov/R/packages.html#support)

This package is considered a 'support' package. For more information, see:
[https://owi.usgs.gov/R/packages.html#support](https://owi.usgs.gov/R/packages.html#support)

### Reporting Bugs
Users are encouraged to post any bugs or comments for additional functionality on the issues page at:
[sedReview Issues](https://github.com/USGS-R/sedReview/issues) 

You may also contact the maintainer at cpenn@usgs.gov

## Description
This package facilitates data review and exploration of discrete sediment data. Data is imported with user-specified options for single or multiple sites and parameter codes using an ODBC connection to the user's local NWIS server. A graphical user interface is in development 

## Installation
### R package
```R
install.packages("devtools")
devtools::install_github(repo = "USGS-R/sedReview")
```
### Shiny GUI
```R
sedReview::SedReviewGUI()
```
or  
Download and Install Standalone GUI App from:  
[ftp://ftpint.usgs.gov/private/cr/co/lakewood/SedReview_EXE/](ftp://ftpint.usgs.gov/private/cr/co/lakewood/SedReview_EXE/)

[SedReview GUI User Manual (better version launched from within GUI)](inst/shiny/SedReviewGUI/www/sedReview_manual.md)

## Disclaimer
[Disclaimer](DISCLAIMER.md)


