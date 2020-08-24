`sedReview 1.2`
==========
# Overview
Package for USGS discrete sediment data review and box coefficient development.
Version 1.2 is released as a tagged release and stand-alone GUI App.

# Package Status
|Linux|Windows| Test Coverage | USGS Status |
|----------|------------|------------|------------|
[![Travis](https://travis-ci.org/USGS-R/sedReview.svg?branch=master)](https://travis-ci.org/USGS-R/sedReview)|[![Windows](https://ci.appveyor.com/api/projects/status/7xfp7x17bpwbdvh2?svg=true)](https://ci.appveyor.com/project/cpenn-usgs/sedreview)|[![Coverage Status](https://coveralls.io/repos/github/USGS-R/sedReview/badge.svg?branch=master)](https://coveralls.io/github/USGS-R/sedReview?branch=master)|[![status](https://img.shields.io/badge/USGS-Support-yellow.svg)](https://owi.usgs.gov/R/packages.html#support)

This package is considered a 'support' package. For more information, see:
[https://owi.usgs.gov/R/packages.html#support](https://owi.usgs.gov/R/packages.html#support)

### Reporting Bugs
Users are encouraged to post any bugs or comments for additional functionality on the issues page at:
[sedReview Issues](https://github.com/USGS-R/sedReview/issues) 

You may also contact the developers at GS-W SedReview Help (gs-w_sedreview_help@usgs.gov)

## Description
This package facilitates data review and exploration of discrete sediment data. Data is imported with user-specified options for single or multiple sites and parameter codes using an ODBC connection to the user's local NWIS server. A Shiny application graphical user interface for site-level assessments and science-center reviews displays data tables and plots to aid in the review process. 

## Installation  
**The GUI requires that Google Chrome or MS Edge be set as your default web browser.**
### R package
```R
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github(repo = "USGS-R/sedReview", 
                        dependencies = TRUE)
```
### Shiny GUI
```R
sedReview::SedReviewGUI()
```
or  
Download and Install Standalone GUI App from (currently for SedReview version 1.2:  
[Available to USGS personnel through ScienceBase](https://www.sciencebase.gov/catalog/item/5c867842e4b09388244b3cb3)

[SedReview GUI User Manual (better version launched from within GUI)](inst/shiny/SedReviewGUI/www/sedReview_manual.md)

## Disclaimer
[Disclaimer](DISCLAIMER.md)


