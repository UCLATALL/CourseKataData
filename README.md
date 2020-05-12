
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CourseKataData <img src='man/figures/logo.png' align="right" height="138" />

<!-- badges: start -->

[![R build
status](https://github.com/UCLATALL/CourseKataData/workflows/R-CMD-check/badge.svg)](https://github.com/UCLATALL/CourseKataData/actions),
[![Codecov test
coverage](https://codecov.io/gh/UCLATALL/CourseKataData/branch/master/graph/badge.svg)](https://codecov.io/gh/UCLATALL/CourseKataData?branch=master),
[![CRAN
status](https://www.r-pkg.org/badges/version/CourseKataData)](https://CRAN.R-project.org/package=CourseKataData)
<!-- badges: end -->

This is the **source code** for CourseKataData. If you are trying to
learn more about the contents of the actual data, how to use this
package, or how other researchers are using this package, **head to the
[Wiki](https://github.com/UCLATALL/CourseKataData/wiki).**

The goal of CourseKataData is to help researchers working with
CourseKata courses on data science, statistics, and modeling. The data
downloaded from [CourseKata](https://www.coursekata.org) can be useful
in the more-or-less “raw” form that it comes in, but you will usually
want to process the data before working with it. This package includes
useful functions for cleaning and tidying the raw course data from
completed courses.

## Installation

Download the package directly from this repository using devtools:

``` r
library(devtools)
install_github("UCLATALL/CourseKataData")
```

## Usage

This section details the usage of the functions in the CourseKataData
package. If you would like to read more about the actual structure of
the data downloaded from CourseKata, scroll down to the **Data
Structure** section.

### One and Done

The easiest way to get started working with your data is to just
download it and run `process_data` with the path to the zip file (no
need to unzip the file first).

``` r
library(CourseKataData)

process_data("path/to/downloaded/data.zip")
```

If you would like to unzip the file first (which can sometimes be
faster), you can also pass the name of the directory to
`process_data()`. If your data was in `path/to/downloaded/data.zip` and
you extracted it to `path/to/downloaded/data`, then the call should be
to the latter path:

``` r
process_data("path/to/downloaded/data")
```

When you run this function it will load all of the data and create six
data frames (actually [tibbles](https://tibble.tidyverse.org/), but they
work pretty much the same). These are the names of the data frames that
are created:

  - `classes`
  - `responses`
  - `items`
  - `tags`
  - `media_views`
  - `page_views`

If there is already an R object with one of these names, you will be
given the opportunity to abort the processing or overwrite the existing
object.

### Automatically Merge Multiple Downloads

If you have downloaded multiple data download zip files from CourseKata,
don’t worry, this package takes care of merging the files for you. To
load multiple data downloads into R, specify a vector of zip files or
directories to load:

``` r
zip_paths <- c("path/to/first/zip", "path/to/second/zip", "path/to/a/directory")
process_data(zip_paths)
```

#### Time Zones

By default the data is parsed using the “UTC” time zone. If you would
like to convert the date time data in each table to a different time
zone you can specify it. In this example, your computer system’s time
zone will be used:

``` r
process_data("path/to/downloaded/data", time_zone = Sys.timezone())
```

#### Splitting Responses

By default, all of the responses in the course are included in the
`responses` table. These responses can be semantically split into three
parts: the surveys at the beginning and end of the course, the practice
quizzes at the end of each chapter, and the rest of the items in the
text. If you would like to split the responses like this, there is a
handy `split_responses()` function that will return a list of three
tibbles:

``` r
parts <- split_responses(responses)
parts$in_text
parts$quizzes
parts$surveys
```

### Piece by Piece

If you would instead like to process only part of the data, you can use
the helpful sub-process functions that exist for each of the file types
in the data download:

  - `process_classes()`
  - `process_responses()`
  - `process_items()`
  - `process_tags()`
  - `process_media_views()`
  - `process_page_views()`

As with `process_data()`, each of these functions will accept the path
to a CourseKata data download zip file or the directory of the extracted
zip file, or you can simply supply the name of the file that you want to
process with the function.

``` r
# zip file
process_classes("path/to/downloaded/data.zip")

# directory
process_classes("path/to/downloaded/data)
```

The classes data file contains information about each class in the data
download (see more about the structure of the data download and what is
in each file in the **Data Structure** section below). However, the
other files are specific to each class included in the download. If you
want to extract a specific class, you can specify the `class_id`, which
should correspond to the name of the class’s folder in the `classes`
folder of the download.

``` r
# a single class
process_page_views(
  "path/to/downloaded/data.zip", 
  class_id = "dad3c954-3cb5-11ea-b81d-1d385b778009"
)

# two classes
process_page_views(
  "path/to/downloaded/data.zip", 
  class_id = c(
    "dad3c954-3cb5-11ea-b81d-1d385b778009",
    "1a99bb15-4d19-4e3f-bb60-6332141573ed"
  )
)
```

### Into the Deep with Responses

The responses take a significant amount of processing. If you would like
to do this in parts (perhaps to inject your own processing at some point
along the way), each of the finer processing functions are available.
The following is equivalent to `process_responses()`:

``` r
read.csv("path/to/downloaded/data/classes/[class_id].csv") %>% 
  convert_types_in_responses() %>%
  ensure_data_in_responses() %>%
  map_response_options()
```

The part functions are purely functional, which means that they can be
run in any order. If you alter the response data and and a function
doesn’t know how to handle it any more, it will tell you what is missing
or needed.

# Contributing

If you see an issue, problem, or improvement that you think we should
know about, or you think would fit with this package, please let us know
on our [issues page](https://github.com/UCLATALL/CourseKataData/issues).
Alternatively, if you are up for a little coding of your own, submit a
pull request:

1.  Fork it\!
2.  Create your feature branch: `git checkout -b my-new-feature`
3.  Commit your changes: `git commit -am 'Add some feature'`
4.  Push to the branch: `git push origin my-new-feature`
5.  Submit a [pull
    request](https://github.com/UCLATALL/CourseKataData/pulls) :D
