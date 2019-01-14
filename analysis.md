---
title: "Reproducible Research: Peer Assessment 2"
author: Nam D. Nguyen
output:
  prettydoc::html_pretty:
    theme: cayman
    highlight: github
    keep_md: true
---

Load libraries and set code chunk defaults.


```r
library(readr)
library(R.utils)
library(ggplot2)
library(dplyr)
knitr::opts_chunk$set(echo = TRUE)
```

## Data Processing

Load the data and display the first 6 rows.


```r
setwd("./")
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
file <- file.path("data", "storm.csv.bz2")
data <- file.path("data", "storm.csv")

# Create data directory
if(!file.exists("data")) {
  dir.create("data")
}

# Retrieve data file
if(!file.exists(data)) {
  download.file(fileUrl, destfile = file, method = "curl")
  bunzip2(file, data)
  dateDownloaded <- date()
  dateDownloaded
}

# load data
data <- file.path("data", "sample.csv")
df <- read_csv(data, col_types = cols(EVTYPE = col_factor()))
head(df)
```

```
## # A tibble: 6 x 37
##   STATE__ BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE EVTYPE
##     <dbl> <chr>    <chr>    <chr>      <dbl> <chr>      <chr> <fct> 
## 1      51 5/24/20… 03:33:0… EST          550 CHESAPEAK… VA    THUND…
## 2      28 5/20/19… 1608     CST           49 HINDS      MS    TSTM …
## 3      30 1/3/199… 03:30:0… MST          111 YELLOWSTO… MT    FLASH…
## 4      51 4/9/199… 06:35:0… EST           83 HALIFAX    VA    TSTM …
## 5      13 3/2/200… 01:18:0… EST          189 MCDUFFIE   GA    THUND…
## 6      31 6/20/20… 09:00:0… CST           53 DODGE      NE    FLOOD 
## # ... with 29 more variables: BGN_RANGE <dbl>, BGN_AZI <lgl>,
## #   BGN_LOCATI <lgl>, END_DATE <lgl>, END_TIME <lgl>, COUNTY_END <dbl>,
## #   COUNTYENDN <lgl>, END_RANGE <dbl>, END_AZI <lgl>, END_LOCATI <lgl>,
## #   LENGTH <dbl>, WIDTH <dbl>, F <dbl>, MAG <dbl>, FATALITIES <dbl>,
## #   INJURIES <dbl>, PROPDMG <dbl>, PROPDMGEXP <chr>, CROPDMG <dbl>,
## #   CROPDMGEXP <lgl>, WFO <lgl>, STATEOFFIC <lgl>, ZONENAMES <lgl>,
## #   LATITUDE <dbl>, LONGITUDE <dbl>, LATITUDE_E <dbl>, LONGITUDE_ <dbl>,
## #   REMARKS <lgl>, REFNUM <dbl>
```

## Results

```r
harm <- df %>%
  group_by(EVTYPE) %>%
  summarize(FATALITIES_EV = sum(FATALITIES),
            INJURIES_EV = sum(INJURIES)) %>%
  arrange(-FATALITIES_EV, -INJURIES_EV)
```
