---
layout: default
title: Data - Chapter 12
---

# Data for Chapter 12: Non i.i.d. Data

This page lists the datasets used in the R scripts for this chapter.

## Datasets

### Time Series Data
- **temperature.csv**: Average global/local temperature records. [[Download]](../data/temperature.csv)
- **DAX-2.csv**: Historical prices for the DAX stock index. [[Download]](../data/DAX-2.csv)
- **temperatur-augsburg.csv**: Specific temperature records for Augsburg. [[Download]](../data/temperatur-augsburg.csv)
- **Usage**: These datasets are analyzed in `time-series.R` for trends and autocorrelation.

### Miete-22.dat
- **Description**: Rent index data (Mietspiegel) used for loglinear modeling.
- **File**: [[Download]](../data/Miete-22.dat)
- **Usage**: Used in `loglinear.R`.

### UKfaculty (Network Data)
- **Description**: Friendship network among 81 faculty members at a UK university.
- **Source**: Part of the `igraphdata` R package.
- **Usage**: Analyzed in `Network.R` for ERGM and LSM modeling.

## Scripts in this Chapter
- **time-series.R**: Analyzes temporal dependence in stock and climate data.
- **Network.R**: Investigates faculty social networks.
- **loglinear.R**: Fits loglinear models to categorical rent data.
