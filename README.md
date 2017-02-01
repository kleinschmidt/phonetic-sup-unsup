# Phonetic distributional learning data

## Installation

```r
library(devtools)
devtools::install_github('kleinschmidt/phonetic-sup-unsup')
```

## Datasets

```r
> data(package='supunsup')

Data sets in package ‘supunsup’:

excludes                      Subjects excluded from analysis (Experiment 1)
prior_stats                   Some reasonable priors on category means/variances
separatemeans                 Supervised/unsupervised phonetic adaptation (Experiment 2)
separatemeans_assignments     Summary of all Experiment 2 assignments
separatemeans_clean           Data from non-excluded assignments (Expt. 2)
separatemeans_excluded        Data from excluded assignments (Expt. 2)
supunsup                      Supervised/unserupvised phonetic adaptation (Experiment 1)
supunsup_clean                Non-excluded assignments (Expt. 1)
supunsup_excluded             Data from excluded assignments (Expt. 1)
```

## Usage

There are three ways to access datasets:

```r
## direct reference
supunsup::supunsup_clean

## import and direct reference
library(supunsup)
supunsup_clean

## using `data`
data('supunsup_clean', package='supunsup')
```
