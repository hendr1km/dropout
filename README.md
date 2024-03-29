# dropout <img src="man/figures/logo.png" align="right" width="150" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/hendr1km/dropout/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hendr1km/dropout/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/dropout)](https://CRAN.R-project.org/package=dropout)
<!-- badges: end -->

The `dropout` package offers robust tools for dropout analysis in survey data. It helps you identify and handle incomplete responses effectively to ensure the quality of your research findings.

## Issues & Support

If you encounter any issues with the software, please report them in the GitHub Repository within the Issues section https://github.com/hendr1km/dropout/issues. This will help to identify and address problems more efficiently. If you wish to contribute to the software please use a pull request. 
Additionally, if you require support with the software or have any related inquiries, please feel free to contact me at hendrik.mann@uni-wuppertal.de. Your feedback and questions are valuable for the future development of the package.


## Installation

You can install the development version of `dropout` from GitHub using the following command:

```r
# Install the released version from CRAN
install.packages("dropout")

# development version from GitHub:
devtools::install_github("hendr1km/dropout")
```

## Features

- **drop_detect**: Detects participants who drop out of the survey by recognizing NA sequences up to the last question of the survey. Additionally, the function provides the column name and index where the dropout occurs.

- **dropout_summary**: Offers a high-level summary of dropout occurrences, providing key statistics to understand the patterns of participant dropouts across different survey questions.



### Using `drop_detect`

```r
library(dropout)
library(dplyr) # not necessary, but recommended

# Detect dropouts in the 'flying' dataset up to the column "location_census_region"
drop_detect(flying, "location_census_region") %>%
  bind_cols(flying, .) %>%
  filter(dropout == FALSE | dropout_index > 22)
```

### Using `dropout_summary`

```r
library(dropout)

# Summarizing dropouts in the 'flying' dataset up to the column "location_census_region"
dropout_summary(flying, "location_census_region")
```

## Further Reading

For more details, practical examples, and tips on interpreting summary statistics, please refer to the package [vignette](https://hendr1km.github.io/dropout/articles/intro_to_dropout.html).

