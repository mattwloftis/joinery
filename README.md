
<!-- README.md is generated from README.Rmd. Please edit that file -->

# joinery

<!-- badges: start -->
<!-- badges: end -->

`joinery` is a set of tools to ease the process of merging (primarily
text) data used in political science research. joinery checks and cleans
common identifiers like country names, political party names, names of
institutions, etc. Once data are cleaned, joinery guides the user to
aggregating or disaggregating the input datasets, executing final joins,
and documenting those joins. See [OPTED](https://opted.eu/) project for
principles and heuristics applied by joinery and for project updates.

## Installation

You can install the development version of joinery from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mattwloftis/joinery")
```

## Example

This is a basic example which shows you how to disambiguate party names
in a dataset from the *Comparative Agendas Project*:

``` r
library(joinery)
## basic example code

spain_cap_manifestos <- read.csv("https://comparativeagendas.s3.amazonaws.com/datasetfiles/Party_Manifestos_CAP_Web_csv.csv", fileEncoding = "UTF-8")

spain_cap_manifestos <- disamb_party(x = spain_cap_manifestos,
                                     party_ref = "politicalparty",
                                     country = "Spain",
                                     year = "year",
                                     origin = NULL)
#> 
#> Countrycode origin format not provided. Assuming: "country.name" (See countrycode package)
#> 
#> -----------------------------------------------------------
#> Found 6 exact matches of 8
#> party x year observations.
#> -----------------------------------------------------------
#> 
#> Party identifier (party_ref) had some party names without exact match in Party Facts data.
#> 
#> ---Trying heuristic matching (English only)---
#> 
#> -----------------------------------------------------------
#> Found 6 exact matches and
#> 2 heuristic matches of 8
#> party x year observations.
#> -----------------------------------------------------------
#> 
#> Note that one or more NAs were present in party, country, or year variables.
```
