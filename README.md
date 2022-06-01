
<!-- README.md is generated from README.Rmd. Please edit that file -->

# joinery

<!-- badges: start -->
<!-- badges: end -->

`joinery` is a set of tools to ease the process of merging (primarily
text) data used in political science research. `joinery` can help with
the following common tasks:

-   check and cleans common identifiers like country names, political
    party names, names of institutions, etc.
-   aggregate or disaggregate the input datasets
-   execute joins
-   document each data transformation

See the [OPTED](https://opted.eu/) project for principles and heuristics
applied by joinery and for project updates.

## Installation

You can install the development version of joinery from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mattwloftis/joinery")
```

## Example: Disambiguating party names

This is a basic example which shows you how to disambiguate party names
in a dataset from the *Comparative Agendas Project*:

``` r
library(joinery)

## basic example of disambiguating party names

# Comparative Agendas Project data on Spanish political party manifestos
spain_cap_manifestos <- read.csv("https://comparativeagendas.s3.amazonaws.com/datasetfiles/Party_Manifestos_CAP_Web_csv.csv", fileEncoding = "UTF-8")

# disambiguate the party names
spain_cap_manifestos <- spain_cap_manifestos %>% 
  disamb_party(party_ref = "politicalparty",
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

# review the types of matches achieved
table(spain_cap_manifestos$politicalparty, spain_cap_manifestos$jnry_match)
#>       
#>        exact heuristic
#>   PP       0      4155
#>   PSOE 12125         0

# see Wikipedia links for the more uncertain 'heuristic' matches
spain_cap_manifestos %>% 
  dplyr::filter(jnry_match %in% 'heuristic') %>% 
  dplyr::select(politicalparty, partyfacts_name, wikipedia)
#> # A tibble: 4,155 × 3
#>    politicalparty partyfacts_name wikipedia                                     
#>    <chr>          <chr>           <chr>                                         
#>  1 PP             AP/PP           https://en.wikipedia.org/wiki/People%27s_Part…
#>  2 PP             AP/PP           https://en.wikipedia.org/wiki/People%27s_Part…
#>  3 PP             AP/PP           https://en.wikipedia.org/wiki/People%27s_Part…
#>  4 PP             AP/PP           https://en.wikipedia.org/wiki/People%27s_Part…
#>  5 PP             AP/PP           https://en.wikipedia.org/wiki/People%27s_Part…
#>  6 PP             AP/PP           https://en.wikipedia.org/wiki/People%27s_Part…
#>  7 PP             AP/PP           https://en.wikipedia.org/wiki/People%27s_Part…
#>  8 PP             AP/PP           https://en.wikipedia.org/wiki/People%27s_Part…
#>  9 PP             AP/PP           https://en.wikipedia.org/wiki/People%27s_Part…
#> 10 PP             AP/PP           https://en.wikipedia.org/wiki/People%27s_Part…
#> # … with 4,145 more rows
```

The `disamb_party` function takes a data frame as input. The user
identifies the variable containing political party names or acronyms and
provides either the country and year of each observation or identifies
the variables containing those information. With that, the function
returns a data frame with several new variables:

-   `partyfacts_id` - the unique numeric id of each recognized political
    party in the [Party Facts](https://partyfacts.herokuapp.com/). The
    Party Facts id allows users to link their data to the Party Facts
    data, which in turn, allows users to link their data to dozens of
    political science datasets.
-   `partyfacts_name` - the name of each recognized political party in
    the Party Facts data
-   `wikipedia` - In the case of heuristic or fuzzy matches, `joinery`
    returns a link to the Wikipedia page of the presumed match so the
    user can review the match for accuracy.
-   `jnry_match` - The nature of the match for each observation. This
    will be one of:
    -   `exact` - the party name, country, and year lead to an exact
        match with Party Facts
    -   `heuristic` - minor changes to the party name lead to a match
    -   `fuzzy` - the input party name, or minor transformations to it,
        are very similar to a match
-   `jnry_country` - disambiguated country name (`joinery` uses the
    tools in the
    [`countrycode`](https://github.com/vincentarelbundock/countrycode)
    package to identify country names, and includes some heuristics to
    address mismatches)
-   `jnry_year` - the four-digit year of each observation

## Example: Aggregating data over time

Harmonizing time scales can be a tedious part of preparing to join data
sets. `joinery` eases this step by executing aggregations for multiple
common time scales and enforcing ISO 8601 time formatting standards to
ease later joins. `aggregate_time` has several useful features:

-   accepts time inputs in multiple formats (date, month, week, etc.)
-   can aggregate data to user-provided adhoc time scales
-   if needed, it will fill time series with missing data for time
    points not represented in the input data

Here are some basic examples of how to use `joinery` to aggregate data
to different time scales.

``` r
library(joinery)

## basic example of aggregating data

# pull in the NYC flight data set & create a date-time variable
nycf <- nycflights13::flights %>%
  dplyr::filter(!is.na(arr_time)) %>%
  dplyr::mutate(
    arr_string = stringr::str_c(stringr::str_pad(string = month, width = 2, side = "left", pad = "0"),
                                stringr::str_pad(string = day, width = 2, side = "left", pad = "0"), year,
                                stringr::str_pad(string = arr_time, width = 4, side = 'left', pad = '0')),
    arrival = lubridate::mdy_hm(arr_string, tz = "EST"))

# mean of all numeric variables by month
nycf %>% 
  aggregate_time(time = 'arrival', 
                 granularity = 'month', 
                 aggregation = 'mean')
#> # A tibble: 13 × 15
#>    jnry_month year_mean month_mean day_mean dep_time_mean sched_dep_time_mean
#>    <chr>          <dbl>      <dbl>    <dbl>         <dbl>               <dbl>
#>  1 2013-01         2013       1        15.8         1347.               1339.
#>  2 2013-02         2013       2.00     15.0         1348.               1340.
#>  3 2013-03         2013       3.00     16.1         1359.               1352.
#>  4 2013-04         2013       4.00     15.4         1353.               1345.
#>  5 2013-05         2013       5        15.9         1351.               1341.
#>  6 2013-06         2013       6        15.5         1350.               1336.
#>  7 2013-07         2013       7.00     16.2         1352.               1340.
#>  8 2013-08         2013       8.00     15.9         1350.               1342.
#>  9 2013-09         2013       9.00     15.7         1334.               1331.
#> 10 2013-10         2013      10        16.0         1340.               1335.
#> 11 2013-11         2013      11        15.3         1345.               1342.
#> 12 2013-12         2013      12        15.9         1357.               1342.
#> 13 2014-01         2013      12        31           1915                1922 
#> # … with 9 more variables: dep_delay_mean <dbl>, arr_time_mean <dbl>,
#> #   sched_arr_time_mean <dbl>, arr_delay_mean <dbl>, flight_mean <dbl>,
#> #   air_time_mean <dbl>, distance_mean <dbl>, hour_mean <dbl>,
#> #   minute_mean <dbl>

# count of flights by week & flight origin
nycf %>% 
  aggregate_time(time = 'arrival', 
                 grouping_vars = 'origin',
                 granularity = 'week', 
                 aggregation = 'count')
#> # A tibble: 157 × 3
#>    origin jnry_week jnry_count
#>    <chr>  <chr>          <int>
#>  1 EWR    2013-W01        2459
#>  2 EWR    2013-W02        2217
#>  3 EWR    2013-W03        2183
#>  4 EWR    2013-W04        2177
#>  5 EWR    2013-W05        2058
#>  6 EWR    2013-W06        1879
#>  7 EWR    2013-W07        2236
#>  8 EWR    2013-W08        2273
#>  9 EWR    2013-W09        2293
#> 10 EWR    2013-W10        2156
#> # … with 147 more rows

# maximum of all numeric variables by quarter & flight origin
nycf %>% 
  aggregate_time(time = 'arrival', 
                 grouping_vars = 'origin',
                 granularity = 'quarter', 
                 aggregation = 'max')
#> # A tibble: 13 × 16
#>    origin jnry_quarter year_max month_max day_max dep_time_max sched_dep_time_m…
#>    <chr>  <chr>           <int>     <int>   <int>        <int>             <int>
#>  1 EWR    2013-Q1          2013         3      31         2359              2339
#>  2 EWR    2013-Q2          2013         6      31         2359              2345
#>  3 EWR    2013-Q3          2013         9      31         2400              2159
#>  4 EWR    2013-Q4          2013        12      31         2359              2330
#>  5 JFK    2013-Q1          2013         3      31         2400              2359
#>  6 JFK    2013-Q2          2013         6      31         2400              2359
#>  7 JFK    2013-Q3          2013         9      31         2400              2359
#>  8 JFK    2013-Q4          2013        12      31         2400              2359
#>  9 JFK    2014-Q1          2013        12      31         1915              1922
#> 10 LGA    2013-Q1          2013         3      31         2359              2225
#> 11 LGA    2013-Q2          2013         6      31         2359              2225
#> 12 LGA    2013-Q3          2013         9      31         2400              2225
#> 13 LGA    2013-Q4          2013        12      31         2400              2200
#> # … with 9 more variables: dep_delay_max <dbl>, arr_time_max <int>,
#> #   sched_arr_time_max <int>, arr_delay_max <dbl>, flight_max <int>,
#> #   air_time_max <dbl>, distance_max <dbl>, hour_max <dbl>, minute_max <dbl>

# minimum of all numeric variables by quarter & flight origin & flight carrier
nycf %>% 
  aggregate_time(time = 'arrival', 
                 grouping_vars = c('origin', 'carrier'),
                 granularity = 'quarter', 
                 aggregation = 'max')
#> # A tibble: 137 × 17
#>    origin carrier jnry_quarter year_max month_max day_max dep_time_max
#>    <chr>  <chr>   <chr>           <int>     <int>   <int>        <int>
#>  1 EWR    9E      2013-Q1          2013         3      31         1939
#>  2 EWR    9E      2013-Q2          2013         6      31         2045
#>  3 EWR    9E      2013-Q3          2013         9      31         2100
#>  4 EWR    9E      2013-Q4          2013        12      31         1930
#>  5 EWR    AS      2013-Q1          2013         3      31         2157
#>  6 EWR    AS      2013-Q2          2013         6      31         2205
#>  7 EWR    AS      2013-Q3          2013         9      31         1958
#>  8 EWR    AS      2013-Q4          2013        12      31         2101
#>  9 EWR    B6      2013-Q1          2013         3      31         2359
#> 10 EWR    B6      2013-Q2          2013         6      31         2355
#> # … with 127 more rows, and 10 more variables: sched_dep_time_max <int>,
#> #   dep_delay_max <dbl>, arr_time_max <int>, sched_arr_time_max <int>,
#> #   arr_delay_max <dbl>, flight_max <int>, air_time_max <dbl>,
#> #   distance_max <dbl>, hour_max <dbl>, minute_max <dbl>

# sum of all numeric variables and count of flights by quarter
# (just chain together and join calls)
nycf %>% 
  aggregate_time(time = 'arrival', 
                 granularity = 'quarter', 
                 aggregation = 'sum') %>% 
  dplyr::left_join(
    nycf %>% 
      aggregate_time(time = 'arrival', 
                     granularity = 'quarter', 
                     aggregation = 'count')
  )
#> Joining, by = "jnry_quarter"
#> # A tibble: 5 × 16
#>   jnry_quarter  year_sum month_sum day_sum dep_time_sum sched_dep_time_sum
#>   <chr>            <int>     <int>   <int>        <int>              <int>
#> 1 2013-Q1      157154910    157614 1224285    105524138          104926389
#> 2 2013-Q2      167050818    414478 1295382    112131228          111244155
#> 3 2013-Q3      169643562    672878 1342480    113403832          112716430
#> 4 2013-Q4      166539516    908485 1301433    111449194          110842579
#> 5 2014-Q1           2013        12      31         1915               1922
#> # … with 10 more variables: dep_delay_sum <dbl>, arr_time_sum <int>,
#> #   sched_arr_time_sum <int>, arr_delay_sum <dbl>, flight_sum <int>,
#> #   air_time_sum <dbl>, distance_sum <dbl>, hour_sum <dbl>, minute_sum <dbl>,
#> #   jnry_count <int>
```
