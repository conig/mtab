
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mtab

<!-- badges: start -->
<!-- badges: end -->

The goal of mtab is to help create data structures from regression
models which can be used to create publication ready tables.

## Installation

You can install the development version of mtab from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("conig/mtab")
```

## Example

This is a basic example which shows you how to create a hierarchical
regression table using {mtab}:

``` r
library(mtab)

T <- data.frame(datasets::Titanic) |> 
  tidyr::pivot_wider(values_from = "Freq", names_from = "Survived")

T
#> # A tibble: 16 × 5
#>    Class Sex    Age      No   Yes
#>    <fct> <fct>  <fct> <dbl> <dbl>
#>  1 1st   Male   Child     0     5
#>  2 2nd   Male   Child     0    11
#>  3 3rd   Male   Child    35    13
#>  4 Crew  Male   Child     0     0
#>  5 1st   Female Child     0     1
#>  6 2nd   Female Child     0    13
#>  7 3rd   Female Child    17    14
#>  8 Crew  Female Child     0     0
#>  9 1st   Male   Adult   118    57
#> 10 2nd   Male   Adult   154    14
#> 11 3rd   Male   Adult   387    75
#> 12 Crew  Male   Adult   670   192
#> 13 1st   Female Adult     4   140
#> 14 2nd   Female Adult    13    80
#> 15 3rd   Female Adult    89    76
#> 16 Crew  Female Adult     3    20

m1 <- glm(cbind(Yes, No) ~ Sex, data = T, family = "binomial")
m2 <- glm(cbind(Yes, No) ~ Sex + Class, data = T, family = "binomial")
m3 <- glm(cbind(Yes, No) ~ Sex * Class, data = T, family = "binomial")
m4 <- glm(cbind(Yes, No) ~ Sex * Class + Age, data = T, family = "binomial")
tabby <- h_tab(m1, m2, m3, m4)
```

Tables can be formatted with a range of functions including kable() or
papaja::apa_table

``` r
tabby |> 
   knitr::kable(escape = FALSE)
```

| Term                     | OR \[95% CI\]           | lnOR  | SE   | z      | p       | Likelihood Ratio Test               |
|:-------------------------|:------------------------|:------|:-----|:-------|:--------|:------------------------------------|
| **Model 1**              |                         |       |      |        |         |                                     |
|      (Intercept)         | 0.27 \[0.24, 0.30\]     | -1.31 | 0.06 | -22.33 | \< .001 |                                     |
|      SexFemale           | 10.15 \[8.05, 12.86\]   | 2.32  | 0.12 | 19.38  | \< .001 |                                     |
| **Model 2**              |                         |       |      |        |         | $\chi^2$(3) = 106.08, $p$ = \< .001 |
|      (Intercept)         | 0.70 \[0.54, 0.92\]     | -0.35 | 0.14 | -2.60  | .009    |                                     |
|      SexFemale           | 11.26 \[8.60, 14.85\]   | 2.42  | 0.14 | 17.41  | \< .001 |                                     |
|      Class2nd            | 0.39 \[0.26, 0.56\]     | -0.95 | 0.19 | -4.90  | \< .001 |                                     |
|      Class3rd            | 0.19 \[0.14, 0.26\]     | -1.66 | 0.17 | -9.88  | \< .001 |                                     |
|      ClassCrew           | 0.41 \[0.30, 0.56\]     | -0.88 | 0.16 | -5.61  | \< .001 |                                     |
| **Model 3**              |                         |       |      |        |         | $\chi^2$(3) = 65.18, $p$ = \< .001  |
|      (Intercept)         | 0.53 \[0.38, 0.71\]     | -0.64 | 0.16 | -4.10  | \< .001 |                                     |
|      SexFemale           | 67.09 \[26.71, 225.74\] | 4.21  | 0.53 | 7.92   | \< .001 |                                     |
|      Class2nd            | 0.31 \[0.18, 0.52\]     | -1.17 | 0.27 | -4.40  | \< .001 |                                     |
|      Class3rd            | 0.40 \[0.27, 0.58\]     | -0.92 | 0.20 | -4.72  | \< .001 |                                     |
|      ClassCrew           | 0.55 \[0.39, 0.77\]     | -0.61 | 0.18 | -3.43  | .001    |                                     |
|      SexFemale:Class2nd  | 0.66 \[0.17, 2.18\]     | -0.42 | 0.64 | -0.65  | .515    |                                     |
|      SexFemale:Class3rd  | 0.06 \[0.02, 0.16\]     | -2.80 | 0.56 | -4.98  | \< .001 |                                     |
|      SexFemale:ClassCrew | 0.35 \[0.07, 1.93\]     | -1.06 | 0.82 | -1.29  | .196    |                                     |
| **Model 4**              |                         |       |      |        |         | $\chi^2$(1) = 20.34, $p$ = \< .001  |
|      (Intercept)         | 1.46 \[0.85, 2.49\]     | 0.38  | 0.27 | 1.38   | .166    |                                     |
|      SexFemale           | 68.93 \[27.43, 232.02\] | 4.23  | 0.53 | 7.97   | \< .001 |                                     |
|      Class2nd            | 0.29 \[0.17, 0.49\]     | -1.23 | 0.27 | -4.58  | \< .001 |                                     |
|      Class3rd            | 0.36 \[0.24, 0.53\]     | -1.02 | 0.20 | -5.14  | \< .001 |                                     |
|      ClassCrew           | 0.56 \[0.40, 0.80\]     | -0.57 | 0.18 | -3.23  | .001    |                                     |
|      AgeAdult            | 0.35 \[0.22, 0.55\]     | -1.05 | 0.23 | -4.57  | \< .001 |                                     |
|      SexFemale:Class2nd  | 0.64 \[0.16, 2.13\]     | -0.45 | 0.65 | -0.69  | .488    |                                     |
|      SexFemale:Class3rd  | 0.06 \[0.02, 0.16\]     | -2.86 | 0.56 | -5.08  | \< .001 |                                     |
|      SexFemale:ClassCrew | 0.34 \[0.07, 1.87\]     | -1.09 | 0.82 | -1.33  | .185    |                                     |
