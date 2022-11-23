
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

tabby |> 
   knitr::kable(escape = FALSE) |> 
   kableExtra::add_indent(which(attr(tabby, "indent")[[1]])) |> 
   kableExtra::kable_classic()
```

<table class=" lightable-classic" style="font-family: &quot;Arial Narrow&quot;, &quot;Source Sans Pro&quot;, sans-serif; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Term
</th>
<th style="text-align:left;">
OR \[95% CI\]
</th>
<th style="text-align:left;">
lnOR
</th>
<th style="text-align:left;">
SE
</th>
<th style="text-align:left;">
z
</th>
<th style="text-align:left;">
p
</th>
<th style="text-align:left;">
Likelihood Ratio Test
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Model 1
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
(Intercept)
</td>
<td style="text-align:left;">
0.27 \[0.24, 0.30\]
</td>
<td style="text-align:left;">
-1.31
</td>
<td style="text-align:left;">
0.06
</td>
<td style="text-align:left;">
-22.33
</td>
<td style="text-align:left;">
\< .001
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
SexFemale
</td>
<td style="text-align:left;">
10.15 \[8.05, 12.86\]
</td>
<td style="text-align:left;">
2.32
</td>
<td style="text-align:left;">
0.12
</td>
<td style="text-align:left;">
19.38
</td>
<td style="text-align:left;">
\< .001
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Model 2
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
$\chi^2$(3) = 106.08, $p$ = \< .001
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
(Intercept)
</td>
<td style="text-align:left;">
0.70 \[0.54, 0.92\]
</td>
<td style="text-align:left;">
-0.35
</td>
<td style="text-align:left;">
0.14
</td>
<td style="text-align:left;">
-2.60
</td>
<td style="text-align:left;">
.009
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
SexFemale
</td>
<td style="text-align:left;">
11.26 \[8.60, 14.85\]
</td>
<td style="text-align:left;">
2.42
</td>
<td style="text-align:left;">
0.14
</td>
<td style="text-align:left;">
17.41
</td>
<td style="text-align:left;">
\< .001
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Class2nd
</td>
<td style="text-align:left;">
0.39 \[0.26, 0.56\]
</td>
<td style="text-align:left;">
-0.95
</td>
<td style="text-align:left;">
0.19
</td>
<td style="text-align:left;">
-4.90
</td>
<td style="text-align:left;">
\< .001
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Class3rd
</td>
<td style="text-align:left;">
0.19 \[0.14, 0.26\]
</td>
<td style="text-align:left;">
-1.66
</td>
<td style="text-align:left;">
0.17
</td>
<td style="text-align:left;">
-9.88
</td>
<td style="text-align:left;">
\< .001
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
ClassCrew
</td>
<td style="text-align:left;">
0.41 \[0.30, 0.56\]
</td>
<td style="text-align:left;">
-0.88
</td>
<td style="text-align:left;">
0.16
</td>
<td style="text-align:left;">
-5.61
</td>
<td style="text-align:left;">
\< .001
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Model 3
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
$\chi^2$(3) = 65.18, $p$ = \< .001
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
(Intercept)
</td>
<td style="text-align:left;">
0.53 \[0.38, 0.71\]
</td>
<td style="text-align:left;">
-0.64
</td>
<td style="text-align:left;">
0.16
</td>
<td style="text-align:left;">
-4.10
</td>
<td style="text-align:left;">
\< .001
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
SexFemale
</td>
<td style="text-align:left;">
67.09 \[26.71, 225.74\]
</td>
<td style="text-align:left;">
4.21
</td>
<td style="text-align:left;">
0.53
</td>
<td style="text-align:left;">
7.92
</td>
<td style="text-align:left;">
\< .001
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Class2nd
</td>
<td style="text-align:left;">
0.31 \[0.18, 0.52\]
</td>
<td style="text-align:left;">
-1.17
</td>
<td style="text-align:left;">
0.27
</td>
<td style="text-align:left;">
-4.40
</td>
<td style="text-align:left;">
\< .001
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Class3rd
</td>
<td style="text-align:left;">
0.40 \[0.27, 0.58\]
</td>
<td style="text-align:left;">
-0.92
</td>
<td style="text-align:left;">
0.20
</td>
<td style="text-align:left;">
-4.72
</td>
<td style="text-align:left;">
\< .001
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
ClassCrew
</td>
<td style="text-align:left;">
0.55 \[0.39, 0.77\]
</td>
<td style="text-align:left;">
-0.61
</td>
<td style="text-align:left;">
0.18
</td>
<td style="text-align:left;">
-3.43
</td>
<td style="text-align:left;">
.001
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
SexFemale:Class2nd
</td>
<td style="text-align:left;">
0.66 \[0.17, 2.18\]
</td>
<td style="text-align:left;">
-0.42
</td>
<td style="text-align:left;">
0.64
</td>
<td style="text-align:left;">
-0.65
</td>
<td style="text-align:left;">
.515
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
SexFemale:Class3rd
</td>
<td style="text-align:left;">
0.06 \[0.02, 0.16\]
</td>
<td style="text-align:left;">
-2.80
</td>
<td style="text-align:left;">
0.56
</td>
<td style="text-align:left;">
-4.98
</td>
<td style="text-align:left;">
\< .001
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
SexFemale:ClassCrew
</td>
<td style="text-align:left;">
0.35 \[0.07, 1.93\]
</td>
<td style="text-align:left;">
-1.06
</td>
<td style="text-align:left;">
0.82
</td>
<td style="text-align:left;">
-1.29
</td>
<td style="text-align:left;">
.196
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Model 4
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
$\chi^2$(1) = 20.34, $p$ = \< .001
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
(Intercept)
</td>
<td style="text-align:left;">
1.46 \[0.85, 2.49\]
</td>
<td style="text-align:left;">
0.38
</td>
<td style="text-align:left;">
0.27
</td>
<td style="text-align:left;">
1.38
</td>
<td style="text-align:left;">
.166
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
SexFemale
</td>
<td style="text-align:left;">
68.93 \[27.43, 232.02\]
</td>
<td style="text-align:left;">
4.23
</td>
<td style="text-align:left;">
0.53
</td>
<td style="text-align:left;">
7.97
</td>
<td style="text-align:left;">
\< .001
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Class2nd
</td>
<td style="text-align:left;">
0.29 \[0.17, 0.49\]
</td>
<td style="text-align:left;">
-1.23
</td>
<td style="text-align:left;">
0.27
</td>
<td style="text-align:left;">
-4.58
</td>
<td style="text-align:left;">
\< .001
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Class3rd
</td>
<td style="text-align:left;">
0.36 \[0.24, 0.53\]
</td>
<td style="text-align:left;">
-1.02
</td>
<td style="text-align:left;">
0.20
</td>
<td style="text-align:left;">
-5.14
</td>
<td style="text-align:left;">
\< .001
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
ClassCrew
</td>
<td style="text-align:left;">
0.56 \[0.40, 0.80\]
</td>
<td style="text-align:left;">
-0.57
</td>
<td style="text-align:left;">
0.18
</td>
<td style="text-align:left;">
-3.23
</td>
<td style="text-align:left;">
.001
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
AgeAdult
</td>
<td style="text-align:left;">
0.35 \[0.22, 0.55\]
</td>
<td style="text-align:left;">
-1.05
</td>
<td style="text-align:left;">
0.23
</td>
<td style="text-align:left;">
-4.57
</td>
<td style="text-align:left;">
\< .001
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
SexFemale:Class2nd
</td>
<td style="text-align:left;">
0.64 \[0.16, 2.13\]
</td>
<td style="text-align:left;">
-0.45
</td>
<td style="text-align:left;">
0.65
</td>
<td style="text-align:left;">
-0.69
</td>
<td style="text-align:left;">
.488
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
SexFemale:Class3rd
</td>
<td style="text-align:left;">
0.06 \[0.02, 0.16\]
</td>
<td style="text-align:left;">
-2.86
</td>
<td style="text-align:left;">
0.56
</td>
<td style="text-align:left;">
-5.08
</td>
<td style="text-align:left;">
\< .001
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
SexFemale:ClassCrew
</td>
<td style="text-align:left;">
0.34 \[0.07, 1.87\]
</td>
<td style="text-align:left;">
-1.09
</td>
<td style="text-align:left;">
0.82
</td>
<td style="text-align:left;">
-1.33
</td>
<td style="text-align:left;">
.185
</td>
<td style="text-align:left;">
</td>
</tr>
</tbody>
</table>
