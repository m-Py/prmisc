# prmisc

Miscellaneous printing of statistical results in Rmarkdown according to
APA style guidelines. This package covers some basic statistical tests
(t-test, ANOVA, correlation, chi-squared test) and some basic number
printing manipulations (formatting p-values, removing leading zeros for
numbers that cannot be greater than one, and others). For more printing
functions in R markdown documents see the R package
[papaja](https://github.com/crsh/papaja).

# Installation

```R
library("devtools") # if not available: install.packages("devtools")
install_github("m-Py/prmisc")

# load the package via
library("prmisc")
```

# Usage

## t.test

```R
ttest <- t.test(1:10, y = c(7:20), var.equal = TRUE)
library("effsize") # for Cohen's d
cohend <- cohen.d(1:10, c(7:20))
print_ttest(ttest, cohend) # include this call in Rmd inline code

# [1] "$t(22) = -5.15$, $p < .001$, $d = -2.13$"

# check out help:
?print_ttest
```

## chi-square-test

```R
x <- matrix(c(12, 5, 7, 7), ncol = 2)
print_chi2(x) # does not use continuity correction by default
# [1] "$\\chi^2(1, N = 31) = 1.37$, $p = .242$, $\\phi = .21$"

print_chi2(x, correct = TRUE) # use continuity correction
# [1] "$\\chi^2(1, N = 31) = 0.64$, $p = .423$, $\\phi = .14$"
```

## Correlation coefficient

```R
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
cor_results <- cor.test(x, y)

print_cortest(cor_results)
# [1] "$r = .57$, $p = .108$"

# also show t statistic:
print_cortest(cor_results, print_t = TRUE)
[1] "$r = .57$, $t = 1.84$, $p = .108$"
```

## ANOVA

```R
library("afex")
# see ?aov_ez
data(md_12.1)
aov_results <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"))

print_anova(aov_results) # returns a list with all effects in this ANOVA
# $angle
# [1] "$F(1.92$, $17.31) = 40.72$, $p < .001$, $\\eta_G^2 = .39$"
# 
# $noise
# [1] "$F(1$, $9) = 33.77$, $p < .001$, $\\eta_G^2 = .39$"
# 
# $`angle:noise`
# [1] "$F(1.81$, $16.27) = 45.31$, $p < .001$, $\\eta_G^2 = .19$"

## Print nonitalic eta, which is required according to APA guidelines
print_anova(aov_results, italic_eta = FALSE)
# $angle
# [1] "$F(1.92$, $17.31) = 40.72$, $p < .001$, $\\upeta_\\mathrm{G}^2 = .39$"
#
# $noise
# [1] "$F(1$, $9) = 33.77$, $p < .001$, $\\upeta_\\mathrm{G}^2 = .39$"
# 
# $`angle:noise`
# [1] "$F(1.81$, $16.27) = 45.31$, $p < .001$, $\\upeta_\\mathrm{G}^2 = .19$"


## Example using other (or no) effect size index (output not shown)
print_anova(aov_ez("id", "rt", md_12.1, within = c("angle", "noise"),
                   anova_table = list(es = "pes")))
print_anova(aov_ez("id", "rt", md_12.1, within = c("angle", "noise"),
                   anova_table = list(es = "none")))

```

## Some functions for printing numbers

```R
force_decimals(c(1.23456, 0.873, 2.3456), decimals = 2)
# [1] "1.23" "0.87" "2.35"
## Note that function `round` will not produce the same results as
## force_decimals in Rmd output

## Leave integers intact:
force_or_cut(c(1:3, 1.23456, 0.873, 2.3456), decimals = 2)
# [1] "1"    "2"    "3"    "1.23" "0.87" "2.35"
## Compare:
force_decimals(c(1:3, 1.23456, 0.873, 2.3456), decimals = 2)
# [1] "1.00" "2.00" "3.00" "1.23" "0.87" "2.35"

## Show only decimals (e.g., for p-values or correlation coefficients)
decimals_only(c(0.23456, 0.873, 0.3456), decimals = 3)
# [1] ".235" ".873" ".346"

## Format a p-value, default is 3 decimals
format_p(0.03123)
# [1] "$p = .031$"

format_p(0.000001231, 3)
# [1] "$p < .001$"

format_p(0.3123, decimals = 2)
# [1] "$p = .31$"

## Format several p-values with one function call
format_p(c(0.3123, 0.001, 0.00001, 0.19))
# [1] "$p = .312$" "$p = .001$" "$p < .001$" "$p = .190$"

format_p(c(.999, .9999, 1))
# [1] "$p = .999$" "$p > .999$" "$p > .999$"

## Number printing functions have two decimals by default, 
## but format_p has three decimals by default

```
