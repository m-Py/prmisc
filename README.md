# prmisc

Miscellaneous printing of stat results in Rmarkdown according to APA
style guidelines. This package covers some basic statistical tests
(t-test, ANOVA, correlation, chi-squared test) and some basic number
printing manipulations (formatting p-values, removing leading zeros
for numbers that cannot be greater than one and others). For more
printing functions for R markdown documents see the package
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
```

## Correlation coefficient

```R
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
cor_results <- cor.test(x, y)

print_cortest(cor_results)

# [1] "$r = .57$, $p = .108$"
```

## ANOVA

```R
library("afex")
# see ?aov_ez
data(md_12.1)
aov_results <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"))

print_anova(aov_results, es = "ges")

## Print nonitalic eta, which is required according to APA guidelines
print_anova(aov_results, es = "ges", font = "nonitalic")

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

## Format a p-value
format_p(0.03123, 3)

# [1] "$p = .031$"

format_p(0.000001231, 3)

# [1] "$p < .001$"

format_p(0.3123, 2)

# [1] "$p = .31$"

format_p(.999, 3)

# [1] "$p = .999$"

format_p(.9999, 3)

# [1] "$p > .999$"

format_p(1, 3)

# [1] "$p > .999$"

## Format several p-values
format_p(c(0.3123, 0.001, 0.00001, 0.19), 3)
# [1] "$p = .312$" "$p = .001$" "$p < .001$" "$p = .190$"

```
