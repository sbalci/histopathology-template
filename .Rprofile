source("renv/activate.R")

if (requireNamespace("magrittr", quietly = TRUE)) {
    `%>%` <- magrittr::`%>%`
}

if (requireNamespace("magrittr", quietly = TRUE)) {
    `%$%` <- magrittr::`%$%`
}

if (requireNamespace("dplyr", quietly = TRUE)) {
    library(dplyr, quietly = TRUE)
}

.First <- function(){
cat("\nWelcome to histopathology template project", date(), "\n", "\n dplyr, %>%, %$% are loaded")
}

.Last <- function(){
cat("\nGoodbye at ", date(), "\n")
}

options(gtsummary.pvalue_fun = function(x) gtsummary::style_pvalue(x, digits = 2))
options(gtsummary.tbl_summary.percent_fun = function(x) sprintf("%.2f", 100 * x))
options(gtsummary.print_engine = "kable")
options(gtsummary.print_engine = "gt")
# options(gtsummary.tbl_summary.percent_fun = function(x) sigfig(x, digits = 3))

