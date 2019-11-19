
mypackages <- function(package) {
    
if (!require({{package}})) install.packages({{package}})
library({{package}})
    
}


pacman::p_load(
    here,
    rmarkdown,
    knitr,
    renv,
    remotes,
    dplyr,
    rprojroot,
    readxl,
    janitor,
    bayestestR,
    prettydoc,
    distill,
    renv,
    remotes,
    rlang,
    xfun,
    stringi,
    magrittr
)

pacman::p_load_gh(
    "easystats/report",
    "noamross/redoc"
)

suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("magrittr"))
suppressPackageStartupMessages(library("readxl"))
suppressPackageStartupMessages(library("janitor"))
suppressPackageStartupMessages(library("report"))
suppressPackageStartupMessages(library("finalfit"))
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("ggstatsplot"))
suppressPackageStartupMessages(library("wakefield"))
