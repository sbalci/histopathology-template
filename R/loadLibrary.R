if (!require("pacman")) install.packages("pacman"); library(pacman)

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
    remotes
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
