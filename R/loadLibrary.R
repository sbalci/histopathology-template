package_list <- c(
    
    # to run rmarkdown
    
    "evaluate",
    "digest",
    "highr",
    "markdown",
    "yaml",
    "Rcpp",
    "htmltools",
    "knitr",
    "jsonlite",
    "base64enc",
    "mime",
    "rmarkdown",
    
    # tidyverse
    
    "ggplot2",
    "dplyr",
    "tidyr",
    "readr",
    "purrr",
    "tibble",
    "stringr",
    "forcats",
    "hms",
    "lubridate",
    "feather",
    "haven",
    "httr",
    "jsonlite",
    "readxl",
    "rvest",
    "xml2",
    "modelr",
    "broom",
    
    "glue",
    "rlang",
    "renv",
    "formatR",
    
    # clean data
    
    "janitor",
    
    # write data
    
    "rio",
    
    # Statistics
    
    "explore",
    "finalfit",
    "dataMaid",
    "describer",
    "inspectdf",
    "visdat",
    "xray",
    "DataExplorer",
    "arsenal",
    "tableone",
    "magicfor",
    "SmartEDA",
    
    # could not install on docker
    
    # "summarytools",
    # "ggstatsplot",
    # "kableExtra",
    # "writexl",
    
    # may require
    
    # "zeallot",
    # "remotes",
    
    # fake data
    
    "wakefield"
)


# pak::pkg_install(package_list)

# pacman::p_load(char = package_list)


# github_packages <- c(
#     "easystats/report"
# )

if (!require("report")) devtools::install_github("easystats/report")

# pacman::p_load_gh(char = github_packages)

# pak::pkg_install(github_packages)


suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("magrittr"))
