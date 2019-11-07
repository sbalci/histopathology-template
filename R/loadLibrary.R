# load Library ----
# suppressPackageStartupMessages(library("tidyverse"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("readxl"))
suppressPackageStartupMessages(library("janitor"))
suppressPackageStartupMessages(library("report"))
suppressPackageStartupMessages(library("finalfit"))
suppressPackageStartupMessages(library("ggstatsplot"))
suppressPackageStartupMessages(library("wakefield"))

# citation("tidyverse")
# citation("readxl")
# citation("janitor")
# citation("report")


# citation("foreign")
# citation("tidylog")
# citation("jmv")
# citation("tangram")
# citation("summarytools")


# suppressPackageStartupMessages({
#     library("tidyverse")
#     library("readxl")
#     library("janitor")
#     library("report")
#     library("finalfit")
#     library("ggstatsplot")
#     library("jmv")
#     library("lubridate")
#     library("summarytools")
#     library("survival")
#     library("psycho")
#     library("kableExtra")
# })





# citation("tidyverse")
# citation("readxl")
# citation("janitor")
# citation("report")


# not used
# citation("foreign")
# citation("tidylog")
# citation("jmv")
# citation("tangram")
# citation("summarytools")

# Some Other Packages that can be used

# Packages for Descriptive Statistics

# devtools::install_github("XanderHorn/autoEDA")



# https://gist.github.com/smithdanielle/9913897
# 
# check.packages <- function(pkg){
#     new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#     if (length(new.pkg)) 
#         install.packages(new.pkg, dependencies = TRUE)
#     sapply(pkg, require, character.only = TRUE)
# }
# 
# # Usage example
# package.check <- lapply(packages, FUN = function(x) {
#     if (!require(x, character.only = TRUE)) {
#         install.packages(x, dependencies = TRUE)
#         library(x, character.only = TRUE)
#     }
# })
# 
# #verify they are loaded
# search()
#
# packages<-c("ggplot2", "afex", "ez", "Hmisc", "pander", "plyr")
# check.packages(packages)
# 
# options(repos=structure(c(CRAN="http://cloud.r-project.org/")))
# 
# if(!require(pacman))install.packages("pacman")
# pacman::p_load("dplyr", "DT", "mosaic", "MASS", "usdm", "tidyverse", "ggplot2", "PerformanceAnalytics", "caTools", "glmnet", "caret","leaps","doParallel")





