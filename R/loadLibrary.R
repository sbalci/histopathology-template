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

"jmv",
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


pak::pkg_install(package_list)

pacman::p_load(char = package_list)


github_packages <- c(
    "easystats/report"
)

pacman::p_load_gh(char = github_packages)




# rmarkdown,
# here,
# knitr,
# wakefield,
# readxl,
# report,
# explore,
# dataMaid,
# inspectdf,
# describer,
# visdat,
# dplyr,
# janitor,
# xray,
# DataExplorer,
# arsenal,
# tableone,
# SmartEDA,
# lubridate,
# forcats,
# survival,
# finalfit,
# tibble,
# ,
# shiny,
# magrittr,
# survminer,
# viridis,
# ggplot2,
# readr,
# rio,
# jmv,
# R,
# tidyr,
# purrr,
# stringr,
# hms,
# feather,
# haven,
# httr,
# jsonlite,
# rvest,
# xml2,
# modelr,
# broom,
# pander,
# magicfor,
# formatR,
# renv,
# zeallot,
# pacman


# 
# evaluate
# digest
# highr
# markdown
# yaml
# Rcpp
# htmltools
# knitr
# jsonlite
# base64enc
# mime

# 
# insight
# bayestestR
# performance
# parameters




# ucminf
# numDeriv
# ordinal
# checkmate
# pan
# jomo
# ggrepel
# ggsci
# cowplot
# ggsignif
# polynom
# exactRankTests
# mvtnorm
# KMsurv
# zoo
# km.ci
# reshape
# Formula
# latticeExtra
# acepack
# htmlTable
# viridis
# minqa
# nloptr
# RcppEigen
# mitml
# ggpubr
# maxstat
# survMisc
# GGally
# gridExtra
# Hmisc
# lme4
# mice
# pROC
# survminer




# zip
# openxlsx
# 

# 
# 
# 
# odbc
# rpart.plot
# 
# 

# 
# DEoptimR
# pander
# robustbase
# 
# 

# 
# shades
# ggfittext
# 
# 




# # magick
# # matrixStats
# # pryr
# # rapportools
# # RCurl
# 
# 

# 
# iterators
# foreach
# 
# 

# 
# igraph
# networkD3
# 
# 
# 

# 
# gtools
# mitools
# gdata
# survey
# e1071
# gmodels
# lmerTest
# labelled
# 
# 
# 

# 
# 
# 
# 
# 
# 
# # ---











# 
# zip
# listenv
# sp
# openxlsx
# qvcalc
# relimp
# gdata
# future
# globals
# DEoptimR
# pcaPP
# matrixStats
# RcppArmadillo
# rjson
# carData
# abind
# pbkrtest
# maptools
# rio
# TH.data
# sandwich
# gnm
# ca
# lmerTest
# pbivnorm
# gplots
# manipulateWidget
# elliptic
# contfrac
# deSolve
# SparseM
# expm
# libcoin
# modeltools
# TMB
# furrr
# fit.models
# robustbase
# rrcov
# pander
# inline
# loo
# mvnfast
# Brobdingnag
# jmvcore
# car
# multcomp
# PMCMR
# vcd
# vcdExtra
# GPArotation
# afex
# mvnormtest
# lavaan
# ggridges
# ROCR
# maps
# dichromat
# mapproj
# rgl
# estimability
# pbapply
# gtools
# MatrixModels
# hypergeo
# mcmc
# quantreg
# DescTools
# multcompView
# EMT
# coin
# lmtest
# nortest
# broom.mixed
# rsample
# colourpicker
# shinyjs
# robust
# skimr
# logspline
# coda
# LaplacesDemon
# rstan
# rstantools
# bridgesampling
# StanHeaders
# jmv
# gameofthrones
# ggthemes
# jcolors
# oompaBase
# harrypotter
# palr
# pals
# scico
# mnormt
# emmeans
# sjlabelled
# sjmisc
# BayesFactor
# ez
# MCMCpack
# rcompanion
# mc2d
# broomExtra
# ggcorrplot
# ggExtra
# groupedstats
# metaBMA
# metafor
# pairwiseComparisons
# paletteer
# psych
# sjstats
# statsExpressions
# WRS2








# if (!require("pak"))
#     install.packages("pak")
# library(pak)
# if (!require("rlang"))
#     install.packages("rlang")
# library(rlang)
# if (!require("renv"))
#     install.packages("renv")
# library(renv)
# if (!require("xfun"))
#     install.packages("xfun")
# library(xfun)
# 
# pacman::p_load(
#     char = c(
#         "knitr",
#         "devtools",
#         "rmarkdown",
#         "prettydoc",
#         "distill",
#         "pacman",
#         "here",
#         "exploreR",
#         "wakefield",
#         "readxl",
#         "rio",
#         "explore",
#         "dataMaid",
#         "inspectdf",
#         "describer",
#         "visdat",
#         "dplyr",
#         "janitor",
#         "summarytools",
#         "xray",
#         "DataExplorer",
#         "dlookr",
#         "ISLR",
#         "SmartEDA",
#         "questionr",
#         "finalfit",
#         "forcats",
#         "stringr",
#         "arsenal",
#         "naniar",
#         "mice",
#         "survival",
#         "tableone",
#         "summarizer",
#         "DT",
#         "ggstatsplot",
#         "ggalluvial",
#         "rgl",
#         "ggparallel",
#         "ggplot2",
#         "plotly",
#         "gapminder",
#         "stats",
#         "OptimalCutpoints",
#         "yardstick",
#         "pROC",
#         "plotROC",
#         "lubridate",
#         "tibble",
#         "glue",
#         "scales",
#         "insight",
#         "purrr",
#         "survminer",
#         "shiny",
#         "magrittr",
#         "viridis",
#         "eval",
#         "correlation",
#         "psycho",
#         "kableExtra",
#         "lme4",
#         "rstanarm",
#         "readr",
#         "writexl",
#         "citation",
#         "citations",
#         "jmv",
#         "magicfor",
#         "pak",
#         "rlang",
#         "renv",
#         "xfun"
#     )
# )
# 
# 
# pacman::p_load_gh("easystats/report",
#                   "noamross/redoc")

# suppressPackageStartupMessages(library("dplyr"))
# suppressPackageStartupMessages(library("magrittr"))
# suppressPackageStartupMessages(library("readxl"))
# suppressPackageStartupMessages(library("janitor"))
# suppressPackageStartupMessages(library("report"))
# suppressPackageStartupMessages(library("finalfit"))
# suppressPackageStartupMessages(library("ggplot2"))
# # suppressPackageStartupMessages(library("ggstatsplot"))
# suppressPackageStartupMessages(library("wakefield"))
