# generating code for Survival ----
magicfor::magic_for()
    for (i in 1:length(explanatoryKM)) {
    
      explanatoryKM_variable <- explanatoryKM[i]
      
      explanation <- paste0("Survival Analysis ", explanatoryKM_variable)
      
      y <- paste0("### ",explanation,"  ", "\n", "\n",

"**Kaplan-Meier Plot Log-Rank Test**","\n", "\n",
      
"```{r Kaplan-Meier ", explanatoryKM_variable, "}
library(survival)
library(survminer)
library(finalfit)

mydata %>%
  finalfit::surv_plot('", dependentKM , "', '", explanatoryKM_variable, "', 
  xlab='Time (months)', pval=TRUE, legend = 'none',
    break.time.by = 12, xlim = c(0,60)

# legend.labs = c('a','b')

)
```", "\n", "\n",

"**Univariate Cox-Regression**","\n", "\n",

"```{r}
explanatoryUni <- '", explanatoryKM_variable, "'\n",
"dependentUni <- '", dependentKM,"'\n",
"mydata %>%
finalfit(dependentUni, explanatoryUni, metrics=TRUE) -> tUni

knitr::kable(tUni[, 1:4], row.names=FALSE, align=c('l', 'l', 'r', 'r', 'r', 'r'))

```", "\n", "\n",
      
"**Univariate Cox-Regression Summary**","\n", "\n",
      
"```{r}
tUni_df <- tibble::as_tibble(tUni, .name_repair = 'minimal') %>%
janitor::clean_names(dat = ., case = 'snake')


n_level <- dim(tUni_df)[1]

tUni_df_descr <- function(n) {
    paste0(
        'When ',
        tUni_df$dependent_surv_overall_time_outcome[1],
        ' is ',
        tUni_df$x[n + 1],
        ', there is ',
        tUni_df$hr_univariable[n + 1],
        ' times risk than ',
        'when ',
        tUni_df$dependent_surv_overall_time_outcome[1],
        ' is ',
        tUni_df$x[1],
        '.'
    )

}



results5 <- purrr::map(.x = c(2:n_level-1), .f = tUni_df_descr)

print(unlist(results5))

```", "\n", "\n", "\\pagebreak", "\n", "\n",


"**Median Survival**","\n", "\n",
      
"```{r Median Survivals ", explanatoryKM_variable, "}
km_fit <- survfit(", dependentKM, " ~ ", explanatoryKM_variable, ", data = mydata)
km_fit

km_fit_median_df <- summary(km_fit)
km_fit_median_df <- as.data.frame(km_fit_median_df$table) %>%
    janitor::clean_names(dat = ., case = 'snake') %>%
    tibble::rownames_to_column(.data = ., var = '", explanatoryKM_variable, "')



km_fit_median_df %>%
    dplyr::mutate(
        description =
        glue::glue(
        'When, ", explanatoryKM_variable, ", {", explanatoryKM_variable, "}, median survival is {median} [{x0_95lcl} - {x0_95ucl}, 95% CI] months.'
)
    ) %>%
        dplyr::mutate(
description = gsub(pattern = 'thefactor=', replacement = ' is ', x = description)
        ) %>%
    dplyr::select(description) %>%
    dplyr::pull() -> km_fit_median_definition

km_fit_median_definition




```", "\n", "\n",

"**1-3-5-yr survival**","\n", "\n",
      
"```{r 1-3-5-yr ", explanatoryKM_variable, "}
summary(km_fit, times = c(12,36,60))

km_fit_summary <- summary(km_fit, times = c(12,36,60))

km_fit_df <- as.data.frame(km_fit_summary[c('strata', 'time', 'n.risk', 'n.event', 'surv', 'std.err', 'lower', 'upper')])

km_fit_df


     


km_fit_df %>%
    dplyr::mutate(
        description =
glue::glue(
    'When {strata}, {time} month survival is {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI].'
)
    ) %>%
    dplyr::select(description) %>%
    dplyr::pull() -> km_fit_definition

km_fit_definition


```", "\n", "\n", "\\pagebreak","\n", "\n",
      
      
"```{r}  
summary(km_fit)$table

km_fit_median_df <- summary(km_fit)
results1html <- as.data.frame(km_fit_median_df$table) %>%
    janitor::clean_names(dat = ., case = 'snake') %>%
    tibble::rownames_to_column(.data = ., var = '", explanatoryKM_variable, "')

results1html[,1] <- gsub(pattern = 'thefactor=',
 replacement = '',
 x = results1html[,1])

knitr::kable(results1html,
 row.names = FALSE,
 align = c('l', rep('r', 9)),
 format = 'html',
 digits = 1)
```", "\n", "\n", "\\pagebreak","\n", "\n",


"**Pairwise Comparisons**","\n", "\n",
      
"```{r eval=FALSE, include=FALSE, echo=TRUE}  

    survminer::pairwise_survdiff(
    formula = formula_p,
    data = self$data,
    p.adjust.method = 'BH'
)
```", "\n", "\n", "\\pagebreak","\n", "\n"
)
      put(y)
        }

writeLines(magicfor::magic_result_as_vector(), here::here("childRmd", paste0("generatedCodeSurvival.Rmd")))
magicfor::magic_free()

