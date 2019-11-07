# generating code for Survival ----
magicfor::magic_for()
    for (i in 1:length(explanatoryKM)) {
    
      explanatoryKM_variable <- explanatoryKM[i]
      
      explanation <- paste0("Survival Analysis ", explanatoryKM_variable)
      
      y <- paste0("### ",explanation,"  ", "\n", "\n",

"**Kaplan-Meier Plot Log-Rank Test**","\n", "\n",
                  
"```{r Kaplan-Meier ", explanatoryKM_variable, "}
# Drawing Survival Curves Using ggplot2
# https://rpkgs.datanovia.com/survminer/reference/ggsurvplot.html
mydata %>%
  finalfit::surv_plot('", dependentKM , "', '", explanatoryKM_variable, "', 
  xlab='Time (months)', pval=TRUE, legend = 'none',
    break.time.by = 12, xlim = c(0,60)),
legend.labs = c('a','b'))

)
```", "\n", "\n",

"**Univariate Cox-Regression**","\n", "\n",

"```{r}
library(finalfit)
library(survival)
explanatoryUni <- '", explanatoryKM_variable, "'\n",
      "dependentUni <- '", dependentKM,"'\n",
      "mydata %>%
finalfit(dependentUni, explanatoryUni) -> tUni

knitr::kable(tUni, row.names=FALSE, align=c('l', 'l', 'r', 'r', 'r', 'r'))
```", "\n", "\n", "\\pagebreak",


"**Median Survival**","\n", "\n",
      
"```{r Median Survivals ", explanatoryKM_variable, "}
km_fit <- survfit(", dependentKM, " ~ ", explanatoryKM_variable, ", data = mydata)
km_fit
```", "\n", "\n",

"**1-3-5-yr survival**","\n", "\n",
      
"```{r 1-3-5-yr ", explanatoryKM_variable, "}
summary(km_fit, times = c(12,36,60))
```", "\n", "\n", "\\pagebreak","\n"
)
      put(y)
        }

writeLines(magicfor::magic_result_as_vector(), here::here("childRmd", paste0("generatedCodeSurvival.Rmd")))
magicfor::magic_free()

