# generating code for Categorical Plots ----
magicfor::magic_for()
    for (i in 1:length(dependent2)) {
    dependent_variable <- dependent2[i]

    explanation <- paste0("Categorical Plots ", dependent_variable, " vs ", mydataCategorical_variable)

    y <- paste0("**",explanation,"** ", "\n", "\n",

"```{r ggstatplot ", dependent_variable , " vs ", mydataCategorical_variable, ", layout='l-page'}
mydata %>% 
    ggstatsplot::ggbarstats(data = ., main = ", mydataCategorical_variable, ", condition = ", dependent_variable, ")
```", "\n", "\n", "\\pagebreak","\n")
    put(y)
        }
writeLines(magicfor::magic_result_as_vector(), here::here("childRmd", "generatedCodeCategoricalPlots.Rmd"))
magicfor::magic_free()
