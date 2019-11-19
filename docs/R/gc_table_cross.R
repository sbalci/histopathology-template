# generating code for Cross Tables ----
magicfor::magic_for()
for (i in 1:length(dependent)) {
    dependent_variable <- dependent[i]
    # explanatory_variable <- explanatory[!explanatory %in% dependent_variable]

    explanation <- paste0("Cross Table ", dependent_variable)
    
    y <- paste0("**",explanation,"** ", "\n", "\n",
                
"```{r crosstable", dependent_variable, ", message=FALSE, warning=FALSE}
mydata %>%
    summary_factorlist(dependent = '", dependent_variable, "', 
                       explanatory = explanatory,
                       column = TRUE,
                       total_col = TRUE,
                       p = TRUE,
                       add_dependent_label = TRUE,
                       na_include=FALSE
                       # catTest = catTestfisher
                       ) -> table

knitr::kable(table, row.names = FALSE, align = c('l', 'l', 'r', 'r', 'r'))
```", "\n", "\n", "\\pagebreak","\n")
    put(y)
}
writeLines(magicfor::magic_result_as_vector(), here::here("childRmd", "generatedCodeCrossTables.Rmd"))
magicfor::magic_free()
