# generating code for Continious Plots ----
magicfor::magic_for()
for (i in 1:length(dependent)) {
    dependent_variable <- dependent[i]
    # explanatory_variable <- explanatory[!explanatory %in% dependent_variable]
    
    y <- paste0("```{r crosstable", dependent_variable, "}
mydata %>%
    summary_factorlist(dependent = '", dependent_variable, "', 
                       explanatory = explanatory,
                       total_col = TRUE,
                       p = TRUE,
                       add_dependent_label = TRUE) -> table

knitr::kable(table, row.names = FALSE, align = c('l', 'l', 'r', 'r', 'r'))
```", "\n", "\n", "\\pagebreak","\n")
    put(y)
}
writeLines(magicfor::magic_result_as_vector(), here::here("childRmd", "generatedCodeContiniousPlots.Rmd"))
magicfor::magic_free()