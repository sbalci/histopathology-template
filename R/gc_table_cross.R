# generating code for Cross Tables ----
magicfor::magic_for()
for (i in 1:length(dependent)) {
    dependent_variable <- dependent[i]
    # explanatory_variable <- explanatory[!explanatory %in% dependent_variable]

    explanation <- paste0("Cross Table ", dependent_variable)
    
    y <- paste0("**",explanation,"** ", "\n", "\n",
                
"```{r crosstable finalfit ", dependent_variable, ", message=FALSE, warning=FALSE}
mydata %>%
    summary_factorlist(dependent = '", dependent_variable, "', 
                       explanatory = explanatory,
                       # column = TRUE,
                       total_col = TRUE,
                       p = TRUE,
                       add_dependent_label = TRUE,
                       na_include=FALSE
                       # catTest = catTestfisher
                       ) -> table

knitr::kable(table, row.names = FALSE, align = c('l', 'l', 'r', 'r', 'r'))
```", "\n", "\n", "\\pagebreak","\n",



"```{r crosstable arsenal ", dependent_variable, ", message=FALSE, warning=FALSE, eval=FALSE, include=FALSE}

table1 <- arsenal::tableby(", dependent_variable, " ~ explanatory, mydata)

summary(table1)

knitr::kable(table1,
                         row.names = FALSE,
                         align = c('l', 'l', 'r', 'r', 'r', 'r'),
                         format = 'html') %>%
                kableExtra::kable_styling(kable_input = .,
                                          bootstrap_options = 'striped',
                                          full_width = F,
                                          position = 'left')
```", "\n", "\n", "\\pagebreak","\n",


"```{r crosstable tangram ", dependent_variable, ", message=FALSE, warning=FALSE, eval=FALSE, include=FALSE}
tangram::tangram(", dependent_variable, " ~ explanatory, mydata)

```", "\n", "\n", "\\pagebreak","\n",


"```{r crosstable tangram NEJM ", dependent_variable, ", message=FALSE, warning=FALSE, eval=FALSE, include=FALSE}
tangram::html5(tangram::tangram(", dependent_variable, " ~ explanatory, mydata),
                    fragment = TRUE,
                    inline = 'nejm.css',
                    caption = 'Cross Table", dependent_variable, "NEJM Style',
                    id = 'tbl3')
```", "\n", "\n", "\\pagebreak","\n",


"```{r crosstable tangram Lancet ", dependent_variable, ", message=FALSE, warning=FALSE, eval=FALSE, include=FALSE}
tangram::html5(tangram::tangram(", dependent_variable, " ~ explanatory, mydata),
                    fragment = TRUE,
                    inline = 'lancet.css',
                    caption = 'Cross Table", dependent_variable, "Lancet Style',
                    id = 'tbl3')
```", "\n", "\n", "\\pagebreak","\n"


)
    put(y)
}
writeLines(magicfor::magic_result_as_vector(), here::here("childRmd", "generatedCodeCrossTables.Rmd"))
magicfor::magic_free()
