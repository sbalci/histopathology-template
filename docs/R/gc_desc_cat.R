# generating code for descriptive statistics ----
magicfor::magic_for()
for (i in 1:length(characterVariables)) {

    explanation <- paste0("Descriptive Statistics ", characterVariables[i])

    y <- paste0("#### ",explanation,"  ", "\n", "\n",

"```{r ", explanation, "}
mydata %>% 
  janitor::tabyl(", characterVariables[i], ") %>%
  janitor::adorn_pct_formatting(rounding = 'half up', digits = 1) %>%
  knitr::kable()
```", "\n", "\n", "\\pagebreak","\n")
    put(y)
}

writeLines(magicfor::magic_result_as_vector(), here::here("childRmd", "gc_desc_cat.Rmd"))
magicfor::magic_free()
