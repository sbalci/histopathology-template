# generating code for descriptive statistics ----
magicfor::magic_for()
for (i in 1:length(names(mydataCategorical))) {

    explanation <- paste0("Descriptive Statistics ", names(mydataCategorical)[i])

    y <- paste0("### ",explanation,"  ", "\n", "\n",

"```{r ", names(mydataCategorical)[i], "}
mydataCategorical %>% 
  janitor::tabyl(", names(mydataCategorical)[i], ") %>%
  adorn_pct_formatting(rounding = 'half up', digits = 1) %>%
  knitr::kable()
```", "\n", "\n", "\\pagebreak","\n")
    put(y)
}

writeLines(magicfor::magic_result_as_vector(), here::here("childRmd", "generatedCodeDescriptives.Rmd"))
magicfor::magic_free()



# whippleables %>% 
#     janitor::tabyl(Organ) %>%
#     janitor::adorn_pct_formatting(rounding = 'half up', digits = 1) %>%
#     janitor::adorn_totals("row") %>%
#     knitr::kable()

