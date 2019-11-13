# generating code for Continious Plots ----
magicfor::magic_for()
for (i in 1:length(dependent)) {
    dependent_variable <- dependent[i]
    explanation <- paste0("Continious Plot betweenstats boxviolinplot ", dependent_variable, " vs ", yContinious)
    y <- paste0("**",explanation,"** ", "\n", "\n",

"```{r betweenstats boxviolinplot ", dependent_variable, " vs ", yContinious, "}
mydata %>%
ggstatsplot::ggbetweenstats(x = ", dependent_variable ,", y = ", yContinious, ")

```", "\n", "\n", "\\pagebreak","\n")
    put(y)
}
writeLines(magicfor::magic_result_as_vector(), here::here("childRmd", "generatedCodeContiniousPlots.Rmd"))
magicfor::magic_free()
