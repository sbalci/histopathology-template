# generating code for descriptive statistics ----
magicfor::magic_for()
for (i in 1:length(names(mydataContinious))) {
y <- paste0("```{r ", names(mydataContinious)[i], "}
mydataContinious %>% 
jmv::descriptives(
    data = .,
    vars = ", names(mydataContinious)[i], ",
    hist = TRUE,
    dens = TRUE,
    box = TRUE,
    violin = TRUE,
    dot = TRUE,
    mode = TRUE,
    sd = TRUE,
    variance = TRUE,
    skew = TRUE,
    kurt = TRUE,
    quart = TRUE)
```", "\n", "\n", "\\pagebreak","\n")
    put(y)
}
writeLines(magicfor::magic_result_as_vector(), here::here("childRmd" , "generatedCodeContinious.Rmd"))
magicfor::magic_free()
