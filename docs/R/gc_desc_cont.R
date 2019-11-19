# generating code for descriptive statistics ----
magicfor::magic_for()
for (i in 1:length(continiousVariables)) {

    explanation <- paste0("Descriptive Statistics ", continiousVariables[i])
    
    y <- paste0("**",explanation,"** ", "\n", "\n",

"```{r ", explanation, "}
mydata %>% 
jmv::descriptives(
    data = .,
    vars = '", continiousVariables[i], "',
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
writeLines(magicfor::magic_result_as_vector(), here::here("childRmd" , "gc_desc_cont.Rmd"))
magicfor::magic_free()

