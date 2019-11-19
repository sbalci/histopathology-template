# Descriptive Statistics for Categorical Variables

des_cat <- function(mydataframe = mydata, myvariable) {
    
    mydataframe %>% 
        janitor::tabyl({{myvariable}}) %>%
        janitor::adorn_pct_formatting(rounding = 'half up', digits = 1) %>%
        knitr::kable()
}
