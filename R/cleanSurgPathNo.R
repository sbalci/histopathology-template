cleanSurgPathNo <- 
    . %>% 
    trimws() %>% 
    gsub(pattern = "[a-zA-Z]",
         replacement = "",
         x = .
         ) %>% 
    gsub(pattern = "-",
         replacement = " ",
         x = .
         ) %>% 
    substr(x = .,
           start = 1,
           stop = 8
           )


