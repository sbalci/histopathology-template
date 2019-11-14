# Find ID and Key Columns to Exclude From Analysis

find_key <-  function(mydataframe = mydata) {
    mydata %>% 
        sapply(., FUN = dataMaid::isKey) %>% 
        as_tibble() %>% 
        select(
            which(.[1, ] == TRUE)
        ) %>% 
        names()
}
    
