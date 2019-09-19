sarcomatoid$surgpathno <- trimws(sarcomatoid$surgpathno)

sarcomatoid$surgpathno <- gsub(pattern = "[a-zA-Z]", replacement = "", x = sarcomatoid$surgpathno)

sarcomatoid$surgpathno <- gsub(pattern = "-", replacement = " ", x = sarcomatoid$surgpathno)

sarcomatoid$surgpathno <- substr(x = sarcomatoid$surgpathno, start = 1, stop = 8)

