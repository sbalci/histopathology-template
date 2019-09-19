ozet <- function(x) {
    ortalama <- mean(x)
    ortanca <- median(x)
    ssapma <- sd(x)
    min  <- min(x)
    max <- max(x)
    cat(names(x), "ortalama+ssapma:",ortalama,"+",ssapma)
}

ozet(merkez_sonuc$X1.kasetteki.parca.sayisi)

