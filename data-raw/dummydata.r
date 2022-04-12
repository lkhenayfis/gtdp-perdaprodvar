devtools::load_all()

arq <- "D:/ONS/OneDrive - Operador Nacional do Sistema Eletrico/Documentos/TESTE/dummyusi.RDS"
dummydata <- readRDS(arq)
oldcod <- attr(dummydata, "cod")

minolds <- sapply(dummydata[, 2:6], min, na.rm = TRUE)
maxolds <- sapply(dummydata[, 2:6], max, na.rm = TRUE)

minnews <- c(50, 20, .1, 40, .006)
maxnews <- c(60, 22, .5, 200, .007)

renorm <- function(vec, minold, maxold, minnew, maxnew) {
    (vec - minold) / (maxold - minold) * (maxnew - minnew) + minnew
}

dummydata[, 2:6] <- mapply(renorm, dummydata[, 2:6], minolds, maxolds, minnews, maxnews, SIMPLIFY = FALSE)
attr(dummydata, "cod") <- 999
attr(dummydata, "nome") <- "dummy"
attr(dummydata, "nmaq") <- 0
attr(dummydata, "qef") <- 210

usethis::use_data(dummydata, overwrite = TRUE)

aux <- bordasCC[usina == oldcod]
aux[, usina := 999]
aux[, queda := renorm(queda, minolds[2], maxolds[2], minnews[2], maxnews[2])]
aux[, vazao := renorm(queda, minolds[4], maxolds[4], minnews[4], maxnews[4])]

bordasCC <- rbind(bordasCC, aux)
