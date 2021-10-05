devtools::load_all()

arq <- "C:/Users/lucask/Documents/TESTE/dummyusi.RDS"
dummydata <- readRDS(arq)

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
