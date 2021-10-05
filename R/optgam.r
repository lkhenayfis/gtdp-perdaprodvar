##################################### OTIMIZACAO DO AJUSTE GAM #####################################

optgam_perda <- function(dat, range.vazao = 5:30, ts.vazao = "tp", extrap = c(1, 1), quantil = c(.05, .95)) {

    fitgams <- lapply(range.vazao, function(ns) fitgam_perda(dat, ns, ts.vazao, extrap, quantil))
    BICs    <- sapply(fitgams, BIC)

    optgam <- fitgams[which.min(BICs)]

    return(optgam)
}

# HELPERS ------------------------------------------------------------------------------------------

matchattr <- function(obj1, obj2, quais = c("cod", "ts")) {

    quais <- paste0("(", paste0(quais, collapse = ")|("), ")")

    attr1 <- attributes(obj1)
    attr2 <- attributes(obj2)

    attr1 <- attr1[grep(quais, names(attr1))]
    attr2 <- attr2[grep(quais, names(attr2))]

    all(mapply(attr1, attr2, FUN = "=="))
}