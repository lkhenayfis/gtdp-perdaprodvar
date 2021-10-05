##################################### OTIMIZACAO DO AJUSTE GAM #####################################

optgam_perda <- function(dat, range.vazao = 5:30, ts.vazao = "tp", extrap = c(1, 1), quantil = c(.05, .95)) {

    temp <- Sys.getenv(".perdaprodvarTEMP")
    temp <- file.path(temp, paste0("optgamperda", ts.vazao, ".RDS"))

    BICs <- data.table(ns = range.vazao, BIC = NA_real_)
    attr(BICs, "cod")      <- attr(dat, "cod")
    attr(BICs, "ts.vazao") <- attr(dat, "ts.vazao")

    if(file.exists(temp)) {
        old <- readRDS(temp)
    }

    if(matchattr(old, BICs)) {
        BICs <- rbind(old, BICs)
        BICs <- BICs[!duplicated(BICs$ns)]
    }

    nsteste <- BICs[is.na(BIC), ns]
    fitgams <- lapply(nsteste, function(ns) fitgam_perda(dat, ns, ts.vazao, extrap, quantil))

    BICs[match(nsteste, ns), BIC := sapply(fitgams, BIC)]
    saveRDS(BICs, temp)

    fitgams <- fitgams[order(BICs$BIC)]

    return(fitgams)
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