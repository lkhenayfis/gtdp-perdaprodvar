######################################### CLASSE GAMPERDA ##########################################

new_gamperda <- function(dat, mod, coefI, corteI, coefS, corteS, atributos) {

    dat <- copy(dat)[, .SD, .SDcols = c("vazao", "perda")]

    modinf <- lm(perda ~ I(vazao^2), dat)
    modinf$coefficients <- c(0, coefI)

    modsup <- lm(perda ~ I(vazao^2), dat)
    modsup$coefficients <- coefS

    out <- list(model = list(modinf, mod, modsup), dat = dat)
    attr(out, "cortes") <- c(inf = corteI, sup = corteS)
    attr(out, "coef")   <- list(inf = c(0, coefI), sup = coefS)
    for(at in names(atributos)) attr(out, at) <- atributos[[at]]
    class(out) <- "gamperda"

    return(out)
}

# METODOS ------------------------------------------------------------------------------------------

#' @export

predict.gamperda <- function(object, newdata, ...) {

    if("data.table" %in% class(newdata)) {
        data <- copy(newdata)
    } else {
        data <- newdata
        setDT(data)
    }

    sect <- findInterval(data$vazao, attr(object, "cortes")) + 1
    pred <- split(data, sect)
    pred <- lapply(names(pred), function(i) predict(object$model[[as.numeric(i)]], newdata = pred[[i]]))

    ordem <- split(seq(nrow(data)), sect)
    pred  <- unlist(pred)
    pred  <- pred[order(unlist(ordem))]

    return(pred)
}

#' @export

fitted.gamperda <- function(object, ...) predict(object, object$dat)

#' @export

residuals.gamperda <- function(object, ...) {

    fit <- fitted(object)
    res <- object$model$perda - fit

    return(res)
}