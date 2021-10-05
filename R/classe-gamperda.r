######################################### CLASSE GAMPERDA ##########################################

#' Objeto \code{gamperda}
#' 
#' Construtor e métodos da classe \code{gamperda}
#' 
#' @name gamperda
NULL

#' Construtor Interno
#' 
#' Função para contrução da saída de \code{fitgam_perda}, não deve ser chamada diretamente
#' 
#' @rdname gamperda

new_gamperda <- function(dat, mod, coefI, corteI, coefS, corteS, atributos, args) {

    dat <- copy(dat)[, .SD, .SDcols = c("vazao", "perda")]

    modinf <- lm(perda ~ I(vazao^2), dat)
    modinf$coefficients <- c(0, coefI)

    modsup <- lm(perda ~ I(vazao^2), dat)
    modsup$coefficients <- coefS

    out <- list(model = list(modinf, mod, modsup), dat = dat)
    attr(out, "cortes")  <- c(inf = corteI, sup = corteS)
    attr(out, "coef")    <- list(inf = c(0, coefI), sup = coefS)
    attr(out, "gamargs") <- args
    for(at in names(atributos)) attr(out, at) <- atributos[[at]]
    class(out) <- "gamperda"

    return(out)
}

# METODOS ------------------------------------------------------------------------------------------

#' @param object objeto \code{gamperda} retornado por \code{\link{fitgam_perda}}
#' @param newdata data.frame ou data.table contendo variável explicativa para previsão
#' @param ... demais parametros
#' 
#' @return para \code{predict.gamperda}, vetor de perdas previstas nas abscissas contidas em 
#' \code{newdata}. Para code{fitted.gamperda}, os valores ajustados e \code{rediduals.gamperda} os
#' erros
#' 
#' @export
#' 
#' @rdname gamperda

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
#' 
#' @rdname gamperda

fitted.gamperda <- function(object, ...) predict(object, object$dat)

#' @export
#' 
#' @rdname gamperda

residuals.gamperda <- function(object, ...) {

    fit <- fitted(object)
    res <- object$model$perda - fit

    return(res)
}

#' @export

AIC.gamperda <- function(object, ...) AIC(object$model[[2]])

#' @export

BIC.gamperda <- function(object, ...) BIC(object$model[[2]])