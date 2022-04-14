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
#' @param dat dado utilizado para ajuste
#' @param mod modelo ajustado
#' @param coefI,coefS coeficientes ajustados das extrapolações inferior e superior
#' @param corteI,corteS valores dos cortes entre extrapolações e modelo aditivo
#' @param fitcall chamada de \code{fitgam_perda} original
#' 
#' @rdname gamperda

new_gamperda <- function(dat, mod, coefI, coefS, corteI, corteS, fitcall) {

    dat <- copy(dat)[, .SD, .SDcols = c("vazao", "perda")]

    modinf <- lm(perda ~ I(vazao^2), dat)
    modinf$coefficients <- c(0, coefI)

    modsup <- lm(perda ~ I(vazao^2), dat)
    modsup$coefficients <- coefS

    out <- list(model = list(modinf, mod, modsup), dat = dat)
    attr(out, "cortes")  <- c(inf = corteI, sup = corteS)
    attr(out, "fitcall") <- fitcall
    class(out) <- "gamperda"

    return(out)
}

#' Print De Objetos \code{gamperda}
#' 
#' @param x objeto da classe \code{gamperda}
#' 
#' @export

print.gamperda <- function(x, ...) {
    cat("\n## Extrapolacao Inferior\n")
    print(x$model[[1]])
    cat("--------------------------\n")
    cat("\n## GAM\n")
    print(x$model[[2]])
    cat("--------------------------\n")
    cat("\n## Extrapolacao Superior\n")
    print(x$model[[3]])
}

# METODOS ------------------------------------------------------------------------------------------

#' @param object objeto \code{gamperda} retornado por \code{\link{fitgam_perda}}
#' @param newdata data.frame ou data.table contendo variável explicativa para previsão
#' @param ... demais parametros
#' 
#' @return para \code{predict.gamperda}, vetor de perdas previstas nas abscissas contidas em 
#'     \code{newdata}. Para code{fitted.gamperda}, os valores ajustados e \code{rediduals.gamperda} 
#'     os erros
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
    res <- object$dat$perda - fit

    return(res)
}

#' @export

AIC.gamperda <- function(object, ...) AIC(object$model[[2]])

#' @export

BIC.gamperda <- function(object, ...) BIC(object$model[[2]])