########################################## CLASSE GAMPROD ##########################################

#' Objeto \code{gamprod}
#' 
#' Construtor e métodos da classe \code{gamprod}
#' 
#' @name gamprod
NULL

#' Construtor Interno
#' 
#' Função para contrução da saída de \code{fitgam_perda}, não deve ser chamada diretamente
#' 
#' @rdname gamprod

new_gamprod <- function(dat, mod, atributos, args) {

    dat <- copy(dat)[, .SD, .SDcols = c("quedal", "vazao", "prod")]

    out <- list(model = mod, dat = dat)
    attr(out, "gamargs") <- args
    for(at in names(atributos)) attr(out, at) <- atributos[[at]]
    class(out) <- "gamprod"

    return(out)
}

#' @export

print.gamprod <- function(x, ...) {
    cat("\n## GAM\n")
    print(x$model)
}

# METODOS ------------------------------------------------------------------------------------------

#' @param object objeto \code{gamprod} retornado por \code{\link{fitgam_prod}}
#' @param newdata data.frame ou data.table contendo variável explicativa para previsão
#' @param ... demais parametros
#' 
#' @return para \code{predict.gamprod}, vetor de produtibilidades previstas nas abscissas contidas 
#'     em \code{newdata}. Para code{fitted.gamprod}, os valores ajustados e \code{rediduals.gamprod} 
#'     os erros
#' 
#' @export
#' 
#' @rdname gamprod

predict.gamprod <- function(object, newdata, ...) {

    pred <- predict(object$model, newdata = newdata)

    return(pred)
}

#' @export
#' 
#' @rdname gamprod

fitted.gamprod <- function(object, ...) predict(object, object$dat)

#' @export
#' 
#' @rdname gamprod

residuals.gamprod <- function(object, ...) {

    fit <- fitted(object)
    res <- object$dat$prod - fit

    return(res)
}

#' @export

AIC.gamprod <- function(object, ...) AIC(object$model)

#' @export

BIC.gamprod <- function(object, ...) BIC(object$model)