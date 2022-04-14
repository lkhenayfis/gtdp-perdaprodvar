########################################## CLASSE GAMPROD ##########################################

#' Construtor Interno
#' 
#' Função para contrução da saída de \code{fitgam_prod}, não deve ser chamada diretamente
#' 
#' @param dat dado utilizado para ajuste
#' @param mod modelo ajustado
#' @param fitcall chamada de \code{fitgam_prod} original
#' 
#' @return objeto \code{gamprod} com modelo ajustado e dado original

new_gamprod <- function(dat, mod, fitcall) {

    usina <- ponto <- vazao <- NULL

    dat <- copy(dat)[, .SD, .SDcols = c("quedal", "vazao", "prod")]

    borda <- bordasCC[usina == attr(dat, "cod")]
    borda[ponto %in% 3:4, vazao := vazao * attr(dat, "nmaq")]

    rangevaz <- c(0, max(max(dat$vazao), attr(dat, "qmax")))
    rangehl  <- c(min(min(dat$quedal), min(borda$queda)), max(max(dat$quedal), max(borda$queda)))

    out <- list(model = mod, dat = dat)
    attr(out, "fitcall") <- fitcall
    attr(out, "ranges") <- list(quedal = rangehl, vazao = rangevaz)
    class(out) <- "gamprod"

    return(out)
}

#' Print De Objetos \code{gamprod}
#' 
#' @param x objeto da classe \code{gamprod}
#' @param ... existe apenas para consistência com a genérica
#' 
#' @return print do modelo no console, sem retornar nenhum valor
#' 
#' @export

print.gamprod <- function(x, ...) {
    cat("\n## GAM\n")
    print(x$model)
}

# METODOS ------------------------------------------------------------------------------------------

#' Previsão Com Objetos \code{gamprod}
#' 
#' Método \code{predict} para objetos da classe \code{gamprod}
#' 
#' @param object objeto \code{gamprod} retornado por \code{\link{fitgam_prod}}
#' @param newdata data.frame ou data.table contendo variáveis explicativas para previsão
#' @param ... existe apenas para consistência com a genérica
#' 
#' @return vetor de produtibilidades previstas nas abscissas contidas em \code{newdata}
#' 
#' @export
#' 
#' @family metodos gamprod

predict.gamprod <- function(object, newdata, ...) {

    pred <- predict(object$model, newdata = newdata)

    return(pred)
}

#' Valores Ajustados De Objetos \code{gamprod}
#' 
#' Método \code{fitted} para objetos da classe \code{gamprod}
#' 
#' @param object objeto \code{gamprod} retornado por \code{\link{fitgam_prod}}
#' @param ... existe apenas para consistência com a genérica
#' 
#' @return vetor de valores ajustados pelo modelo estimado
#' 
#' @export
#' 
#' @family metodos gamprod

fitted.gamprod <- function(object, ...) predict(object, object$dat)

#' Resíduos De Objetos \code{gamprod}
#' 
#' Método \code{residuals} para objetos da classe \code{gamprod}
#' 
#' @param object objeto \code{gamprod} retornado por \code{\link{fitgam_prod}}
#' @param ... existe apenas para consistência com a genérica
#' 
#' @return vetor de erros de ajuste do modelo estimado
#' 
#' @export
#' 
#' @family metodos gamprod

residuals.gamprod <- function(object, ...) {

    fit <- fitted(object)
    res <- object$dat$prod - fit

    return(res)
}

#' @export

AIC.gamprod <- function(object, ...) AIC(object$model)

#' @export

BIC.gamprod <- function(object, ...) BIC(object$model)