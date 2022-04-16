######################################### CLASSE GRIDPERDA #########################################

#' @export
#' 
#' @rdname extraigrid

extraigrid.gamprod <- function(fit, dim, ...) {

    ranges <- attr(fit, "ranges")
    quedal <- seq(ranges$quedal[1], ranges$quedal[2], length.out = dim[1])
    vazao  <- seq(ranges$vazao[1], ranges$vazao[2], length.out = dim[2])
    prod <- predict(fit, newdata = expand.grid(quedal = quedal, vazao = vazao))

    new_gridprod(prod, quedal, vazao, fit)
}

#' Construtor Interno
#' 
#' Função para contrução da saída de \code{extraigrid.gamprod}, não deve ser chamada diretamente
#' 
#' @param prod prod amostrada do modelo
#' @param quedal quedas líquidas correspondentes às segmentações da grade
#' @param vazao vazões correspondentes às segmentações da grade
#' @param fit modelo ajustado aos dados
#' 
#' @return objeto \code{gridprod} com a grade extraída e modelo original

new_gridprod <- function(prod, quedal, vazao, fit) {

    gridprod <- as.data.table(expand.grid(quedal = quedal, vazao = vazao))
    gridprod[, prod := prod]

    out <- list(grid = gridprod, model = fit)
    class(out) <- "gridprod"

    return(out)
}

#' Print De Objetos \code{gridprod}
#' 
#' @param x objeto da classe \code{gridprod}
#' @param ... exite apenas para consistência com a genérica
#' 
#' @return print da grade no console, sem retornar nenhum valor
#' 
#' @export

print.gridprod <- function(x, ...) print(x$grid)

# METODOS ------------------------------------------------------------------------------------------

#' Previsão Com Objetos \code{gridprod}
#' 
#' Método \code{predict} para objetos da classe \code{gridprod}
#' 
#' @param object objeto \code{gridprod} retornado por \code{\link{extraigrid.gamprod}}
#' @param newdata data.frame ou data.table contendo variável explicativa para previsão
#' @param ... existe apenas para consistência com a genérica
#' 
#' @return vetor de prods previstas nas abscissas contidas em \code{newdata}
#' 
#' @export
#' 
#' @family metodos gridprod

predict.gridprod <- function(object, newdata, ...) {

    grid  <- copy(object$grid)
    setorder(grid, vazao, quedal)

    quedal <- unique(grid$quedal)
    vazao  <- unique(grid$vazao)
    prod   <- data.matrix(dcast(grid, quedal ~ vazao, value.var = "prod")[, -1])

    ordem <- order(newdata$vazao)
    newdata <- newdata[ordem, ]

    interp <- INTERPBILIN(quedal, vazao, prod, newdata$quedal, newdata$vazao)
    interp <- interp[order(ordem)]

    return(interp)
}

#' Valores Ajustados De Objetos \code{gridprod}
#' 
#' Método \code{fitted} para objetos da classe \code{gridprod}
#' 
#' @param object objeto \code{gridprod} retornado por \code{\link{extraigrid.gamprod}}
#' @param ... existe apenas para consistência com a genérica
#' 
#' @return vetor de valores ajustados pelo modelo estimado
#' 
#' @export
#' 
#' @family metodos gridprod

fitted.gridprod <- function(object, ...) {

    quedal <- object$model$dat$quedal
    vazao  <- object$model$dat$vazao

    fit <- predict(object, newdata = expand.grid(quedal = quedal, vazao = vazao))

    return(fit)
}

#' Resíduos De Objetos \code{gridprod}
#' 
#' Método \code{residuals} para objetos da classe \code{gridprod}
#' 
#' @param object objeto \code{gridprod} retornado por \code{\link{extraigrid.gamprod}}
#' @param ... existe apenas para consistência com a genérica
#' 
#' @return vetor de erros de ajuste do modelo estimado
#' 
#' @export
#' 
#' @family metodos gridprod

residuals.gridprod <- function(object, ...) {

    fit <- fitted(object)
    res <- object$model$dat$prod - fit

    return(res)
}