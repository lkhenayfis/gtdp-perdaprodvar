######################################### CLASSE GRIDPERDA #########################################

#' @export
#' 
#' @rdname extraigrid

extraigrid.gamperda <- function(fit, dim, ...) {

    dim <- dim[1]

    vazao <- seq(0, attr(fit$dat, "qmax"), length.out = dim)
    perda <- predict(fit, newdata = data.frame(vazao = vazao))

    new_gridperda(perda, vazao, fit)
}

#' Construtor Interno
#' 
#' Função para contrução da saída de \code{extraigrid.gamperda}, não deve ser chamada diretamente
#' 
#' @param perda perda amostrada do modelo
#' @param vazao vazoes correspondentes às segmentações da grade
#' @param fit modelo ajustado aos dados
#' 
#' @return objeto \code{gridperda} com a grade extraída e modelo original

new_gridperda <- function(perda, vazao, fit) {

    gridperda <- data.table(vazao = vazao, perda = perda)
    gridperda[vazao == 0, perda := 0]

    out <- list(grid = gridperda, model = fit)
    class(out) <- "gridperda"

    return(out)
}

#' Print De Objetos \code{gridperda}
#' 
#' @param x objeto da classe \code{gridperda}
#' @param ... exite apenas para consistência com a genérica
#' 
#' @return print da grade no console, sem retornar nenhum valor
#' 
#' @export

print.gridperda <- function(x, ...) print(x$grid)

# METODOS ------------------------------------------------------------------------------------------

#' Previsão Com Objetos \code{gridperda}
#' 
#' Método \code{predict} para objetos da classe \code{gridperda}
#' 
#' @param object objeto \code{gridperda} retornado por \code{\link{extraigrid.gamperda}}
#' @param newdata data.frame ou data.table contendo variável explicativa para previsão
#' @param ... existe apenas para consistência com a genérica
#' 
#' @return vetor de perdas previstas nas abscissas contidas em \code{newdata}
#' 
#' @export
#' 
#' @family metodos gridperda

predict.gridperda <- function(object, newdata, ...) {

    grid  <- object$grid
    setorder(grid, vazao)

    ordem <- order(newdata$vazao)
    vazao <- sort(newdata$vazao)

    interp <- INTERPLIN(grid$vazao, grid$perda, vazao)
    interp <- interp[order(ordem)]

    return(interp)
}

#' Valores Ajustados De Objetos \code{gridperda}
#' 
#' Método \code{fitted} para objetos da classe \code{gridperda}
#' 
#' @param object objeto \code{gridperda} retornado por \code{\link{extraigrid.gamperda}}
#' @param ... existe apenas para consistência com a genérica
#' 
#' @return vetor de valores ajustados pelo modelo estimado
#' 
#' @export
#' 
#' @family metodos gridperda

fitted.gridperda <- function(object, ...) {

    vazao <- object$model$dat[, list(vazao)]

    fit <- predict(object, newdata = vazao)

    return(fit)
}

#' Resíduos De Objetos \code{gridperda}
#' 
#' Método \code{residuals} para objetos da classe \code{gridperda}
#' 
#' @param object objeto \code{gridperda} retornado por \code{\link{extraigrid.gamperda}}
#' @param ... existe apenas para consistência com a genérica
#' 
#' @return vetor de erros de ajuste do modelo estimado
#' 
#' @export
#' 
#' @family metodos gridperda

residuals.gridperda <- function(object, ...) {

    fit <- fitted(object)
    res <- object$model$dat$perda - fit

    return(res)
}