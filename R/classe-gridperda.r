######################################### CLASSE GRIDPERDA #########################################

#' Extrator De Grades de Perda
#' 
#' Amostra \code{dim} pontos igualmente espaçacados no ajuste da curva de perdas
#' 
#' @param fit object \code{gamperda} retornado por \code{fitgam_perda} ou \code{optgam_perda}
#' @param dim inteiro indicando número de pontos na grade a ser extraída
#' @param ... existe apenas para consistência com a genérica
#' 
#' @return objeto da classe \code{gridperda} contendo grade extraída e ajuste original
#' 
#' @export
#' 
#' @rdname gridperda

extraigrid.gamperda <- function(fit, dim, ...) {

    dim <- dim[1]

    vazao <- seq(0, attr(fit, "qmax"), length.out = dim)
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
#' @rdname gridperda

new_gridperda <- function(perda, vazao, fit) {

    gridperda <- data.table(vazao = vazao, perda = perda)

    out <- list(grid = gridperda, gam = fit)
    class(out) <- "gridperda"

    return(out)
}

#' Print De Objetos \code{gridperda}
#' 
#' @param x objeto da classe \code{gridperda}
#' 
#' @export

print.gridperda <- function(x, ...) print(x$grid)

# METODOS ------------------------------------------------------------------------------------------

#' @param object objeto \code{gridperda} retornado por \code{\link{extraigrid.gamperda}}
#' @param newdata data.frame ou data.table contendo variável explicativa para previsão
#' @param ... demais parametros
#' 
#' @return objeto \code{gridperda} contendo grade extraída e modelo originalmente ajustado ao dado
#' 
#' @export
#' 
#' @rdname gridperda

predict.gridperda <- function(object, newdata, ...) {

    grid  <- object$grid
    setorder(grid, vazao)

    ordem <- order(newdata$vazao)
    vazao <- sort(newdata$vazao)

    interp <- INTERPLIN(grid$vazao, grid$perda, vazao)
    interp <- interp[order(ordem)]

    return(interp)
}

#' @export 
#' 
#' @rdname gridperda

fitted.gridperda <- function(object, ...) {

    vazao <- object$gam$dat[, list(vazao)]

    fit <- predict(object, newdata = vazao)

    return(fit)
}

#' @export
#' 
#' @rdname gridperda

residuals.gridperda <- function(object, ...) {

    fit <- fitted(object)
    res <- object$gam$dat$perda - fit

    return(res)
}