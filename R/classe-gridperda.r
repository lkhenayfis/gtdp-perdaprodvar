######################################### CLASSE GRIDPERDA #########################################

extraigrid.gamperda <- function(fit, dim, ...) {

    dim <- dim[1]

    vazao <- seq(0, attr(fit, "qmax"), length.out = dim)
    perda <- predict(fit, newdata = data.frame(vazao = vazao))

    new_gridperda(perda, vazao, fit)
}

new_gridperda <- function(perda, vazao, fit) {

    gridperda <- data.table(vazao = vazao, perda = perda)

    out <- list(grid = gridperda, gam = fit)
    class(out) <- "gridperda"

    return(out)
}

print.gridperda <- function(x, ...) print(gg$grid)

# METODOS ------------------------------------------------------------------------------------------

predict.gridperda <- function(object, newdata, ...) {

    grid  <- object$grid
    setorder(grid, vazao)

    vazao <- sort(newdata$vazao)

    interp <- INTERPLIN(grid$vazao, grid$perda, vazao)

    return(interp)
}

reiduals.gridperda <- function(object, ...) {

    vazao <- object$gam$dat$vazao

    fit <- predict(object, newdata = data.frame(vazao = vazao))

    res <- object$gam$dat$perda - fit

    return(res)
}