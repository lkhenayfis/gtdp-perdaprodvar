######################################### CLASSE GRIDPERDA #########################################

extraigrade.gamperda <- function(fit, dim, ...) {

    dim <- dim[1]

    vazao <- seq(0, attr(fit, "qmax"), length.out = dim)
    perda <- predict(fit, newdata = data.frame(vazao = vazao))

    new_gridperda(perda, vazao, fit)
}

new_gridperda <- function(perda, vazao, fit) {

    gridperda <- data.table(vazao = vazao, perda = perda)

    out <- list(grid = gridperda, gam = fit)
    class(out) <- "gridperda"
}
