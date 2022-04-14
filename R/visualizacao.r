###################################### FUNCOES DE VISUALIZACAO #####################################

# PLOT DE PERDAS -----------------------------------------------------------------------------------

#' @export
#' 
#' @rdname gamperda

plot.gamperda <- function(x, legenda = TRUE, ...) {

    xlim <- c(0, attr(x$dat, "qmax"))
    ylim <- c(0, max(x$dat$perda))

    xline <- seq(xlim[1], xlim[2])

    plot(x$dat, panel.first = grid(col = "grey85"), col = "deepskyblue2", pch = 16,
        xlab = expression("Vaz\u00E3o Turbinada (m"^3 * "/s)"), ylab = "Perda (m)",
        xlim = xlim, ylim = ylim)
    lines(xline, predict(x, newdata = data.frame(vazao = xline)), col = 2, lwd = 2)
    if(legenda) {
        legend("bottomright", inset = .02,
            legend = c("Dados hist\u00F3ricos", "Ajuste"),
            pch = c(16, NA), col = c("deepskyblue2", 2), lty = c(NA, 1), lwd = c(NA, 2))
    }
}

#' @export
#' 
#' @rdname gamperda

lines.gamperda <- function(x, ...) {

    xlim <- c(0, attr(x$dat, "qmax"))
    ylim <- c(0, max(x$dat$perda))

    xline <- seq(xlim[1], xlim[2])

    lines(xline, predict(x, newdata = data.frame(vazao = xline)), ...)
}

#' @export
#' 
#' @rdname gridperda

plot.gridperda <- function(x, legenda = TRUE, ...) {

    plot(x$model, legenda = FALSE)

    points(x$grid, type = "o", lwd = 2)
    if(legenda) {
        legend("bottomright", inset = .02,
            legend = c("Dados hist\u00F3ricos", "Ajuste", "Grade"),
            pch = c(16, NA, 1), col = c("deepskyblue2", 2, 1), lty = c(NA, 1, 1), lwd = c(NA, 2, 2))
    }
}

#' @export

plot.varreduraperda <- function(x, legenda = TRUE, ...) {

    estaveis <- x$front[1, X]:length(x$range)

    plot(x$range, x$razao, panel.first = grid(col = "grey85"), pch = 16, col = "purple",
        type = "o", lwd = 2,
        xlab = "N\u00FAmero de segmenta\u00E7\u00F5es", ylab = "Raz\u00E3o entre erros", ylim = c(.95, 1.25))
    points(x$range[estaveis], x$razao[estaveis], col = "yellow2", pch = 16, type = "o", lwd = 2)
    points(x$range[estaveis[1]], x$razao[estaveis[1]], col = "red", pch = 16, type = "o", lwd = 2)
    abline(h = x$R, col = "blue", lty = 2, lwd = 2)
    if(legenda) {
        legend("topright", inset = .02,
            legend = c("Inadequado", "Est\u00E1vel", "Fronteira"), pch = 16, col = c("purple", "yellow3", "red"))    
    }
}
