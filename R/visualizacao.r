###################################### FUNCOES DE VISUALIZACAO #####################################

# PLOT DE PERDAS -----------------------------------------------------------------------------------

#' Plot Completo De Objetos \code{gamperda}
#' 
#' Wrapper para visualização dos modelos contínuos de perda junto aos dados ajustados
#' 
#' @param x objeto da classe \code{gamperda}
#' @param legenda booleano indicando se uma legenda automática deve ser adicionada
#' @param ... existe apenas para consistência com a genérica
#' 
#' @examples 
#' 
#' dat <- agregasemana(dummydata)
#' mod <- fitgam_perda(dat)
#' 
#' \dontrun{
#' plot(mod)
#' }
#' 
#' @return plota dados originais e ajuste realizado
#' 
#' @importFrom graphics plot lines points legend
#' 
#' @export
#' 
#' @family plots gamperda

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

#' Plot De Linha Dos Modelos Ajustados Para Perda
#' 
#' Wrapper para visualização apenas dos modelos ajustados, sem dados ao fundo
#' 
#' @param x objeto da classe \code{gamperda}
#' @param ... demais parâmetros passados à genérica \code{\link[graphics]{lines}}
#' 
#' @examples 
#' 
#' dat <- agregasemana(dummydata)
#' mod1 <- fitgam_perda(dat)
#' mod2 <- fitgam_perda(dat, 15)
#' 
#' \dontrun{
#' plot(mod1)
#' lines(mod2, col = 3, lwd = 2. lty = 2)
#' }
#' 
#' @return plota a linha do ajuste do modelo contido em \code{x}
#' 
#' @importFrom graphics lines
#' 
#' @export
#' 
#' @family plots gamperda

lines.gamperda <- function(x, ...) {

    xlim <- c(0, attr(x$dat, "qmax"))
    ylim <- c(0, max(x$dat$perda))

    xline <- seq(xlim[1], xlim[2])

    lines(xline, predict(x, newdata = data.frame(vazao = xline)), ...)
}

#' Plot Completo De Objetos \code{gridperda}
#' 
#' Wrapper para visualização dos modelos contínuos de perda, dados ajustados e grade extraída
#' 
#' @param x objeto da classe \code{gridperda}
#' @param legenda booleano indicando se uma legenda automática deve ser adicionada
#' @param ... existe apenas para consistência com a genérica
#' 
#' @examples 
#' 
#' dat <- agregasemana(dummydata)
#' mod <- fitgam_perda(dat)
#' grd <- extraigrid(mod, 20)
#' 
#' \dontrun{
#' plot(grd)
#' }
#' 
#' @return plota dados originais, ajuste realizado e grade de perdas extraída
#' 
#' @importFrom graphics plot points legend
#' 
#' @export
#' 
#' @family plots gridperda

plot.gridperda <- function(x, legenda = TRUE, ...) {

    plot(x$model, legenda = FALSE)

    points(x$grid, type = "o", lwd = 2)
    if(legenda) {
        legend("bottomright", inset = .02,
            legend = c("Dados hist\u00F3ricos", "Ajuste", "Grade"),
            pch = c(16, NA, 1), col = c("deepskyblue2", 2, 1), lty = c(NA, 1, 1), lwd = c(NA, 2, 2))
    }
}

#' @importFrom graphics plot abline points legend
#' 
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

# PLOT DE PRODUTIBILIDADE --------------------------------------------------------------------------

#' Plot Completo De Objetos \code{gamprod}
#' 
#' Wrapper para visualização dos modelos contínuos de produtibilidade junto aos dados ajustados
#' 
#' @param x objeto da classe \code{gamprod}
#' @param ... existe apenas para consistência com a genérica
#' 
#' @examples 
#' 
#' dat <- agregasemana(dummydata)
#' mod <- fitgam_prod(dat)
#' 
#' \dontrun{
#' plot(mod)
#' }
#' 
#' @return plota dados originais e ajuste realizado. Como o plot é 3d através do pacote 
#'     \code{plotly}, é retornado um objeto desta classe contendo o plot para posteriores 
#'     modificações
#' 
#' @importFrom plotly plot_ly add_markers add_surface hide_legend hide_colorbar layout %>%
#' 
#' @export
#' 
#' @family plots gamprod

plot.gamprod <- function(x, ...) {

    ranges <- attr(x, "ranges")
    newdata <- expand.grid(vazao = seq(ranges$vazao[1], ranges$vazao[2], length.out = 100),
                           quedal = seq(ranges$quedal[1], ranges$quedal[2], length.out = 100))
    setDT(newdata)
    fitt <- predict(x, newdata = newdata)
    fitt <- cbind(newdata, prod = fitt)

    f1 <- list(size = 17, color = "black")
    f2 <- list(size = 12, color = "black")
    p <- plot_ly() %>%
        add_markers(data = x$dat, type = "scatter3d",
            x = ~quedal, y = ~vazao, z = ~prod,
             marker = list(color = "deepskyblue2", size = 12, opacity = 1)) %>%
        add_surface(x = unique(fitt$quedal), y = unique(fitt$vazao),
            z = data.matrix(dcast(fitt, quedal ~ vazao, value.var = "prod"))[, -1],
            inherit = FALSE) %>%
        layout(scene =
            list(xaxis = list(titlefont = f1, tickfont = f2, title = "Queda l\u00edquida (m)"),
                yaxis = list(titlefont = f1, tickfont = f2, title = "Vaz\u00E3o turbinada (m<sup>3</sup>/s)"),
                zaxis = list(titlefont = f1, tickfont = f2, title = "Produtibilidade (MW/m<sup>4</sup>/s)"),
                camera = list(eye = list(x = -1.5, y = -1.6, z = 1.2))),
             legend = list(font = f1)) %>%
        hide_legend() %>%
        hide_colorbar()

    return(p)
}