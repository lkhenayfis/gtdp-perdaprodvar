###################################### FUNCOES DE VISUALIZACAO #####################################

# PLOT DE PERDAS -----------------------------------------------------------------------------------

#' Plot Completo De Objetos \code{gamperda}
#' 
#' Wrapper para visualização dos modelos contínuos de perda junto aos dados ajustados
#' 
#' @param x objeto da classe \code{gamperda}
#' @param ... outros objetos \code{gamperda} para adicionar ao plot
#' @param plot booleano indicando se o plot deve ser gerado ou apenas retornado invisivelmente
#' 
#' @examples 
#' 
#' dat <- agregasemana(dummydata)
#' mod1 <- fitgam_perda(dat)
#' mod2 <- fitgam_perda(dat, 5)
#' mod3 <- fitgam_perda(dat, 7)
#' 
#' \dontrun{
#' plot(mod1)
#' plot(mod1, mod2, mod3)
#' }
#' 
#' @return plot dos dados originais e ajuste(s) realizado(s)
#' 
#' @importFrom ggplot2 ggplot aes geom_point geom_line labs theme_bw
#' 
#' @export
#' 
#' @family plots gamperda

plot.gamperda <- function(x, ..., plot = TRUE) {

    vazao <- perda <- Ajuste <- NULL

    xlim <- seq(0, attr(x$dat, "qmax"))

    mods <- match.call()
    mods <- mods[names(mods) != "plot"]
    mods <- lapply(mods[-1], function(var) paste0(as.character(var), collapse = ""))

    pred <- c(list(x), list(...))
    pred <- lapply(pred, function(mod) predict(mod, data.frame(vazao = xlim)))

    dplot <- mapply(mods, pred, FUN = function(mod, pred) {
        data.frame(vazao = xlim, perda = pred, Ajuste = mod)
    }, SIMPLIFY = FALSE)

    dplot <- do.call(rbind, dplot)

    p <- ggplot() +
        geom_point(data = x$dat, aes(vazao, perda), inherit.aes = FALSE) +
        geom_line(data = dplot, aes(vazao, perda, color = Ajuste), lwd = 1.1) +
        labs(x = expression("Vaz\u00E3o Turbinada (m"^3 * "/s)"), y = "Perda (m)") +
        theme_bw()

    if(plot) print(p)

    invisible(p)
}

#' Plot Completo De Objetos \code{gridperda}
#' 
#' Wrapper para visualização dos modelos contínuos de perda, dados ajustados e grade extraída
#' 
#' @param x objeto da classe \code{gridperda}
#' @param plot booleano indicando se o plot deve ser gerado ou apenas retornado invisivelmente
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
#' @importFrom ggplot2 aes geom_point geom_line scale_color_discrete guides guide_legend
#' 
#' @export
#' 
#' @family plots gridperda

plot.gridperda <- function(x, plot = TRUE, ...) {

    vazao <- perda <- Ajuste <- NULL

    dplot <- cbind(x$grid, Ajuste = "Grid")

    p <- plot(x$model, plot = FALSE)

    p <- p +
        geom_point(data = dplot, aes(vazao, perda, color = Ajuste), shape = 1, size = 3, stroke = 1.1) +
        geom_line(data = dplot, aes(vazao, perda, color = Ajuste), lwd = 1.1) +
        scale_color_discrete(labels = c("Continuo", "Grade")) +
        guides(color = guide_legend(override.aes = list(shape = c(NA, 1))))

    if(plot) print(p)

    invisible(p)
}

#' Plot Da Varredura De Número De Divisões Para Perda
#' 
#' Wrapper para visualização do resultado da varredura realizada na otimização do tamanho de grade
#' 
#' @param x objeto da classe \code{gridperda}
#' @param plot booleano indicando se o plot deve ser gerado ou apenas retornado invisivelmente
#' @param ... existe apenas para consistência com a genérica
#' 
#' @examples 
#' 
#' dat <- agregasemana(dummydata)
#' mod <- fitgam_perda(dat)
#' grd <- optgrid(mod, full.output = TRUE)
#' 
#' \dontrun{
#' plot(grd[[2]])
#' }
#' 
#' @return plota dados originais, ajuste realizado e grade de perdas extraída
#' 
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_hline scale_color_manual
#' @importFrom ggplot2 coord_cartesian labs theme_bw
#' 
#' @export

plot.varreduraperda <- function(x, plot = TRUE, ...) {

    segs <- razao <- tipo <- X <- NULL

    tries <- data.table(segs = x$range, razao = x$razao[, 1])

    ruins    <- cbind(tries[segs <= x$front[1, X]], tipo = "Inadequado")
    estaveis <- cbind(tries[segs >= x$front[1, X]], tipo = "Est\u00E1vel")
    front    <- cbind(tries[segs == x$front[1, X]], tipo = "Fronteira")

    dplot <- rbind(ruins, front, estaveis)
    dplot$tipo <- factor(dplot$tipo, levels = unique(dplot$tipo))

    p <- ggplot(dplot, aes(segs, razao, group = tipo, color = tipo)) +
        geom_line() +
        geom_point(data = ruins) + geom_point(data = estaveis) + geom_point(data = front) +
        geom_hline(yintercept = x$R, color = "blue", lty = 2) +
        scale_color_manual(name = "",
            values = c("purple", "red", "yellow2")) +
        coord_cartesian(ylim = c(.95, 1.25)) +
        labs(x = "N\u00FAmero de segmenta\u00E7\u00F5es", y = "Raz\u00E3o entre erros") +
        theme_bw()

    if(plot) print(p)

    invisible(p)
}

# PLOT DE PRODUTIBILIDADE --------------------------------------------------------------------------

#' Plot Completo De Objetos \code{gamprod}
#' 
#' Wrapper para visualização dos modelos contínuos de produtibilidade junto aos dados ajustados
#' 
#' @param x objeto da classe \code{gamprod}
#' @param plot booleano indicando se o plot deve ser gerado ou apenas retornado invisivelmente
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

plot.gamprod <- function(x, plot = TRUE, ...) {

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
            z = t(data.matrix(dcast(fitt, quedal ~ vazao, value.var = "prod"))[, -1]),
            inherit = FALSE) %>%
        layout(scene =
            list(xaxis = list(titlefont = f1, tickfont = f2, title = "Queda l\u00edquida (m)"),
                yaxis = list(titlefont = f1, tickfont = f2, title = "Vaz\u00E3o turbinada (m<sup>3</sup>/s)"),
                zaxis = list(titlefont = f1, tickfont = f2, title = "Produtibilidade (MW/m<sup>4</sup>/s)"),
                camera = list(eye = list(x = -1.5, y = -1.6, z = 1.2))),
             legend = list(font = f1)) %>%
        hide_legend() %>%
        hide_colorbar()

    if(plot) print(p)

    invisible(p)
}

#' Plot Da Varredura De Número De Divisões Para Produtibilidade
#' 
#' Wrapper para visualização do resultado da varredura realizada na otimização do tamanho de grade
#' 
#' @param x objeto da classe \code{gridprod}
#' @param plot booleano indicando se o plot deve ser gerado ou apenas retornado invisivelmente
#' @param ... existe apenas para consistência com a genérica
#' 
#' @examples 
#' 
#' dat <- agregasemana(dummydata)
#' mod <- fitgam_prod(dat)
#' grd <- optgrid(mod, full.output = TRUE)
#' 
#' \dontrun{
#' plot(grd[[2]])
#' }
#' 
#' @return plota dados originais, ajuste realizado e grade de produtibilidade extraída
#' 
#' @importFrom plotly plot_ly add_markers layout %>%
#' 
#' @export

plot.varreduraprod <- function(x, plot = TRUE, ...) {

    drazao <- as.data.table(x$razao, keep.rownames = TRUE)
    drazao <- melt(drazao, id.vars = "rn", value.name = "razao")

    dpersis <- as.data.table(x$persis, keep.rownames = TRUE)
    dpersis <- melt(dpersis, id.vars = "rn", value.name = "persis")

    dplot <- merge(drazao, dpersis, by = c("rn", "variable"))
    dplot[, quedal := as.numeric(sub("X", "", rn))]
    dplot[, vazao  := as.numeric(sub("X", "", variable))]

    dplot[persis == FALSE, tipo := "Inadequado"]
    dplot[persis == TRUE, tipo := "Regi\u00E3o de persist\u00eancia"]

    dplot[dplot[x$front, on = list(quedal, vazao), which = TRUE], tipo := "Fronteira"]
    dplot[dplot[x$minprod, on = list(quedal, vazao), which = TRUE], tipo := "Menor Produto"]
    dplot[, tipo := factor(tipo,
        levels = c("Inadequado", "Fronteira", "Regi\u00E3o de persist\u00eancia", "Menor Produto"))]

    f1 <- list(size = 17, color = "black")
    f2 <- list(size = 12, color = "black")

    p <- plot_ly() %>%
        add_markers(data = dplot, x = ~quedal, y = ~vazao, z = ~razao, color = ~tipo,
            colors = c("#440154FF", "#21908CFF", "#FDE725FF", "red")) %>%
        layout(scene =
            list(xaxis = list(titlefont = f1, tickfont = f2, title = "Divis\u00F5es de queda l\u00edquida"),
                yaxis = list(titlefont = f1, tickfont = f2, title = "Divis\u00F5es de vaz\u00E3o turbinada"),
                zaxis = list(titlefont = f1, tickfont = f2, title = "Raz\u00E3o entre erros"),
                camera = list(eye = list(x = 2.1, y = 1, z = 1))),
            legend = list(font = f1))

    if(plot) print(p)

    invisible(p)
}