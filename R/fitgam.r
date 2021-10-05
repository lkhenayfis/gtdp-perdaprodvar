############################### AJUSTE DE MODELOS ADITIVOS AOS DADOS ###############################

#' Ajuste De Curva Para Perdas
#' 
#' Estima GAM para perdas, complementando regiões inferior e superior com extrapolação
#' 
#' \code{fitgam_perda} espera que alguns atributos especiais estejam presetnes no argumento
#' \code{dat}, dentre eles: codigo, nome e vazao turbinada efetiva total da usina. Dentre estes,
#' apenas o último é relevante para a estimação de curva, sendo os outros dois necessários apenas
#' para escrita de resultados e plots. Desta forma, para garantir o funcionamento apropriado da 
#' função o objeto passado em \code{dat} deve ter sido lido e (possivelmente) agregado semanalmente
#' pelas funções fornecidas no pacote.
#' 
#' Para extrapolação, independentemente do tipo, duas partes do dado serão seccionadas: 
#' correspondente às vazões inferiores a \code{quantil[1]} e outra àquelas superiores a 
#' \code{quantil[2]}. Caso seja adotada extrapolação tipo 1:
#' 
#' 1. Para região inferior, uma equação do tipo \eqn{kQ^2} será ajustada entre a origem e o ponto de
#'    de menor perda
#' 1. Para região superior é feito o mesmo, porém para o ponto de maior vazão
#' 
#' Se for selecionada a extrapolação do tipo 2
#' 
#' 1. Ajusta-se uma curva \eqn{kQ^2} aos pontos da região inferior
#' 1. Ajusta-se uma curva \eqn{a + kQ^2} aos pontos da região superior
#' 
#' Uma vez realizadas as extrapolações, o ajuste final é composto chaveando modelos no cruzamento 
#' entre curvas. Caso não ocorra interseção, é utilizado o ponto de vazão no qual a distância entre 
#' elas seja a menor possível e um aviso será emitido.
#' 
#' @param dat \code{data.table} de dados para ajuste. Ver Detalhes
#' @param ns.vazao numero de nós no GAM ajustado. Padrao 10
#' @param ts.vazao tipo de spline utilizada para vazao -- um de \code{c("tp", "cr")}; veja
#'     \code{link[mgcv]{gam}} e Detalhes. Padrao "tp"
#' @param extrap vetor de duas posições indicando o tipo de extrapolação em cada região. Ver 
#'     Detalhes
#' @param quantil quantis para uso na extrapolação. Ver Detalhes
#' 
#' @return objeto \code{gamperda} contendo GAM e extrapolações estimadas
#' 
#' @seealso Métodos aplicáveis ao objeto retornado: \code{\link{predict.gamperda}},
#' \code{\link{fitted.gamperda}}, \code{\link{residuals.gamperda}}; assim como visualização 
#' \code{\link{plot.gamperda}}
#' 
#' @export

fitgam_perda <- function(dat, ns.vazao = 10, ts.vazao = "tp", extrap = c(1, 1), quantil = c(.05, .95)) {

    if(!(all(extrap %in% 1:2))) stop("extrap so pode assumir valor 1 ou 2")

    atributos <- attributes(dat)[c("cod", "nome", "nmaq", "qmax")]

    form <- as.formula(paste0("perda ~ s(vazao, bs = '", ts.vazao, "', k = ", ns.vazao, ")"))
    mod  <- mgcv::gam(form, data = dat)

    # Extrapolacao inferior ----------------------------------------------

    dI <- dat[vazao < quantile(vazao, quantil[1])]

    if(extrap[1] == 1) {
        coefI  <- dI[which.min(perda), predict(mod, .SD) / vazao^2]
        corteI <- dI[which.min(perda), vazao][1]
    } else {
        r     <- lm(perda ~ I(vazao^2) - 1, data = dI, )
        coefI <- r$coefficients

        vx <- seq(0, max(dI$vazao), length.out = 1000)
        vextrap <- coefI * vx^2
        vgam    <- predict(mod, newdata = data.frame(vazao = vx))

        vdiff <- vextrap - vgam
        vsign <- sign(vdiff)
        corteI <- which(diff(vsign) != 0)

        if(length(corteI) != 0) {
            corteI <- vx[corteI + 1][1]
        } else {
            corteI <- which.min(abs(vdiff))
            corteI <- vx[corteI][1]
            warning("Não há cruzamento entre a extrapolação inferior escolhida e o ajuste do GAM - foi utilizado o ponto mais próximo")
        }
    }

    # Extrapolacao superior ----------------------------------------------

    dS <- dat[vazao > quantile(vazao, quantil[2])]
    vx <- seq(min(min(dS$vazao), atributos$qmax), atributos$qmax, length.out = 1000)

    if(extrap[2] == 1) {
        coefS <- dS[which.max(vazao), perda / vazao^2]
        coefS <- c(0, coefS)
    } else {
        X   <- cbind(1, dS$vazao^2)
        Y   <- dS$perda
        e   <- matrix(c(1, 0, 0, 1), 2)
        f   <- c(0, 0)
        coefS <- lsei::lsei(X, Y, e = e, f = f)
    }

    vextrap <- coefS[1] + coefS[2] * vx^2
    vgam    <- predict(mod, newdata = data.frame(vazao = vx))

    vdiff <- vextrap - vgam
    vsign <- sign(vdiff)
    corteS <- which(diff(vsign) != 0)

    if(length(corteS) != 0) {
        corteS <- vx[corteS + 1][1]
    } else {
        corteS <- which.min(abs(vdiff))
        corteS <- vx[corteS][1]
        warning("Não há cruzamento entre a extrapolação superior escolhida e o ajuste do GAM - foi utilizado o ponto mais próximo")
    }

    # Monta objeto de saida ----------------------------------------------

    args <- list(ns.vazao = ns.vazao, ts.vazao = ts.vazao, extrap = extrap, quantil = quantil)

    new_gamperda(dat, mod, coefI, corteI, coefS, corteS, atributos, args)
}