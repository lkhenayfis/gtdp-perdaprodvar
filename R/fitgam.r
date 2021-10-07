############################### AJUSTE DE MODELOS ADITIVOS AOS DADOS ###############################

#' Ajuste De Curva Para Perdas
#' 
#' Estima GAM para perdas, complementando regiões inferior e superior com extrapolação
#' 
#' \code{fitgam_perda} espera que alguns atributos especiais estejam presetnes no argumento
#' \code{dat}, dentre eles: codigo, nome e vazao turbinada efetiva total da usina. Destes, apenas
#' o último é relevante para a estimação de curva, sendo os outros dois necessários para escrita
#' de resultados e plots. Desta forma, para garantir o funcionamento apropriado da função o objeto 
#' passado em \code{dat} deve ter sido lido e (possivelmente) agregado semanalmente pelas funções 
#' fornecidas no pacote.
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
#' O argumento \code{ts.vazao} permite utilizar diferentes tipos de splines no ajuste do dado. Todas
#' as splines suportadas pelo pacote \code{mgcv} podem ser utilizadas na modelagem de perdas.
#' Como cada um dos tipos define uma base diferente, o resultado do ajuste com splines diferentes,
#' mesmo mantendo-se o número de nós, pode ser significativamente diferente.
#' 
#' @param dat \code{data.table} de dados para ajuste. Ver Detalhes
#' @param ns.vazao numero de nós no GAM ajustado. Padrao 10
#' @param ts.vazao tipo de spline utilizada para vazao -- um de \code{c("tp", "cr", "ps")}; veja
#'     \code{link[mgcv]{gam}} e Detalhes. Padrao "ps"
#' @param extrap vetor de duas posições indicando o tipo de extrapolação em cada região. Ver 
#'     Detalhes. Padrao tipo 2 para ambas as extrapolações
#' @param quantil quantis para uso na extrapolação. Ver Detalhes
#' 
#' @examples
#' 
#' dat <- agregasemana(dummydata)
#' 
#' # ajustes com diferentes tipos de splines (com 10 nós em todos os casos)
#' fit_tp <- fitgam_perda(dat, ns.vazao = 10, ts.vazao = "tp")
#' fit_cr <- fitgam_perda(dat, ns.vazao = 10, ts.vazao = "cr")
#' fit_ps <- fitgam_perda(dat, ns.vazao = 10, ts.vazao = "ps")
#' 
#' # objetos retornados por fitgam_perda possuem um metodo de plot e lines, para facil visualizacao
#' plot(fit_tp, legenda = FALSE)
#' lines(fit_cr, col = 3, lwd = 3)
#' lines(fit_ps, col = 6, lwd = 3)
#' legend("bottomright", inset = .02, legend = c("tp", "cr", "ps"), lty = 1, col = c(2, 3, 6))
#' 
#' @return objeto \code{gamperda} contendo GAM e extrapolações estimadas
#' 
#' @seealso Métodos aplicáveis ao objeto retornado: \code{\link{predict.gamperda}},
#'     \code{\link{fitted.gamperda}}, \code{\link{residuals.gamperda}}; assim como visualização 
#'     \code{\link{plot.gamperda}}. Função para otimização de nós \code{\list{optgam_perda}}
#' 
#' @export

fitgam_perda <- function(dat, ns.vazao = 10, ts.vazao = "ps", extrap = c(2, 2), quantil = c(.05, .95)) {

    if(!(all(extrap %in% 1:2))) stop("extrap so pode assumir valor 1 ou 2")

    atributos <- attributes(dat)[c("cod", "nome", "nmaq", "qmax")]

    mod  <- mgcv::gam(perda ~ s(vazao, bs = ts.vazao, k = ns.vazao), data = dat)

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

#' Ajuste De Curva Para Perdas
#' 
#' Estima GAM para perdas, complementando regiões inferior e superior com extrapolação

fitgam_prod <- function(dat, ns.quedal = 10, ns.vazao = 10, ts.quedal = "ps", ts.vazao = "ps", bordas = TRUE,
    modo = c("tensor", "multivar", "simples")) {

    modo <- match.arg(modo)

    atributos <- attributes(dat)[c("cod", "nome", "nmaq", "qmax")]

    m_borda <- bordasCC[usina == atributos$cod, 3:5]

    dfit <- rbind(dat, as.data.table(m_borda[bordas, , drop = FALSE]), fill = TRUE)

    if(modo == "simples") {
        term1 <- paste0("s(quedal, bs = '", ts.quedal, "', k = ", ns.quedal, ")")
        term2 <- paste0("s(vazao, bs = '", ts.vazao, "', k = ", ns.vazao, ")")
        form <- as.formula(paste0("prod ~ ", term1, " + ", term2))
    } else if(modo == "multivar") {
        if(!((ts.quedal == "tp") & (ts.vazao == "tp"))) stop("modo multivar so suporta tipos de spline 'tp'")

        term <- paste0("s(quedal, vazao, bs = 'tp', k = c(", ns.quedal, ", ", ns.vazao, "))")
        form <- as.formula(paste0("prod ~ ", term))
    } else {
        term <- paste0("te(quedal, vazao, bs = c('", ts.quedal, "', '", ts.vazao, "'), k = c(", ns.quedal, ", ", ns.vazao, "))")
        form <- as.formula(paste0("prod ~ ", term))
    }

    mod <- mgcv::gam(form, data = dfit)

    args <- list(ns.quedal = ns.quedal, ns.vazao = ns.vazao, ts.quedal = ts.quedal, ts.vazao = ts.vazao,
        bordas = bordas)

    new_gamprod(dat, mod, atributos, args)
}