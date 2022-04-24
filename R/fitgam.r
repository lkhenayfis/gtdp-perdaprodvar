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
#' \code{ts.vazao} permite a especificação do tipo de spline utilizada na expansão de base. Todos os 
#' tipos definidos em \code{\link[mgcv]{mgcv}} são suportados. Para maiores detalhes a respeito das 
#' possibilidades e suas descrições, veja \code{\link[mgcv]{smooth.terms}}. Por padrão é utilizado 
#' \code{ts.vazao = "ps"}, o que corresponde à expansão por P-Splines.
#' 
#' @param dat \code{data.table} de dados para ajuste. Ver Detalhes
#' @param ns.vazao dimensão da base expandida para ajuste. Padrão 10
#' @param ts.vazao tipo de spline utilizada para vazão -- veja \code{\link[mgcv]{smooth.terms}} para
#'     todas as opções. Padrão \code{"ps"}
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
#' # valores ajustados, previsao e residuos
#' res <- residuals(fit_tp)
#' fitt <- fitted(fit_tp)
#' predd <- predict(fit_tp, newdata = data.frame(vazao = runif(100, 20, 150)))
#' 
#' # objetos retornados por fitgam_perda possuem um metodo de plot e lines, para facil visualizacao
#' \dontrun{
#' plot(fit_tp)
#' plot(fit_tp, fit_cr, fit_ps)
#' }
#' 
#' @return objeto \code{gamperda} contendo modelo aditivo e extrapolações estimadas
#' 
#' @seealso otimização da dimensão de base \code{\link{optgam_perda}}.
#' 
#' @family metodos gamperda
#' @family plots gamperda
#' 
#' @export

fitgam_perda <- function(dat, ns.vazao = 10, ts.vazao = "ps", extrap = c(2, 2), quantil = c(.05, .95)) {

    vazao <- perda <- NULL

    # um jeito um pouco mais longo de pegar a call, mas guarda inclusive os args default
    fc <- match.call()
    da <- formals()
    da[match(names(fc[-1]), names(da))] <- fc[-1]
    fc <- as.call(c(fc[[1]], da))
    fc[-1] <- lapply(fc[-1], eval, envir = parent.frame())

    wrn <- paste0("Nao ha cruzamento entre a extrapolacao inferior escolhida e o ajuste do GAM - ",
        "foi utilizado o ponto mais proximo")

    if(!(all(extrap %in% 1:2))) stop("extrap so pode assumir valor 1 ou 2")

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
            warning(wrn)
        }
    }

    # Extrapolacao superior ----------------------------------------------

    dS <- dat[vazao > quantile(vazao, quantil[2])]
    vx <- seq(min(min(dS$vazao), attr(dat, "qmax")), attr(dat, "qmax"), length.out = 1000)

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
        warning(wrn)
    }

    # Monta objeto de saida ----------------------------------------------

    new_gamperda(dat, mod, coefI, coefS, corteI, corteS, fc)
}

#' Ajuste De Superfície Para Produtibilidade
#' 
#' Estima GAM para produtibilidades
#' 
#' \code{fitgam_prod} espera que alguns atributos especiais estejam presetnes no argumento
#' \code{dat}, dentre eles: codigo, nome e vazao turbinada efetiva total da usina. Destes, apenas
#' o último é relevante para a estimação de curva, sendo os outros dois necessários para escrita
#' de resultados e plots. Desta forma, para garantir o funcionamento apropriado da função o objeto 
#' passado em \code{dat} deve ter sido lido e (possivelmente) agregado semanalmente pelas funções 
#' fornecidas no pacote.
#' 
#' \code{ts} permite a especificação do tipo de spline utilizada na expansão de base em cada 
#' marginal. A maioria das opções suportadas pelo pacote \code{mgcv} são utilizáveis, com algumas 
#' exceções que não condizem com esta aplicação específica. \code{fitgam_prod} suporta as splines 
#' livres
#' 
#' \itemize{
#' \item \code{"tp"}: thin-plate regression splines
#' \item \code{"ts"}: thin-plate regression splines com encolhimento
#' \item \code{"ds"}: duchon splines
#' \item \code{"cr"}: cubic regression splines
#' \item \code{"cs"}: cubic regression splines com encolhimento
#' \item \code{"ps"}: P-splines
#' }
#' 
#' Para maiores detalhes a respeito das possibilidades e suas descrições, veja
#' \code{\link[mgcv]{smooth.terms}}. Por padrão é utilizado  \code{ts.vazao = "ps"}, o que 
#' corresponde à expansão por P-Splines.
#' 
#' Adicionalmente às splines denominadas livres, \code{fitgam_prod} também suporta o uso de shape 
#' constrained P-splines, especificamente uma reparametrização que garante uma superfície côncava no
#' domínio de ajuste, via \code{ts = c("cv", "cv")}. Maiores detalhes a respeito desta modelagem 
#' encontram-se em \code{link[scam]{scam}} e \code{\link[scam]{shape.constrained.smooth.terms}}. 
#' Embora existam ainda outras diversas parametrizações de shape constraints, apenas a concavidade 
#' faz sentido para produtibilidade.
#' 
#' Embora a função ofereça a possibilidade de utilizar ou não as bordas da curva colina como apoio 
#' para estimação do modelo, o domínio final é sempre o mesmo:
#' \itemize{
#' \item o ajuste de produtibilidade sempre cobre da vazão zero até o máximo entre a vazão histórica
#'     e aquela estipulada no HIDR, ou seja, mesmo que colina tenha bordas em vazões superiores, o 
#'     ajuste NÃO vai até este ponto, dado que o DECOMP não consultará vazões tão elevadas
#' \item o domínio de queda líquida, por outro lado, sempre vai da menor a maior queda observadas na
#'     curva colina, mesmo que a borda não seja contemplada no ajuste. Isto é feito de modo a 
#'     garantir que o ajuste possua o domínio completo de quedas operáveis pelas máquinas da usina
#' }
#' 
#' Ainda no que toca a borda, seu uso pode ser informado de duas formas: um valor lógico ou vetor 
#' numérico. Caso o primeiro seja informado, todos os pontos da borda são utilizados ou não. Caso 
#' se informe um vetor numérico, apenas os pontos listados neste vetor serão utilizados. A 
#' disposição do dado de borda é:
#' \enumerate{
#'     \item menor queda, menor vazão
#'     \item MAIOR queda, menor vazão
#'     \item menor queda, MAIOR vazão
#'     \item MAIOR queda, MAIOR vazão
#' }
#' 
#' O argumento \code{modo} permite definir a estrutura geral do modelo aditivo. 
#' \code{modo = "simples"} implica em um modelo de efeitos aditivos sem iteração, isto é, a função
#' estimada é da forma \eqn{g(x) = f_1(X_1) + f_2(X_2)}, sem interação entre as variáveis. 
#' \code{modo = "tensor"} construirá uma função por produto tensor das marginais, isto é, levando em
#' consideração a interação entre variáveis. \code{modo = "multivar"} só é aplicável quando ambos 
#' \code{ts.quedal = ts.vazao = "tp"|"ts"}, pois corresponde a modelagem com suavizadores 
#' naturalmente multivariados, que é o caso singular das thin plate regression splines.
#' 
#' @param dat \code{data.table} de dados para ajuste. Ver Detalhes
#' @param ns vetor dois inteiros, indicando dimensão da base em queda líquida e vazão, nesta ordem
#' @param ts vetor de duas strings tipo de spline utilizada para queda líquida e vazão, 
#'     respectivamente -- veja \code{\link[mgcv]{smooth.terms}} para mais detalhes sobre as opções
#' @param dist objeto da classe \code{family} indicando a distribuição e link a serem usados no 
#'     ajuste. Veja \code{\link[stats]{family}} e \code{\link[stats]{glm}} para mais informações.
#' @param bordas booleano indicando o uso ou não de bordas; alternativamente, um vetor de inteiros 
#'     indicando quais vértices utilizar. Ver Detalhes
#' @param modo um de \code{"tensor"}, \code{"multivar"} ou \code{"simples"} indicando o modo de 
#'     interação entre funções marginais. Ver Detalhes
#' 
#' @examples
#' 
#' dat <- agregasemana(dummydata)
#' # dimensao de base 8 para queda e 10 para vazao -- thin plate splines para queda e vazao
#' mod <- fitgam_prod(dat, ns = c(8, 10), ts = c("tp", "tp"))
#' 
#' # valores ajustados, previsao e residuos
#' res <- residuals(mod)
#' fitt <- fitted(mod)
#' predd <- predict(mod, newdata = data.frame(quedal = runif(100, 20.5, 21),
#' vazao = runif(100, 20, 150)))
#' 
#' # objetos retornados por fitgam_prod possuem um metodo de plot e lines, para facil visualizacao
#' \dontrun{
#' plot(mod)
#' }
#' 
#' @return objeto \code{gamprod} contendo modelo aditivo ajustado
#' 
#' @seealso otimização da dimensão de base \code{\link{optgam_prod}}.
#' 
#' @family metodos gamprod
#' @family plots gamprod
#' 
#' @export

fitgam_prod <- function(dat, ns = c(10, 10), ts = c("ps", "ps"), dist = gaussian(),
    bordas = TRUE, modo = c("tensor", "multivar", "simples")) {

    usina <- vazao <- ponto <- NULL

    # um jeito um pouco mais longo de pegar a call, mas guarda inclusive os args default
    fc <- match.call()
    da <- formals()
    da[match(names(fc[-1]), names(da))] <- fc[-1]
    fc <- as.call(c(fc[[1]], da))
    fc[-1] <- lapply(fc[-1], eval, envir = parent.frame())

    modo <- match.arg(modo)

    borda <- bordasCC[usina == attr(dat, "cod")]
    borda[ponto %in% 3:4, vazao := vazao * attr(dat, "nmaq")]

    dfit <- rbind(dat, as.data.table(borda[bordas, 3:5, drop = FALSE]), fill = TRUE)

    free_splines  <- c("tp", "ts", "ds", "cr", "cs", "ps")
    shape_splines <- c("cv")

    is_fs <- all(ts %in% free_splines)
    is_sc <- all(ts %in% shape_splines)

    if(!(is_fs | is_sc)) stop("'ts' contem um tipo de spline nao suportado")

    if(modo == "simples") {
        term1 <- paste0("s(quedal, bs = '", ts[1], "', k = ", ns[1], ")")
        term2 <- paste0("s(vazao, bs = '", ts[2], "', k = ", ns[2], ")")
        form <- as.formula(paste0("prod ~ ", term1, " + ", term2))
    } else if(modo == "multivar") {
        if(!all(ts %in% c("tp", "ts"))) {
            stop("modo multivar so suporta tipos de spline 'tp' ou 'ts'")
        }

        term <- paste0("s(quedal, vazao, bs = 'tp', k = c(", ns[1], ", ", ns[2], "))")
        form <- as.formula(paste0("prod ~ ", term))
    } else {
        if(is_sc) stop("Splines marginais 'cv' nao sao suportadas em produto tensor -- use modo = 'simples'")
        termbs <- paste0(ts, collapse = "', '")
        termns <- paste0(ns, collapse = ", ")
        term <- paste0("te(quedal, vazao, bs = c('", termbs, "'), k = c(", termns, "))")
        form <- as.formula(paste0("prod ~ ", term))
    }

    # quando se esta estimando um SC GAM, forca a penalidade para zero pois o modelo ja sera concavo
    if(is_sc) SP <- c(0, 0) else SP <- NULL
    
    CALL <- as.call(list(quote(mgcv::gam), form, quote(dist), quote(dfit), sp = SP))
    if(is_sc) CALL[[1]] <- quote(scam::scam)

    mod <- eval(CALL)

    new_gamprod(dat, mod, fc)
}
