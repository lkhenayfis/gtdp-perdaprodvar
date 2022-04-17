##################################### OTIMIZACAO DO AJUSTE GAM #####################################

#' Otimização Do Ajuste De Perdas
#' 
#' Varre uma faixa de dimensões de base, reportando aquela correspondente ao ajuste de menor BIC
#' 
#' Os parâmetros recebidos por \code{optgam_perda} são exatamente aqueles de 
#' \code{\link{fitgam_perda}}, com uma exceção: o argumento \code{ns.vazao} é substituído por 
#' \code{range.vazao}, um vetor de inteiros indicando as dimensões de base candidatas para seleção.
#' Mais detalhes a respeito da estimação e efeito dos argumentos individuais podem ser encontrados 
#' em \code{\link{fitgam_perda}}
#' 
#' @param dat \code{data.table} de dados para ajuste. Ver Detalhes
#' @param range.vazao vetor de dimensões de base a testar na seleção. Padrão 
#'     \code{range.vazao = 5:30}
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
#' # execucao limitando a faixa de numero de splines a algo mais baixo
#' optfit <- optgam_perda(dat, range.vazao = 5:10)
#' \dontrun{
#' plot(optfit)
#' }
#' 
#' @return objeto \code{gamperda} contendo GAM e extrapolações estimadas
#' 
#' @seealso ajuste individual para uma dada dimensão de base \code{\link{fitgam_perda}}
#' 
#' @family metodos gamperda
#' @family plots gamperda
#' 
#' @export

optgam_perda <- function(dat, range.vazao = 5:30, ts.vazao = "ps", extrap = c(2, 2), quantil = c(.05, .95)) {

    fitgams <- lapply(range.vazao, function(ns) fitgam_perda(dat, ns, ts.vazao, extrap, quantil))
    BICs    <- sapply(fitgams, BIC)

    optgam <- fitgams[[which.min(BICs)]]

    return(optgam)
}

#' Otimização Do Ajuste De Produtibilidade
#' 
#' Varre uma faixa de dimensões de base, reportando aquela correspondente ao ajuste de menor BIC
#' 
#' Os parâmetros recebidos por \code{optgam_prod} são exatamente aqueles de 
#' \code{\link{fitgam_prod}}, com uma exceção: os argumentos \code{ns.quedal} e \code{ns.vazao} são
#' substituídos por \code{range.quedal} e \code{range.vazao}, vetores de inteiros indicando as 
#' dimensões de base candidatas em cada marginal para seleção.
#' Mais detalhes a respeito da estimação e efeito dos argumentos individuais podem ser encontrados 
#' em \code{\link{fitgam_prod}}
#' 
#' @param dat \code{data.table} de dados para ajuste. Ver Detalhes
#' @param range.quedal,range.vazao vetor de dimensões de base a testar na seleção. Todas as
#'     combinações serão testadas (isto é, um domínio quadrado é gerado a partir dos ranges). Padrão
#'     \code{range.quedal = range.vazao = 5:30}
#' @param ts.quedal,ts.vazao tipo de spline utilizada para queda líquida e vazão -- 
#'     veja \code{\link[mgcv]{smooth.terms}} para todas as opções. Padrão \code{"ps"} em ambos
#' @param bordas booleano indicando o uso ou não de bordas; alternativamente, um vetor de inteiros 
#'     indicando quais vértices utilizar. Ver Detalhes
#' @param modo um de \code{"tensor"}, \code{"multivar"} ou \code{"simples"} indicando o modo de 
#'     interação entre funções marginais. Ver Detalhes
#' 
#' @examples 
#' 
#' dat <- agregasemana(dummydata)
#' 
#' # execucao limitando a faixa de numero de splines a algo mais baixo
#' optfit <- optgam_prod(dat, range.quedal = 5:10, range.vazao = 5:10)
#' plot(optfit)
#' 
#' # dentre os argumentos de optfit esta a lista 'gamargs', contendo a parametrizacao do gam
#' attr(optfit, "gamargs") # ns.vazao corresponde ao numero de nos utilizados no ajuste otimo
#' 
#' @return objeto \code{gamprod} contendo GAM e extrapolações estimadas
#' 
#' @seealso ajuste individual para uma dada dimensão de base \code{\link{fitgam_prod}}
#' 
#' @family metodos gamprod
#' @family plots gamprod
#' 
#' @export

optgam_prod <- function(dat, range.quedal = 5:30, range.vazao = 5:30, ts.quedal = "ps", ts.vazao = "ps",
    bordas = TRUE, modo = c("tensor", "multivar", "simples")) {

    ranges <- expand.grid(quedal = range.quedal, vazao = range.vazao)

    fitgams <- lapply(seq(nrow(ranges)), function(i)
        fitgam_prod(dat, ranges[i, 1], ranges[i, 2], ts.quedal, ts.vazao, bordas, modo)
    )
    BICs <- sapply(fitgams, BIC)

    optgam <- fitgams[[which.min(BICs)]]

    return(optgam)
}
