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
#' \code{\link{fitgam_prod}}, com uma exceção: o argumento \code{ns} é substituído por \code{range}, 
#' uma lista de dois vetores de inteiros indicando as dimensões de base candidatas em queda e vazão,
#' nesta ordem, para seleção. Mais detalhes a respeito da estimação e efeito dos argumentos 
#' individuais podem ser encontrados em \code{\link{fitgam_prod}}.
#' 
#' @param dat \code{data.table} de dados para ajuste. Ver Detalhes
#' @param range lista com dois vetores inteiros indicando dimensões de base em queda líquida e 
#'     vazão, nesta ordem, a testar na seleção. Todas as combinações serão testadas (um domínio 
#'     quadrado é gerado a partir dos ranges)
#' @param ts vetor de duas strings tipo de spline utilizada para queda líquida e vazão -- 
#'     veja \code{\link[mgcv]{smooth.terms}} para mais detalhes sobre as opções
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
#' 
#' # execucao limitando a faixa de numero de splines a algo mais baixo
#' optfit <- optgam_prod(dat, range = list(5:10, 5:10))
#' \dontrun{
#' plot(optfit)
#' }
#' 
#' @return objeto \code{gamprod} contendo GAM e extrapolações estimadas
#' 
#' @seealso ajuste individual para uma dada dimensão de base \code{\link{fitgam_prod}}
#' 
#' @family metodos gamprod
#' @family plots gamprod
#' 
#' @export

optgam_prod <- function(dat, range = list(5:30, 5:30), ts = c("ps", "ps"), dist = gaussian(),
    bordas = TRUE, modo = c("tensor", "multivar", "simples")) {

    ranges <- expand.grid(quedal = range[[1]], vazao = range[[2]])

    fitgams <- lapply(seq(nrow(ranges)), function(i)
        fitgam_prod(dat, unlist(ranges[i, ]), ts, dist, bordas, modo)
    )
    BICs <- sapply(fitgams, BIC)

    optgam <- fitgams[[which.min(BICs)]]

    return(optgam)
}
