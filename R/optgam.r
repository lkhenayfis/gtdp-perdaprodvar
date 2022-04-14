##################################### OTIMIZACAO DO AJUSTE GAM #####################################

#' Otimização Do Ajuste De Perdas
#' 
#' Varre uma faixa de dimensões de base, reportando aquele correspondente ao ajuste de menor BIC
#' 
#' Os parâmetros recebidos por \code{optgam_perda} são exatamente aqueles de 
#' \code{\link{fitgam_perda}}, com uma exceção: o argumento \code{ns.vazao} é substituído por 
#' \code{range.vazao}, um vetor de inteiros indicando as dimensões de base candidatos para seleção.
#' 
#' @param dat \code{data.table} de dados para ajuste. Ver Detalhes
#' @param range.vazao vetor de numero de nós a testar na seleção. Padrao 5:30
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
#' plot(optfit)
#' 
#' # dentre os argumentos de optfit esta a lista 'gamargs', contendo a parametrizacao do gam
#' attr(optfit, "gamargs") # ns.vazao corresponde ao numero de nos utilizados no ajuste otimo
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
#' Varre uma faixa de dimensões de base, reportando aquele correspondente ao ajuste de menor BIC

optgam_prod <- function() {

    return(NULL)
}

# HELPERS ------------------------------------------------------------------------------------------

matchattr <- function(obj1, obj2, quais = c("cod", "ts")) {

    quais <- paste0("(", paste0(quais, collapse = ")|("), ")")

    attr1 <- attributes(obj1)
    attr2 <- attributes(obj2)

    attr1 <- attr1[grep(quais, names(attr1))]
    attr2 <- attr2[grep(quais, names(attr2))]

    all(mapply(attr1, attr2, FUN = "=="))
}