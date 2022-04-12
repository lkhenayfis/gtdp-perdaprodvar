########################################## DADOS DO PACOTE #########################################

#' Exemplo De Dado Semanal
#' 
#' Dado horário de uma usina do SIN reescalonado de modo a manter a anonimidade. 
#' 
#' @format \code{data.table} com as colunas
#' 
#' \describe{
#'     \item{data}{Data e hora do registro}
#'     \item{nmont}{Nível de montante}
#'     \item{quedal}{Queda líquida media}
#'     \item{perda}{Perda média}
#'     \item{vazao}{Vazão defluente tota}
#'     \item{prod}{Produtibilidade média}
#'     \item{nmaq}{NA, não faz sentido em média semanal}
#'     \item{patamar}{NA, não faz sentido em média semanal}
#'     \item{energia}{NA, não faz sentido em média semanal}
#' }
"dummydata"