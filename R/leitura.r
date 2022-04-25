############################ FUNCOES PARA LEITURA E TRATAMENTO DE DADOS ############################

#' Interface Com a Planilha Padrão
#' 
#' Realiza a leitura e tratamento dos dados na planilha padrão do GTDP
#' 
#' @param arq caminho completo do arquivo a ser lido
#' 
#' @return \code{data.table} em base horária contendo as variáveis
#'     \describe{
#'         \item{datahora}{Data e hora do registro}
#'         \item{patamar}{Patamar de carga}
#'         \item{energia}{Energia gerada total da usina}
#'         \item{prod}{Produtibilidade média}
#'         \item{vazao}{Vazão defluente tota}
#'         \item{nmaq}{Número de máquinas em operação}
#'         \item{nmont}{Nível de montante}
#'         \item{perda}{Perda média}
#'         \item{quedal}{Queda líquida média}
#'     }
#' 
#' @importFrom readxl read_xlsx
#' 
#' @export

leplanilha <- function(arq) {

    Qef <- datahora <- data <- hora <- patamar <- energia <- vazao <- nmont <- njus <- quedal <- NULL

    letrasExcel <- c(LETTERS, c(t(outer(LETTERS, LETTERS, paste0))))

    options(readxl.show_progress = FALSE)

    # Leitura de Cadastro ------------------------------------------------

    usina <- read_xlsx(arq, sheet = "Cadastro", range = "C1:C2", col_names = FALSE, .name_repair = "minimal")
    nome <- as.character(usina[1, 1])
    cod  <- as.numeric(usina[2, 1])

    maqs <- setDT(read_xlsx(arq, sheet = "Cadastro", range = "E15:G300", .name_repair = "minimal"))
    maqs <- maqs[complete.cases(maqs)]
    qmax  <- maqs[, sum(Qef)]
    nmaq <- maqs[, .N]

    # Leitura de Aberturas -----------------------------------------------

    info <- read_xlsx(arq, sheet = "Abertura(1)", range = "G13:G18", .name_repair = "minimal")
    info <- data.matrix(info)
    G   <- as.numeric(ifelse(!is.na(info[4, ]), info[4, ], info[1, ]))
    rho <- as.numeric(ifelse(!is.na(info[5, ]), info[5, ], info[2, ]))

    # Leitura de Rendimentos ---------------------------------------------

    rend <- leaba(arq, "Rendimento Usina (hora)", "A2:F87650",
        c("data", "hora", "rend", "energia", "diasem", "patamar"),
        c("date", "numeric",  "numeric", "numeric", "text", "text"))

    rend[, prod := rend * rho * G * 1e-6]
    rend <- rend[, list(datahora, patamar, energia, prod)]
    setkey(rend, datahora)

    # Leitura de Vazao Turbinada -----------------------------------------

    turb <- leaba(arq, "QTurb", paste0("A2:", letrasExcel[2 + nmaq + 1], "87650"),
        c("data", "hora", paste0("vaz_maq", seq(nmaq)), "vazao"),
        c("date", rep("numeric", 1 + nmaq + 1)))

    nmaqs <- turb[, lapply(.SD, function(x) is.na(x) | (x == 0)), .SDcols = names(turb) %like% "vaz_maq"]
    turb[, nmaq := rowSums(!nmaqs)]

    turb <- turb[, list(datahora, vazao, nmaq)]
    setkey(turb, datahora)

    # Leitura de Dados Hidrologicos --------------------------------------

    quedab <- leaba(arq, "Dados", "A2:E87650",
        c("data", "hora", "XXX", "nmont", "njus"),
        c("date", rep("numeric", 4)))

    quedab[, quedab := nmont - njus]
    quedab <- quedab[, list(datahora, nmont, quedab)]
    setkey(quedab, datahora)

    # Leitura de Perdas --------------------------------------------------

    perda <- leaba(arq, "Perda Usina (hora)", "A2:C87650",
        c("data", "hora", "perda"),
        c("date", rep("numeric", 2)))

    perda <- perda[, list(datahora, perda)]
    setkey(perda, datahora)

    # Junta tudo ---------------------------------------------------------

    out <- Reduce(function(d1, d2) merge(d1, d2, all = FALSE), list(rend, turb, quedab, perda))
    out[, quedal := quedab - perda]
    out[, quedab := NULL]

    attr(out, "cod")  <- cod
    attr(out, "nome") <- nome
    attr(out, "nmaq") <- nmaq
    attr(out, "qmax") <- qmax

    return(out)
}

#' Leitura De Uma Aba Da Planilha Padrão
#' 
#' Função interna para leitura das múltiplas abas da planilha padrão
#' 
#' @param arq caminho completo da planilha
#' @param aba nome ou índice da aba a ser lida
#' @param range range no formato excel de células para ler, incluindo celulas com nome das colunas
#' @param nomes nomes para atribuir às colunas
#' @param tipos tipo de dado em cada coluna sendo lida
#' 
#' @return \code{data.table} contendo o range na aba especificados

leaba <- function(arq, aba, range, nomes, tipos) {

    data <- hora <- datahora <- NULL

    out <- read_xlsx(arq, sheet = aba, range = range, col_types = tipos, .name_repair = "minimal")
    out <- setDT(out)
    colnames(out) <- nomes

    allNA <- out[, Reduce("&", lapply(.SD, is.na))]
    out <- out[!allNA]

    out[, datahora := as.POSIXct(paste0(data, " ", hora - 1, ":00"), format = "%Y-%m-%d %H:%M", "GMT")]

    return(out)
}

#' Agregação Semanal
#' 
#' Agrega dado horário extraído da planilha em semanas, criticando por um número mínimo de registros
#' 
#' O argumento \code{dat} pode ser tanto um data.table do dado já lido e retornado por 
#' \code{leplanilha} ou uma string contendo o caminho do dado. Neste segundo caso, há duas
#' possibilidades:
#' 
#' 1. Se \code{dat} é um caminho para arquivo com extensão ".RDS", será lido e agregado
#' 1. Se \code{dat} é um caminho de planilha padrão, com extensão ".xlsm", primeiro será chamada
#'    \code{link{leplanilha}} neste arquivo e então é feita a agregação
#' 
#' @param dat caminho de arquivo ou \code{data.table} contendo o dado a ser agregado. Ver Detalhes
#' @param min.horas percentual indicando o mínimo de horas para considerar a semana valida
#' 
#' @examples
#' 
#' # utilizando o dado exemplo do pacote
#' dat_agregado <- agregasemana(dummydata)
#' 
#' @return \code{data.table} com médias semanais cotendo as variáveis
#'     \describe{
#'         \item{data}{Data e hora inicial da semana}
#'         \item{nmont}{Nível de montante}
#'         \item{quedal}{Queda líquida media}
#'         \item{perda}{Perda média}
#'         \item{vazao}{Vazão defluente tota}
#'         \item{prod}{Produtibilidade média}
#'         \item{nmaq}{NA, não faz sentido em média semanal}
#'         \item{patamar}{NA, não faz sentido em média semanal}
#'         \item{energia}{Energia gerada total da usina}
#'     }
#' 
#' @export

agregasemana <- function(dat, min.horas = .9) UseMethod("agregasemana")

#' @export 
#' 
#' @rdname agregasemana

agregasemana.data.table <- function(dat, min.horas = .9) {

    semana <- datahora <- regvale <- semanafull <- energia <- vazao <- NULL

    if(min.horas > 1) min.horas <- min.horas / 100

    col0 <- colnames(dat)

    datsem <- copy(dat)

    # a leitura da planilha pradrao nao garante que todas as datas estarao la. Desta forma, e melhor
    # gerar um vetor com +- uma semana do qual identificar os comecos de semana para classificacao
    rangedatas <- seq(min(datsem$datahora) - 168 * 3600, max(datsem$datahora) + 168 * 3600, "hour")

    diasem <- wday(rangedatas)
    hora   <- hour(rangedatas)
    inisem <- rangedatas[(diasem == 7) & (hora == 0)]

    datsem[, semana := findInterval(datahora, inisem)]
    datsem <- datsem[semana != 0]
    # esse | serve para evitar que o data.table fique vazio quando so tem uma semana no dado (planilhas teste)
    datsem <- datsem[(semana != length(inisem)) | (length(inisem) == 1)]
    datsem <- datsem[, regvale := complete.cases(datsem)]

    datsem[, semanafull := sum(regvale) > (min.horas * 168), by = semana]

    # funcao alternativa para weighted.mean, que so corta NA de x e nao dos pesos
    wm2 <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)

    datsem <- datsem[semanafull == TRUE, lapply(.SD, wm2, w = energia), .SDcols = c(4:5, 7:9), by = semana]

    datsem[, c("nmaq", "patamar", "energia") := lapply(1:3, function(x) rep(NA, .N))]

    datsem[, datahora := inisem[semana]]
    datsem[, semana := NULL] # tira coluna de indice da semana

    setcolorder(datsem, col0[col0 %in% colnames(datsem)])

    attr(datsem, "cod") <- attr(dat, "cod")
    attr(datsem, "nome") <- attr(dat, "nome")
    attr(datsem, "nmaq") <- attr(dat, "nmaq")
    attr(datsem, "qmax") <- max(attr(dat, "qmax"), datsem[, max(vazao)])

    return(datsem)
}

#' @export 
#' 
#' @rdname agregasemana

agregasemana.character <- function(dat, min.horas = .9) {

    extensao <- sub(".*\\.", "", dat)

    if(extensao == "RDS") {
        dat <- readRDS(dat)
    } else if(grepl("xlsm|x", dat)) {
        dat <- leplanilha(dat)
    } else {
        stop("Formato de arquivo nao permitido")
    }

    dat <- agregasemana(dat, min.horas)

    return(dat)
}