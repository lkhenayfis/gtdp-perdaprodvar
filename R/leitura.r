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

    options(readxl.show_progress = FALSE)

    usina <- read_xlsx(arq, sheet = "Cadastro", range = "C1:C2", col_names = FALSE, .name_repair = "minimal")
    nome <- as.character(usina[1, 1])
    cod  <- as.numeric(usina[2, 1])

    maqs <- setDT(read_xlsx(arq, sheet = "Cadastro", range = "E15:G300", .name_repair = "minimal"))
    maqs <- maqs[complete.cases(maqs)]
    qmax  <- maqs[, sum(Qef)]
    nmaq <- maqs[, .N]

    info <- read_xlsx(arq, sheet = "Abertura(1)", range = "G13:G18", .name_repair = "minimal")
    info <- data.matrix(info)
    G   <- as.numeric(ifelse(!is.na(info[4, ]), info[4, ], info[1, ]))
    rho <- as.numeric(ifelse(!is.na(info[5, ]), info[5, ], info[2, ]))

    classes <- c("date", "numeric",  "numeric", "numeric", "text", "text")
    rend <- read_xlsx(arq, sheet = "Rendimento Usina (hora)", .name_repair = "minimal",
                     range = "A2:F87650", col_types = classes)
    rend <- setDT(rend)
    rend <- rend[apply(rend, 1, function(r) !all(is.na(r)))]
    colnames(rend) <- c("data", "hora", "rend", "energia", "diasem", "patamar")
    rend[, datahora := as.POSIXct(paste0(data, " ", hora - 1, ":00"), format = "%Y-%m-%d %H:%M", "GMT")]
    rend[, prod := rend * rho * G * 1e-6]
    rend <- rend[, list(datahora, patamar, energia, prod)]
    setkey(rend, datahora)

    turb <- read_xlsx(arq, sheet = "QTurb", skip = 1, .name_repair = "minimal",
        range = paste0("A2:", LETTERS[2 + nmaq + 1], "87650"))
    turb <- setDT(turb)
    turb <- turb[apply(turb, 1, function(r) !all(is.na(r)))]
    colnames(turb) <- c("data", "hora", paste0("vaz_maq", seq(nmaq)), "vazao")
    turb[, datahora := as.POSIXct(paste0(data, " ", hora - 1, ":00"), format = "%Y-%m-%d %H:%M", "GMT")]
    nmaqs <- turb[, lapply(.SD, function(x) is.na(x) | (x == 0)), .SDcols = names(turb) %like% "vaz_maq"]
    turb[, nmaq := rowSums(!nmaqs)]
    turb <- turb[, list(datahora, vazao, nmaq)]
    setkey(turb, datahora)

    quedab <- read_xlsx(arq, sheet = "Dados", range = "A2:E87650", .name_repair = "minimal")
    setDT(quedab)
    quedab <- quedab[apply(quedab, 1, function(r) !all(is.na(r)))]
    colnames(quedab) <- c("data", "hora", "XXX", "nmont", "njus")
    quedab[, datahora := as.POSIXct(paste0(data, " ", hora - 1, ":00"), format = "%Y-%m-%d %H:%M", "GMT")]
    quedab[, quedab := nmont - njus]
    quedab <- quedab[, list(datahora, nmont, quedab)]
    setkey(quedab, datahora)

    perda <- read_xlsx(arq, sheet = "Perda Usina (hora)", .name_repair = "minimal",
                     range = "A2:C87650")
    setDT(perda)
    perda <- perda[apply(perda, 1, function(r) !all(is.na(r)))]
    colnames(perda) <- c("data", "hora", "perda")
    perda[, datahora := as.POSIXct(paste0(data, " ", hora - 1, ":00"), format = "%Y-%m-%d %H:%M", "GMT")]
    perda <- perda[, list(datahora, perda)]
    setkey(perda, datahora)

    out <- Reduce(function(d1, d2) merge(d1, d2, all = TRUE), list(rend, turb, quedab, perda))
    out[, quedal := quedab - perda]
    out[, quedab := NULL]

    attr(out, "cod")  <- cod
    attr(out, "nome") <- nome
    attr(out, "nmaq") <- nmaq
    attr(out, "qmax") <- qmax

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

    diasem <- wday(datsem$datahora)
    hora   <- hour(datsem$datahora)
    inisem <- datsem$datahora[(diasem == 7) & (hora == 0)]

    datsem[, semana := findInterval(datahora, inisem)]
    datsem <- datsem[semana != 0]
    # esse | serve para evitar que o data.table fique vazio quando so tem uma semana no dado (planilhas teste)
    datsem <- datsem[(semana != length(inisem)) | (length(inisem) == 1)]
    datsem <- datsem[, regvale := complete.cases(datsem)]

    datsem[, semanafull := mean(regvale) > min.horas, by = semana]

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
    } else if(extensao == "xlsm") {
        dat <- leplanilha(dat)
    } else {
        stop("Formato de arquivo nao permitido")
    }

    dat <- agregasemana(dat)

    return(dat)
}