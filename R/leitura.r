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
#'         \item{nmont}{Nível de montante}
#'         \item{quedal}{Queda líquida media}
#'         \item{perda}{Perda média}
#'         \item{vazao}{Vazão defluente tota}
#'         \item{prod}{Produtibilidade média}
#'         \item{nmaq}{Número de máquinas em operação}
#'         \item{patamar}{Patamar de carga}
#'         \item{energia}{Energia gerada total}
#'     }
#' 
#' @importFrom readxl read_xlsx
#' 
#' @export

leplanilha <- function(arq) {

    options(readxl.show_progress = FALSE)

    usina <- read_xlsx(arq, sheet = "Cadastro", range = "C1:C2", col_names = FALSE, .name_repair = "minimal")
    nome <- as.character(usina[1, 1])
    cod  <- as.numeric(usina[2, 1])

    maqs <- setDT(read_xlsx(arq, sheet = "Cadastro", range = "E15:G300", .name_repair = "minimal"))
    maqs <- maqs[complete.cases(maqs)]
    qmax <- maqs[, sum(Qef)]
    nmaq <- maqs[, .N]

    info <- read_xlsx(arq, sheet = "Abertura(1)", range = "G13:G18", .name_repair = "minimal")
    info <- data.matrix(info)
    G   <- as.numeric(ifelse(!is.na(info[4, ]), info[4, ], info[1, ]))
    rho <- as.numeric(ifelse(!is.na(info[5, ]), info[5, ], info[2, ]))

    classes <- c("date", "numeric",  "numeric", "numeric", "text", "text")
    rend <- read_xlsx(arq, sheet = "Rendimento Usina (hora)", .name_repair = "minimal", 
                     range = "A2:F87650", col_types = classes)
    rend  <- setDT(rend)
    datas <- paste0(rend$Data, " ", formatC(rend$Hora - 1, width = 2, flag = "0", format = "d"), ":00")
    datas <- as.POSIXct(datas, "GMT", format = "%Y-%m-%d %H:%M")
    pat   <- factor(rend$Patamar, levels = c("P", "M", "L"), ordered = TRUE)
    energ <- rend[[4]]
    rend  <- rend[[3]]

    turb <- read_xlsx(arq, sheet = "QTurb", skip = 1, col_types = "numeric",
                        .name_repair = "minimal", n_max = 87649)
    turb <- data.matrix(turb[, 3:(nmaq + 3)])
    nmaqs <- apply(turb[, 1:nmaq], 2, function(x) is.na(x) | (x == 0))
    nmaqs <- factor(rowSums(!nmaqs))
    turb <- turb[, nmaq + 1]

    quedab <- read_xlsx(arq, sheet = "Dados", range = "D2:E87650", col_types = "numeric", .name_repair = "minimal")
    quedab <- data.matrix(quedab)
    nmont  <- quedab[, 1]
    quedab <- apply(quedab, 1, function(x) x[1] - x[2])

    perda <- read_xlsx(arq, sheet = "Perda Usina (hora)", .name_repair = "minimal", 
                     range = "C2:C87650", col_types = "numeric")
    perda <- c(data.matrix(perda))

    quedal <- quedab - perda

    out <- data.table("datahora" = datas, "nmont" = nmont, "quedal" = quedal, "perda" = perda,
        "vazao" = turb, "prod" = rend * G * rho * 1e-6, "nmaq" = nmaqs,
        "patamar" = pat, "energia" = energ)
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
#'    \cdde{link{leplanilha}} neste arquivo e então é feita a agregação
#' 
#' @param dat caminho de arquivo ou \code{data.table} contendo o dado a ser agregado. Ver Detalhes
#' @param min.horas percentual indicando o mínimo de horas para considerar a semana valida
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
#'         \item{energia}{NA, não faz sentido em média semanal}
#'     }
#' 
#' @export

agregasemana <- function(dat, min.horas = .9) UseMethod("agregasemana")

#' @export 
#' 
#' @rdname agregasemana

agregasemana.data.table <- function(dat, min.horas = .9) {

    if(min.horas > 1) min.horas <- min.horas / 100

    datsem <- copy(dat)

    diasem <- lubridate::wday(datsem$datahora)
    hora   <- lubridate::hour(datsem$datahora)
    inisem <- datsem$datahora[(diasem == 7) & (hora == 0)]

    datsem[, semana := findInterval(datahora, inisem)]
    datsem <- datsem[semana != 0]
    datsem <- datsem[semana != length(inisem)]
    datsem <- datsem[, regvale := complete.cases(datsem)]

    datsem[, semanafull := mean(regvale) > min.horas, by = semana]

    # funcao alternativa para weighted.mean, que so corta NA de x e nao dos pesos
    wm2 <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)

    datsem <- datsem[semanafull == TRUE, lapply(.SD, wm2, w = energia), .SDcols = 2:9, by = semana]

    datsem[, c("nmaq", "patamar", "energia") := lapply(1:3, function(x) rep(NA, .N))]
    
    datsem[, data := inisem[semana]]

    setcolorder(datsem, c("data", colnames(datsem)[-c(1, 10)]))

    datsem <- datsem[, -10]

    attr(datsem, "cod") <- attr(dat, "cod")
    attr(datsem, "nome") <- attr(dat, "nome")
    attr(datsem, "nmaq") <- attr(dat, "nmaq")
    attr(datsem, "qmax") <- attr(dat, "qmax")

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