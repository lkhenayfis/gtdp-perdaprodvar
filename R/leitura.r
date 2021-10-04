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

    cod <- read_xlsx(arq, sheet = "Cadastro", range = "C2", col_names = FALSE, .name_repair = "minimal")

    maq   <- setDT(read_xlsx(arq, sheet = "QTurb", n_max = 1, .name_repair = "minimal"))
    n_Maq <- sum(!is.na(maq[1, ])) - 5

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
    pat   <- factor(rend[, 6, drop = TRUE], levels = c("P", "M", "L"), ordered = TRUE)
    energ <- rend[, 4, drop = TRUE]
    rend  <- rend[, 3, drop = TRUE]

    turb <- read_xlsx(arq, sheet = "QTurb", skip = 1, col_types = "numeric",
                        .name_repair = "minimal", n_max = 87649)
    turb <- data.matrix(turb[, 3:(n_Maq + 3)])
    nmaqs <- apply(turb[, 1:n_Maq], 2, function(x) is.na(x) | (x == 0))
    nmaqs <- factor(rowSums(!nmaqs))
    turb <- turb[, n_Maq + 1]

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
    attr(out, "cod") <- as.numeric(cod)

    return(out)
}