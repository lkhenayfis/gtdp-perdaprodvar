library(data.table)
library(readxl)

devtools::load_all()

# DADO DUMMY PARA EXEMPLOS -------------------------------------------------------------------------

arq <- "D:/ONS/OneDrive - Operador Nacional do Sistema Eletrico/Documentos/TESTE/dummyusi.xlsm"
dummydata <- leplanilha(arq)
oldcod <- attr(dummydata, "cod")

minolds <- sapply(dummydata[, c(4:5, 7:9)], min, na.rm = TRUE)
maxolds <- sapply(dummydata[, c(4:5, 7:9)], max, na.rm = TRUE)

minnews <- c(.006, 1, 50, .1, 20)
maxnews <- c(.007, 200, 60, .5, 22)

renorm <- function(vec, minold, maxold, minnew, maxnew) {
    (vec - minold) / (maxold - minold) * (maxnew - minnew) + minnew
}

dummydata[, c(4:5, 7:9)] <- mapply(renorm,
    dummydata[, c(4:5, 7:9)], minolds, maxolds, minnews, maxnews, SIMPLIFY = FALSE)
attr(dummydata, "cod") <- 999
attr(dummydata, "nome") <- "dummy"
attr(dummydata, "nmaq") <- 0
attr(dummydata, "qmax") <- 210

usethis::use_data(dummydata, overwrite = TRUE)

# BORDAS CURVA COLINA ------------------------------------------------------------------------------

bordasCC <- read_xlsx(
    "D:/ONS/OneDrive - Operador Nacional do Sistema Eletrico/Documentos/TESTE/Extremos Guia Curva Colina.xlsx",
    skip = 2, sheet = 2, col_names = FALSE)

setDT(bordasCC)
bordasCC <- bordasCC[, c(2, 13 + 0:2, 20 + 0:2, 27 + 0:2, 34 + 0:2)]
bordasCC <- bordasCC[!apply(bordasCC, 1, function(v) all(is.na(v)))]

colnames(bordasCC) <- c("usina", paste0(c("quedal", "vazao", "prod"), "_", rep(1:4, each = 3)))

bordasCC <- melt(bordasCC, id.var = "usina", variable.name = "ponto",
    measure.vars = patterns(queda = "^quedal_", vazao = "^vazao_", prod = "^prod_"))

aux <- bordasCC[usina == oldcod]
aux[, usina := 999]
aux[, queda := renorm(queda, minolds[5], maxolds[5], minnews[5], maxnews[5])]
aux[, vazao := renorm(vazao, minolds[2], maxolds[2], minnews[2], maxnews[2])]

bordasCC <- rbind(bordasCC, aux)

setorder(bordasCC, usina)

# HIDR ---------------------------------------------------------------------------------------------

HIDR <- fread("data-raw/Hidr_PMO_Jun2021.csv")

colunas <- colnames(HIDR)
colunas <- colunas[grep("(CodUsina)|(#Maq\\([[:digit:]]\\))|(QEf\\([[:digit:]]\\))", colunas)]
HIDR    <- HIDR[, ..colunas]
colnames(HIDR) <- c("cod", sub("\\(([[:digit:]])\\)", "_\\1", colnames(HIDR))[-1])

HIDR <- melt(HIDR, id = 1, measure.vars = patterns(nmaq = "^#Maq_", vazao = "^QEf_"), variable.name = "grupo")

HIDR <- HIDR[, .(nmaq = sum(nmaq), qmax = sum(nmaq * vazao)), by = cod]

# NOMES --------------------------------------------------------------------------------------------

#root <- "//rio-arq-01/_GTDP/Ciclo 2 - 2010 a 2019/_Resultados Finais por Usina"
#dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE)
#dirs <- file.path(dirs, "Vazao Turbinada")
#
#arqs <- sapply(dirs, list.files, full.names = TRUE, pattern = "^Processo.*xlsm$")
#arqs <- unlist(unname(arqs))
#
#NOMES <- lapply(arqs, function(arq) {
#    usina <- read_xlsx(arq, sheet = "Cadastro", range = "C1:C2", col_names = FALSE, .name_repair = "minimal")
#    nome <- as.character(usina[1, 1])
#    cod  <- as.numeric(usina[2, 1])
#
#    data.table(nome = nome, cod = cod)
#})
#
#NOMES <- rbindlist(NOMES)
#HIDR <- merge(NOMES, HIDR)

# ESCREVE INTERNOS ---------------------------------------------------------------------------------

usethis::use_data(HIDR, bordasCC, internal = TRUE, overwrite = TRUE)