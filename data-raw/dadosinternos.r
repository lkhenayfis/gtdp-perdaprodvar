library(data.table)
library(readxl)

# NOMES --------------------------------------------------------------------------------------------

root <- "//rio-arq-01/_GTDP/Ciclo 2 - 2010 a 2019/_Resultados Finais por Usina"
dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE)
dirs <- file.path(dirs, "Vazao Turbinada")

arqs <- sapply(dirs, list.files, full.names = TRUE, pattern = "^Processo.*xlsm$")
arqs <- unlist(unname(arqs))

NOMES <- lapply(arqs, function(arq) {
    usina <- read_xlsx(arq, sheet = "Cadastro", range = "C1:C2", col_names = FALSE, .name_repair = "minimal")
    nome <- as.character(usina[1, 1])
    cod  <- as.numeric(usina[2, 1])

    data.table(nome = nome, cod = cod)
})

NOMES <- rbindlist(NOMES)

# HIDR ---------------------------------------------------------------------------------------------

HIDR <- fread("data-raw/Hidr_PMO_Jun2021.csv")

colunas <- colnames(HIDR)
colunas <- colunas[grep("(CodUsina)|(#Maq\\([[:digit:]]\\))|(QEf\\([[:digit:]]\\))", colunas)]
HIDR    <- HIDR[, ..colunas]
colnames(HIDR) <- c("cod", sub("\\(([[:digit:]])\\)", "_\\1", colnames(HIDR))[-1])

HIDR <- melt(HIDR, id = 1, measure.vars = patterns(nmaq = "^#Maq_", vazao = "^QEf_"), variable.name = "grupo")

HIDR <- HIDR[, .(nmaq = sum(nmaq), qmax = sum(nmaq * vazao)), by = cod]

# COMBINA E ESCREVE --------------------------------------------------------------------------------

HIDR <- merge(NOMES, HIDR)

#usethis::use_data(HIDR, internal = TRUE, overwrite = TRUE)