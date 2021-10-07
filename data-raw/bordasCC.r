library(readxl)
library(data.table)

# dado do ciclo passado #########################
bordasCC <- read_xlsx("C:/Users/lucask/Documents/TESTE/ExtremosColina_editado.xlsx", skip = 1, .name_repair = "minimal")
#################################################

setDT(bordasCC)
colnames(bordasCC) <- c("usina", paste0(c("quedal", "vazao", "prod"), "_", rep(1:4, each = 3)))

bordasCC <- melt(bordasCC, id.var = "usina", variable.name = "ponto",
    measure.vars = patterns(queda = "^quedal_", vazao = "^vazao_", prod = "^prod_"))
setorder(bordasCC, usina)

usethis::use_data(bordasCC, internal = TRUE, overwrite = TRUE)