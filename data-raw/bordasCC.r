library(readxl)
library(data.table)

# dado do ciclo passado
bordasCC <- read_xlsx("C:/Users/lucask/Documents/TESTE/ExtremosColina_editado.xlsx", skip = 1, .name_repair = "minimal")
setDT(bordasCC)
colnames(bordasCC) <- c("usina", rep(c("quedal", "vazao", "prod"), 4))

usethis::use_data(bordasCC, internal = TRUE, overwrite = TRUE)