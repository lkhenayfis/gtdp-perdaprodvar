root <- "//rio-arq-01/_GTDP/Ciclo 2 - 2010 a 2019/_Resultados Finais por Usina"
dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE)
dirs <- file.path(dirs, "Vazao Turbinada")

arqs <- sapply(dirs, list.files, full.names = TRUE, pattern = "^Processo.*xlsm$")
arqs <- unlist(unname(arqs))

clst <- parallel::makeCluster(7, "PSOCK")
parallel::clusterExport(clst, c("arqs"))
parallel::clusterEvalQ(clst, devtools::load_all("C:/Users/lucask/Documents/GTDP/perdaprodvar"))

parallel::parLapply(clst, arqs, function(arq) {

    dado <- leplanilha(arq)

    outdir <- sub("Vazao.*", "Produtibilidade Variável", arq)
    usina  <- gsub("(.*iterativo - )|(\\.xls[xm])", "", arq)
    if(!dir.exists(outdir)) dir.create(outdir)

   # data.table::fwrite(dado, file.path(outdir, paste0(usina, ".csv")), quote = FALSE)
    saveRDS(dado, file.path(outdir, paste0(usina, ".RDS")))
})

parallel::stopCluster(clst)

# Checa quem deu erro ------------------------------------------------------------------------------

for(arq in arqs) {
    outdir <- sub("Vazao.*", "Produtibilidade Variável", arq)
    usina  <- gsub("(.*iterativo - )|(\\.xls[xm])", "", arq)

    if(!file.exists(file.path(outdir, paste0(usina, ".RDS")))) print(usina)
}