devtools::load_all()

# extrai historicos horarios -----------------------------------------------------------------------
root <- "//rio-arq-01/_GTDP/Ciclo 2 - 2010 a 2019/_Resultados Finais por Usina"
dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE)
dirs <- file.path(dirs, "Vazao Turbinada")

arqs <- sapply(dirs, list.files, full.names = TRUE, pattern = "^Processo.*xlsm$")
arqs <- unlist(unname(arqs))

for(arq in arqs) {

    dado <- leplanilha(arq)

    outdir <- sub("Vazao.*", "Produtibilidade Variável", arq)
    if(!dir.exists(outdir)) dir.create(outdir)

    data.table::fwrite(dado, file.path(outdir, "dadohorario.csv"), quote = FALSE)
    saveRDS(dado, file.path(outdir, "dadohorario.RDS"))
}
