########################################## CLASSE GAMPROD ##########################################

#' Objeto \code{gamprod}
#' 
#' Construtor e métodos da classe \code{gamprod}
#' 
#' @name gamprod
NULL

#' Construtor Interno
#' 
#' Função para contrução da saída de \code{fitgam_perda}, não deve ser chamada diretamente
#' 
#' @rdname gamprod

new_gamprod <- function(dat, mod, atributos, args) {

    dat <- copy(dat)[, .SD, .SDcols = c("quedal", "vazao", "perda")]

    out <- list(model = mod, dat = dat)
    attr(out, "gamargs") <- args
    for(at in names(atributos)) attr(out, at) <- atributos[[at]]
    class(out) <- "gamprod"

    return(out)
}

#' @export

print.gamprod <- function(x, ...) {
    cat("\n## GAM\n")
    print(x$model)
}
