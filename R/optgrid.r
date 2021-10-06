################################## OTIMIZACAO DO TAMANHO DE GRADE ##################################

#' Otimização De Segmentações De Grade
#' 
#' Otimiza o número de divisões de grade para atendimento de um critério mínimo
#' 
#' @param fit ajuste do dado de perda ou produtibilidade
#' @param R razão mínima entre erro do ajuste e da grade para aceitação
#' @param ... demais parâmetros específicos de grade de perda ou produtibilidade. Ver métodos 
#'     Exemplos
#' @param full.output booleano, se verdadeiro retorna objeto de varredura
#' 
#' @examples
#' 
#' # otimizando grade de perdas -----------------------------------------------
#' 
#' dat <- agregasemana(dummydata)
#' ajuste <- fitgam_perda(dat, ns.vazao = 5)
#' 
#' # roda otimizacao do numero de segmentacoes sem retornar varredura
#' optgrade <- optgrid(ajuste, range.vazao = 10:60)
#' 
#' # assim como ajustes de curvas, grades tambem possuem um metodo de plot
#' plot(optgrade)
#' 
#' # e possivel rodar a otimizacao retornando a varredura de numero de segmentacoes e plota-la
#' optgrade <- optgrid(ajuste, range.vazao = 10:60, full.output = TRUE)
#' plot(optgrade[[2]])
#' 
#' @return se \code{full.output = FALSE}, a grade parsimoniosa que atenda a razão mínima \code{R}.
#'     Se \code{full.output = TRUE}, uma lista com a grade no primeiro elemento e a varredura no 
#'     segundo
#' 
#' @export

optgrid <- function(fit, R = 1.01, ..., full.output = FALSE) {
    UseMethod("optgrid")
}

#' @param range.vazao vetor de inteiros indicando números de segmentações a serem testados
#' 
#' @export
#' 
#' @rdname optgrid

optgrid.gamperda <- function(fit, R = 1.01, range.vazao = 5:50, ..., full.output = FALSE) {

    errofit <- sum(residuals(fit)^2)

    grades <- lapply(range.vazao, function(segs) extraigrid(fit, segs))
    erros  <- lapply(grades, function(grade) sum(residuals(grade)^2))

    razaoerros <- matrix(unlist(erros) / errofit, length(range.vazao))

    persis <- achapersistencia(razaoerros <= R)

    if(!any(persis)) {
        cat("Nenhum numero de segmentacoes atende o criterio -- aumentando range")

        range.vazao <- seq(max(range.vazao), max(range.vazao) + diff(range(range.vazao)))
        optgrid(fit, R, range.vazao = range.vazao, full.output = full.output)
    } else {
        front  <- achafronteira(persis)

        out <- grades[[front[1, X]]]

        if(full.output) {
            varredura <- list(razao = razaoerros, range = range.vazao, R = R, front = front)
            class(varredura) <- "varreduraperda"

            out <- list(optgrid = out, varredura = varredura)
        }

        return(out)
    }
}

# GENERICA PARA EXTRACAO DE GRADE ------------------------------------------------------------------

#' Extrator De Grades
#' 
#' Extrai grades de perda e produtibilidade a parti dos ajustes realizados
#' 
#' @param fit objeto \code{gam(perda)|(prod)} gerado pelas funções de \code{fit} ou \code{opt} 
#'     apropriadas
#' @param dim dimensão da grade -- vetor de uma posição para perda e duas para produtibilidade 
#'     (divisoes de queda liquida, divisoes de vazao)
#' 
#' @return object \code{grid(perda)|(prod)}, dependendo da classe do argumento \code{fit}. Em ambos
#'     os casos será uma lista com a grade na primeira posição e o modelo original na segunda

extraigrid <- function(fit, dim, ...) UseMethod("extraigrid")

# HELPERS ------------------------------------------------------------------------------------------

#' Identifica Região De Convergência
#' 
#' \code{achapersistencia} identifica dentro de um array de razão de erros a região onde já ocorre
#' convergência
#' 
#' Esta é uma função auxiliar que não deve ser utilizada senão internamente pelo código. É 
#' implementada no contexto de otimização do número de segmentações para geração final do grid.
#' 
#' @param mat matriz de verdadeiros e falsos resultante da comparação da matriz de razão de erros 
#'     com um parâmetro de razão mínima. Colunas representam de segmentações de vazão turbinada e
#'     linhas de queda líquida
#' 
#' @return matriz booleana; TRUE na região de convergência e FALSE do contrário

achapersistencia <- function(mat) {

    # Extrai numero de colunas e linhas
    R <- nrow(mat)
    C <- ncol(mat)

    # Conjunto de coordenadas a testar
    coords <- data.table(X = rep(1:R, each = C), Y = 1:C, testar = TRUE)

    # Inicializa saida
    out <- matrix(FALSE, R, C, dimnames = dimnames(mat))

    # Testa a regiao de aceitacao para cada coordenada
    for(i in seq(nrow(coords))) {

        # Checa se essa coordenada deve ser testada
        if(!coords[i, testar]) next

        # Coordenadas a testar
        xi <- coords[i, X]
        yi <- coords[i, Y]

        # Subset da matriz
        submat <- mat[xi:R, yi:C]

        # Testa se esse subset pertence a regiao de aceitacao
        if(all(submat)) {

            # Caso positivo, marca a regiao na matriz de saida e reduz as buscas futuras
            out[xi:R, yi:C] <- TRUE
            coords[(X >= xi) & (Y >= yi), testar := FALSE]
        } else {

            # Identifica a coordenada do FALSE mais interno
            maisint <- tail(which(!submat), 1)
            maisint <- c((maisint - 1) %%  (R - xi + 1) + xi,
                         (maisint - 1) %/% (R - xi + 1) + yi)

            # Corta as buscas que incluem esse ponto
            coords[(X <= maisint[1]) & (Y <= maisint[2]), testar := FALSE]
        }
    }

    # Retorna saida
    return(out)
}

#' Identifica Fronteira Da Região De Convergência
#' 
#' \code{achafronteira} identifica os pontos mais externos da região de convergência
#' 
#' Esta é uma função auxiliar que não deve ser utilizada senão internamente pelo código. É 
#' implementada no contexto de otimização do número de segmentações para geração final do grid.
#' 
#' @param mat matriz de verdadeiros e falsos representando a região de convergência
#' 
#' @return data.table contendo as coordenadas na matriz dos pontos de fronteira

achafronteira <- function(mat) {

    # Olha pelos x
    dx <- apply(mat, 1, function(x) ifelse(length(x) > 1, which(x)[1], NA))
    dx <- data.table(X = seq(nrow(mat)), Y = dx)

    # Olha pelos y
    dy <- apply(mat, 2, function(y) ifelse(length(y) > 1, which(y)[1], NA))
    dy <- data.table(X = dy, Y = seq(ncol(mat)))

    # Combina os dois e remove possiveis duplicatas
    front <- rbind(dx, dy)
    front <- front[complete.cases(front)]
    front <- front[!duplicated(paste0(X, Y))]

    # Retorna
    return(front)
}