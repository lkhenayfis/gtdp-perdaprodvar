% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classe-gamprod.r
\name{new_gamprod}
\alias{new_gamprod}
\title{Construtor Interno}
\usage{
new_gamprod(dat, mod, fitcall)
}
\arguments{
\item{dat}{dado utilizado para ajuste}

\item{mod}{modelo ajustado}

\item{fitcall}{chamada de \code{fitgam_prod} original}
}
\value{
objeto \code{gamprod} com modelo ajustado e dado original
}
\description{
Função para contrução da saída de \code{fitgam_prod}, não deve ser chamada diretamente
}
