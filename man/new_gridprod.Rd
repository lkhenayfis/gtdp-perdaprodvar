% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classe-gridprod.r
\name{new_gridprod}
\alias{new_gridprod}
\title{Construtor Interno}
\usage{
new_gridprod(prod, quedal, vazao, fit)
}
\arguments{
\item{prod}{prod amostrada do modelo}

\item{quedal}{quedas líquidas correspondentes às segmentações da grade}

\item{vazao}{vazões correspondentes às segmentações da grade}

\item{fit}{modelo ajustado aos dados}
}
\value{
objeto \code{gridprod} com a grade extraída e modelo original
}
\description{
Função para contrução da saída de \code{extraigrid.gamprod}, não deve ser chamada diretamente
}
