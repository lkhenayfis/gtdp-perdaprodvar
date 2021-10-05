////////////////////////////////////// FUNCOES DE INTERPOLACAO /////////////////////////////////////

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

//' Interpolação Linear
//' 
//' Realiza interpolação linear em uma grade bidimensional de valores
//' 
//' @param yData,zData valores da variável explicativa e dependente, respectivamente, na grade
//' @param yPred valores da variável explicativa a interpolar na grade
//' 
//' @return vetor de valores interpolados
// [[Rcpp::export]]
arma::vec INTERPLIN( arma::vec &yData, arma::vec &zData, arma::vec yPred )
{
    
    // Extrai tamanho do vetor de previsao
    int size = yPred.size();
	
    // Inicializa variavel de saida
	arma::vec out(size);
	
    // Inicializa indice da vazao mais baixa
    int j = 0;

    // Interpola para cada elemento do vetor de previsao
	for(int i = 0; i < size; ++i) {

        // Determina o indice da vazao mais baixa
		while (yData[j + 1] < yPred[i]) j++;

        // Identifica as coordenadas Y dos pontos entre os quais se interpola
		double y1 = yData[j], y2 = yData[j + 1];

        // Identifica as coordenadas Z dos pontos entre os quais se interpola
		double z1 = zData[j], z2 = zData[j + 1];

        // Calcula coeficiente angular
		double a = (z2 - z1) / (y2 - y1);

        // Realiza interpolacao e preenche vetor de saida
		out[i] = z1 + a * (yPred[i] - y1);
		
	}

    // Retorna saida
	return out;
}

//' Interpolação Bilinear
//' 
//' Realiza interpolação bilinear em uma grade tridimensional de valores
//' 
//' @param xData,yData,zData valores das variáveis explicativas (x e y) e dependente (z), na grade
//' @param xPred,yPred valores das variáveis explicativas a interpolar na grade
//' 
//' @return vetor de valores interpolados
// [[Rcpp::export]]
arma::vec INTERPBILIN( arma::vec &xData, arma::vec &yData, arma::mat &zData, 
    arma::vec &xPred, arma::vec &yPred )
{
    
    // Extrai tamanho dos vetores de previsao
    int size = xPred.size();
	
    // Inicializa variavel de saida
	arma::vec out(size);
	
    // Inicializa indice da vazao mais baixa
    int jY = 0;

    // Interpola para cada elemento do vetor de previsao
	for(int i = 0; i < size; ++i) {

        // Determina o indice da vazao mais baixa
		while(yData[jY + 1] < yPred[i]) jY++;

        // Inicializa indice da queda mais baixa
        int jX = 0;

        // Determina infice da queda mais baixa
        while (xData[jX + 1] < xPred[i]) jX++;

        // Identifica as coordenadas X dos pontos entre os quais se interpola
		double x1 = xData[jX], x2 = xData[jX + 1];
        
        // Identifica as coordenadas Y dos pontos entre os quais se interpola
		double y1 = yData[jY], y2 = yData[jY + 1];

        // Identifica as coordenadas Z dos pontos entre os quais se interpola
        double z11 = zData(jX, jY), z12 = zData(jX, jY+1), z21 = zData(jX+1, jY), z22 = zData(jX+1,jY+1);

        // Calcula as primeiras interpolacoes em X
        double f1 = (x2 - xPred[i]) / (x2 - x1) * z11 + (xPred[i] - x1) / (x2 - x1) * z21;
        double f2 = (x2 - xPred[i]) / (x2 - x1) * z12 + (xPred[i] - x1) / (x2 - x1) * z22;

        // Interpola em Y e preenche o vetor de saida
        out[i] = (y2 - yPred[i]) / (y2 - y1) * f1 + (yPred[i] - y1) / (y2 - y1) * f2;
		
	}

    // Retorna saida
	return out;
}