# master

## New features

* `optgam_perda` e `optgam_prod` agora fazem uma consistência dos valores de range fornecidos: para
  perda o vetor `range` é encolhido apenas para os valores menores que o número de pontos; para 
  produtibilidade o data.frame resultante das combinações entre os vetores em `range` é encolhido 
  para as linhas nas quais o produto entre dimensões é menor que o número de pontos

### Minor

* `(fit|opt)gam` agora recebem um argumento `gamctrl`, uma lista passada como `(gam|scam).control` 
  para as funções de estimação dos modelos aditivos apropriadas

## Bug fixes

* `optgam_perda` e `optgam_prod` agora possuem um controle de erro na estimação dos modelos. De vez 
  em quando ocorre um erro do próprio `gam` ou `scam` cujas causas ainda não foram identificadas, o
  que torna o `tryCatch` necessário

# perdaprodvar 1.2

## New features

* `leplanilha` mais robusta. Todos os tipos de dado esperados por coluna nas planilhas Excel são 
  agora especificados; o tratamento de linhas completamente vazias foi melhorado e agilizado.
* `agregasemana` agora está adequada para receber históricos horários com datas faltantes. Antes era
  assumido que todas as linhas estariam lá, o que poderia levar a médias semanais mal calculadas ou
  erradas
* Funções para ajuste de GAM agora suportam outras distribuições. Foi incluído em `fitgam_perda` e 
  `fitgam_prod` um argumento `dist` através do qual é possível informar qual distribuição deve ser 
  considerada no ajuste.
* Funções para ajuste de GAM agora suportam bases de splines com restrição de forma. Para perdas é
  possível especificar bases que formam funções convexas ou monotonicamente crescentes, enquanto 
  para produtibilidade é possível especificar uma função côncava.
* `fitgam_perda` agora permite o ajuste de GAM para todo o domínio, isto é, sem utilizar 
  extrapolações externas. Quando utilizada esta opção, é adicionado um ponto próximo a zero para
  balizar o ajuste.

### Minor

* Adiciona atributo `bordas` aos objetos `gamprod`. Inclui estes pontos no plot de `gamprod` e 
  `gridprod`
* A chamada guardada pelas funções `fitgam` agora avaliam os argumentos antes de retornar os 
  objetos. Isso é feito para que se tenha um registro exato da parametrização das chamadas caso seja preciso rodá-las com versões posteriores do pacote e comparar resultados operacionais
* `(fit|opt)gam_prod` agora recebem um vetor de duas posições indicando o tipo de splines e um vetor
  (fit) ou lista (opt) indicando dimensão de base

## Bug fixes

* Corrige o nome das colunas em `bordasCC`. Antes a coluna de queda se chamava `queda`, sem o l no 
  final indicando queda líquida. Em função disso ao juntar o dado de bordas com histórico para 
  ajuste, a coluna `quedal` ficava `NA` nos pontos de borda e eles nao entravam no ajuste.
* Corrige escala de produtibilidade na borda da usina dummy. Antes esse dado ficava com a prod 
  original, sem reescalonamento como é feito no dado dummy, o que introduzia uma inconsistência

# perdaprodvar 1.1.2

## New features

* Adiciona método de plot para `varreduraprod` e `gridprod`

### Minor 

* Melhora doc de `extraigrid` e `optgrid` com mais referências aos métodos pertinentes
* Adiciona testes das novas funcoes de plot
* Adiciona testes de `optgrid.gamprod`
* Robustifica um pouco testes de `optgrid.perda`

## Bug fixes

* Corrige determinação de melhor dimensão de grade para produtibilidade. A versão anterior do código
  confundia a saída de `achafronteira` com número de divisões, quando na verdade era o índice no 
  vetor do número de divisões (issue #12)
* Corrige exemplos de `plot.varreduraperda` que estava mostrando plot de grid de perda normal 
  (issue #10)

# perdaprodvar 1.0

Este pacote contém o conjunto de funções necessárias para aplicação e análise da metodologia para
modelagem de perda e produtibillidade variáveis através de modelos aditivos. Além de funções
dedicadas à extração dos dados e cálculo das médias semanais, são fornecidas funções para ajuste dos
modelos, otimização dos hiperparâmetros dos mesmos, extração de grades e otimização do número de
segmentações necessárias, segundo critérios acordados no GT Produtibillidade Hidrelétrica da CPAMP.
Finalmente, funções para visualização e diagnósticos são inclusas.