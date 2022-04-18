# fixoptgrid

# New features

* Adiciona método de plot para `varreduraprod` e `gridprod`

## Big fixes

* Corrige determinação de melhor dimensão de grade para produtibilidade. A versão anterior do código
  confundia a saída de `achafronteira` com número de divisões, quando na verdade era o índice no 
  vetor do número de divisões.
* Corrige exemplos de `plot.varreduraperda` que estava mostrando plot de grid de perda normal

# perdaprodvar 1.0

Este pacote contém o conjunto de funções necessárias para aplicação e análise da metodologia para
modelagem de perda e produtibillidade variáveis através de modelos aditivos. Além de funções
dedicadas à extração dos dados e cálculo das médias semanais, são fornecidas funções para ajuste dos
modelos, otimização dos hiperparâmetros dos mesmos, extração de grades e otimização do número de
segmentações necessárias, segundo critérios acordados no GT Produtibillidade Hidrelétrica da CPAMP.
Finalmente, funções para visualização e diagnósticos são inclusas.