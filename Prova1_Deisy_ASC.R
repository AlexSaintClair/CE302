##########################################################################################################
# Questão 1
##########################################################################################################

# Considere a matriz A dada por:
A<-c(28,7,47,48,45,32,21,43,42,44,8,35,15,19,39,9,28,34,32,50,49,10,2,26,26)
A<-matrix(Matrix_A, nrow=5, ncol=5, byrow=TRUE)
A


B<-c(0,35,27,31,24,26,12,24,36,43,3,19,12,40,31,8,27,17,35,21,30,27,29,8,39)
B<-matrix(Matrix_B, nrow=5, ncol=5, byrow=TRUE)
B

## Seja C = (A · B T )−1
## Seja P = B · (B T · B) · B 

#(a) Considere a matriz de projeção P . A soma de seus autovetores é dada por: -1,1193.
P<- B %*% (t(B) %*% B) %*% t(B)
autovetores_P<-eigen(P)$vectors
soma_autovetores_P<-sum(autovetores_P)
soma_autovetores_P

#(b) A soma dos valores absolutos da diagonal da matriz C é 0,0722
C<-solve(A %*% t(B))
soma_diagonal_C<-sum(abs(diag(C)))
soma_diagonal_C

#(c) A soma de uma matriz triangular inferior para a matriz A é 233.
soma_matriz_triangular_inferior_A<-sum(A[lower.tri(A)])
soma_matriz_triangular_inferior_A

#(d) 
  #Parte 1 - O log10 do valor absoluto do determinante de A é 6,335 
  log10_determinante_A<-log10(abs(det(A)))
  log10_determinante_A
  #Parte2 - O log10 do valor absoluto do determinante de B é 6,7168.
  log10_determinante_B<-log10(abs(det(B)))
  log10_determinante_B
  #Parte3 - O log10 do valor absoluto do determinante da matriz resultante do produto matricial entre A e B é 13,0518.
  log10_determinante_A_B<-log10(abs(det(A %*% B)))
  log10_determinante_A_B

  # (e) O maior elemento da diagonal do inverso da matriz resultante do produto matricial entre A e o transposto de B é 0,026.  
  inverso_A_B<-solve(A %*% t(B))
  maior_elemento_diagonal_inverso_A_B<-max(diag(inverso_A_B))
  maior_elemento_diagonal_inverso_A_B  
  
  
  ##########################################################################################################
  # Questão 2
  ##########################################################################################################
  
  # Leia o arquiivo chocolate.csv e crie um dataframe
  chocolate<-read.csv("chocolate.csv")
  head(chocolate)
  
  #(a) Quantos países que produzem chocolate.
  paises_que_produzem_chocolate<-length(unique(chocolate$origem_cacau))
  paises_que_produzem_chocolate
  #(b1) Verificar a quantidade de ingredientes distintos que existem dentro do campo caracteristicas
  ingredientes_distintos<-length(unique(unlist(strsplit(as.character(chocolate$caracteristicas), ","))))
  ingredientes_distintos
  #(b2) criar no data frame a coluna "Qt_Ingredientes" que contém o numeral antes do separador "-" da coluna ingredientes
  chocolate$Qt_Ingredientes<-as.numeric(sapply(strsplit(as.character(chocolate$ingredientes), "-"), "[[", 1))
  head(chocolate)  
  #(b3) criar no data frame a coluna "Qt_caracteristicas" que conta aquantidade de caracteristicas contida na coluna caracteristicas, sabendo que estão separadas por "," na coluna caracteristicas
  chocolate$Qt_caracteristicas<-sapply(strsplit(as.character(chocolate$caracteristicas), ","), length)
  str(chocolate)
  #Quantos países que produzem chocolate?
  paises_que_produzem_chocolate<-length(unique(chocolate$origem_cacau))
  paises_que_produzem_chocolate
  #Quantos chocolates existem com 4 ingredientes_distintos e 2 caracteristicas?
  chocolates_4_ingredientes_2_caracteristicas<-nrow(chocolate[chocolate$Qt_Ingredientes==4 & chocolate$Qt_caracteristicas==2,])
  chocolates_4_ingredientes_2_caracteristicas  
  # (c) Qual é a frequência absoluta para chocolates que contenham 5 ingredientes distintos? 
  frequencia_absoluta_5_ingredientes<-nrow(chocolate[chocolate$Qt_Ingredientes==5,])
  frequencia_absoluta_5_ingredientes
  # (d) Quais são as 8 caracterististicas mais marcantes dos chocolates?
  caracteristicas_mais_marcantes<-sort(table(unlist(strsplit(as.character(chocolate$caracteristicas), ","))), decreasing=TRUE)[1:8]
  caracteristicas_mais_marcantes  
  # Qual é o número total de características mais marcantes?
  numero_total_caracteristicas_mais_marcantes<-sum(caracteristicas_mais_marcantes)
  numero_total_caracteristicas_mais_marcantes  
  # Quantos chocolates incluem o ingrediente adoçante, dado que o ingrediente adoçante é representado por S*?
  chocolates_com_ingredientes_adoçante<-nrow(chocolate[grep("S*", chocolate$ingredientes),])
  chocolates_com_ingredientes_adoçante

  
  ##########################################################################################################
  # Questão 3
  |##########################################################################################################
  
  # Utilizar os arquivos Art.csv e Art_Moma.csv
  
  Art<-read.csv("Art.csv")
  Art_Moma<-read.csv("Art_Moma.csv")    
  
  # Una as bases dados considerando a coluna "id" e crie um novo dataframe chamado Art_Moma_consolidated
  Art_Moma_consolidated<-merge(Art, Art_Moma, by="artist_unique_id")  
  head(Art_Moma_consolidated)  
  
  # (a) Consolide uma tabela com o nome do artista e a soma das quantidades de "whitney_count_to_year" para cada artista e classifique em ordem decrescente
  Art_Moma_consolidated %>% 
    group_by(artist_name) %>% 
    summarise(soma_whitney_count_to_year = sum(whitney_count_to_year, na.rm = T)) %>% 
    arrange(desc(soma_whitney_count_to_year)) %>% View
  # (b) Qual a quantidade de artistas que possuem a nacionalidade é Swiss ou Mexican ou Japonese?
  quantidade_artistas<-nrow(Art_Moma_consolidated[Art_Moma_consolidated$nationality=="Swiss" | Art_Moma_consolidated$nationality=="Mexican" | Art_Moma_consolidated$nationality=="Japonese",])
  quantidade_artistas    
  # (c) Quais são os artistas que possuem nacionalidade suiça?
  artistas_suicos<-unique(Art_Moma_consolidated[Art_Moma_consolidated$nationality=="Swiss",]$artist_name)
  artistas_suicos  
  