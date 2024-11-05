#Prova Deisy 2023
######################################################################################################################
# Questão 01
######################################################################################################################
# Define as matrizes A e B
A <- matrix(c(28, 32, 8,9,49,7,21,35,28,10,47,43,15,34,2,48,42,19,32,26,45,44,39,50,26), nrow = 5, ncol = 5, byrow = TRUE)
B <- matrix(c(0, 26,3,8,30,35,12,19,27,27,27,24,12,17,29,31,36,40,35,8,24,43,31,21,39), nrow = 5, ncol = 5, byrow = TRUE)

## print(A)
## print(B)

# Calcula o transposto de B
Bt <- t(B)
## print(Bt)

# Calcula C
C <- solve(A %*% Bt)

# Calcula P
P <- B %*% (Bt %*% B) %*% Bt

# (a) Soma dos autovalores de P
eigen_P <- eigen(P)
soma_autovalores_P <- sum(eigen_P$values)

# (b) Soma dos valores absolutos da diagonal de C
soma_diag_C <- sum(abs(diag(C)))

# (c) Soma de uma matriz triangular inferior para A (requer interpretação)
soma_triang_inf_A <- sum(A[lower.tri(A, diag = TRUE)])

# (d) Log10 do determinante de A, B e A*B 

log10_det_A <- log10(abs(det(A)))
log10_det_B <- log10(abs(det(B)))
log10_det_AxB <- log10(abs(det(A %*% B)))

# (e) Maior elemento da diagonal do inverso de A*Bt (requer que A*Bt seja quadrada)
#Matriz A*Bt
Inv_Matriz_AxBt <- solve(A %*% Bt)
#Menor elemento da diagonal de Inv_Matriz_AxBt
maior_diag_Inv_Matriz_AxBt <- max(diag(Inv_Matriz_AxBt))

# Imprime os resultados
print(paste("(a) Soma dos autovalores de P:", soma_autovalores_P))
print(paste("(b) Soma dos valores absolutos da diagonal de C:", soma_diag_C))
print(paste("(c) Soma da matriz triangular inferior de A:", soma_triang_inf_A))
print(paste("(d) Log10 do determinante de A, B e A*B :", {log10_det_A}, {log10_det_B}, {log10_det_AxB}))
print(paste("(e) Maior elemento da diagonal do inverso de A*Bt:", maior_diag_Inv_Matriz_AxBt))


######################################################################################################################
# Questão 02
######################################################################################################################

# Instala e carrega pacotes necessários (caso ainda não estejam instalados)
if(!require("readr")) install.packages("readr")
library(readr)
if(!require("dplyr")) install.packages("dplyr")
library(dplyr)


# Lê o arquivo chocolate.csv.gz
chocolate <- read_csv("chocolate.csv")

# (a) Existem 2443 países que produzem chocolate.
num_paises <- n_distinct(chocolate$origem_cacau)
alternativa_a <- num_paises == 2443

# (b) Existem 104 chocolates com 4 ingredientes que são descritos por 2 características.
# Supondo que 'ingredientes' indica o número de ingredientes e 'caracteristicas' o número de características
# Você pode precisar ajustar essa parte dependendo do formato exato das colunas no seu arquivo
alternativa_b <- nrow(chocolate %>% filter(ingredientes == 4, caracteristicas == 2)) == 104

# (c) A frequência absoluta para chocolates que contenham 5 ingredientes é 750.
alternativa_c <- sum(chocolate$ingredientes == 5) == 750

# (d) As 8 características mais marcantes dos chocolates são sweet, nutty, cocoa, roasty, creamy, earthy, sandy e fatty e juntas correspondem a 1663 descrições dos chocolates.
# Este item requer mais detalhes sobre como as 'características' são representadas no dataset.
# Assumindo que 'caracteristicas' é uma coluna com strings separadas por vírgula:

top_caracteristicas <- chocolate %>%
  separate_rows(caracteristicas, sep = ",") %>% # Separa as características em múltiplas linhas
  group_by(caracteristicas) %>% # Agrupa por característica
  summarise(n = n()) %>% # Conta a frequência de cada característica
  arrange(desc(n)) %>% # Ordena por frequência decrescente
  slice_head(n = 8) # Seleciona as 8 características mais frequentes

caracteristicas_mencionadas <- c("sweet", "nutty", "cocoa", "roasty", "creamy", "earthy", "sandy", "fatty")
soma_top_caracteristicas <- sum(top_caracteristicas$n[top_caracteristicas$caracteristicas %in% caracteristicas_mencionadas])

alternativa_d <- all(top_caracteristicas$caracteristicas %in% caracteristicas_mencionadas) && (soma_top_caracteristicas == 1663)


# (e) Existem 81 chocolates que incluem o ingrediente 'Adoçante' em sua composição.
# Assumindo que a coluna 'ingredientes' contém uma string com os ingredientes separados por algum caractere:
alternativa_e <- sum(grepl("S\\*", chocolate$ingredientes)) == 81  # Usando S* como indicador de adoçante, conforme o enunciado


# Imprime os resultados
print(paste("Alternativa (a) é incorreta:", !alternativa_a))
print(paste("Alternativa (b) é incorreta:", !alternativa_b))
print(paste("Alternativa (c) é incorreta:", !alternativa_c))
print(paste("Alternativa (d) é incorreta:", !alternativa_d))
print(paste("Alternativa (e) é incorreta:", !alternativa_e))


######################################################################################################################
# Questão 03
######################################################################################################################

# Instala e carrega pacotes necessários (caso ainda não estejam instalados)
if(!require("readr")) install.packages("readr")
library(readr)
if(!require("dplyr")) install.packages("dplyr")
library(dplyr)

# Lê os arquivos Art.csv.gz e Art_Moma.csv.gz
art <- read_csv("Art.csv")
art_moma <- read_csv("Art_Moma.csv")

# Remove artistas sem nacionalidade e/ou sem nome em 'art'
art <- art %>% filter(!is.na(artist_nationality), !is.na(artist_name))

# (a) Os 3 artistas com mais exposições no The Whitney classificados em ordem decrescente de exposições são: Edward Hopper, Georgia O’Keeffe e Stuart Davis.
# ... (código anterior para carregar pacotes e ler os arquivos)

# Realiza o left join dos dataframes 'art' e 'art_moma' pelo 'artist_unique_id'
art_combined <- left_join(art, art_moma, by = "artist_unique_id")

# (a) Os 3 artistas com mais exposições no The Whitney...
top_whitney_artists <- art_combined %>%  # Usa o dataframe combinado aqui
  group_by(artist_name) %>%
  summarise(whitney_count = sum(whitney_count_to_year, na.rm = TRUE)) %>% # Adiciona na.rm = TRUE para lidar com NAs
  arrange(desc(whitney_count)) %>%
  slice_head(n = 3)

artistas_esperados <- c("Edward Hopper", "Georgia O’Keeffe", "Stuart Davis")
alternativa_a <- all(top_whitney_artists$artist_name == artistas_esperados)

# ... (resto do código, usando art_combined quando necessário)s)

# (b) Do total de artistas, 152 são Swiss, Mexican ou Japanese.
num_artistas_nacionalidades <- art %>% filter(artist_nationality %in% c("Swiss", "Mexican", "Japanese")) %>% nrow()
alternativa_b <- num_artistas_nacionalidades == 152


# (c) Apenas 6 artistas com a nacionalidade Swiss tiveram entre 0 e 1 exposições no The Whitney.
num_artistas_suicos_poucas_exposicoes <- art_combined %>%
  filter(artist_nationality == "Swiss", whitney_count_to_year <= 1) %>%
  nrow()

alternativa_c <- num_artistas_suicos_poucas_exposicoes == 6

# (d) A diferença entre a média de páginas para artistas Brancos e Não Brancos no ano de 2007 é -0.24.
# Assumindo que "páginas" se refere a 'space_ratio_per_page_total' e que 'artist_race_nwi' indica raça não branca (1 = sim, 0 = não).
# Você precisará juntar os dataframes 'art' e 'art_moma' se 'space_ratio_per_page_total' estiver em 'art_moma'.

art_combined <- left_join(art, art_moma, by = "artist_unique_id")
############################## Solução 1 ####################################### 
media_paginas_brancos_2007 <- art_combined %>%
  filter(year == 2007, artist_race_nwi == 0) %>%
  summarise(media = mean(space_ratio_per_page_total, na.rm = TRUE)) %>%
  pull(media)
print(media_paginas_brancos_2007)


media_paginas_nao_brancos_2007 <- art_combined %>%
  filter(year == 2007, artist_race_nwi == 1) %>%
  summarise(media = mean(space_ratio_per_page_total, na.rm = TRUE)) %>%
  pull(media)

diferenca_medias <- media_paginas_brancos_2007 - media_paginas_nao_brancos_2007
alternativa_d <- abs(diferenca_medias - (-0.24)) < 1e-6 # Usando uma tolerância pequena para comparação de floats
print(diferenca_medias)
############################## Solução 2 #######################################

media_paginas_brancos_2007 <- art_combined %>%
  filter(year == 2007, artist_race_nwi == 0) %>%
  summarise(media = mean(space_ratio_per_page_total, na.rm = TRUE))

media_paginas_nao_brancos_2007 <- art_combined %>%
  filter(year == 2007, artist_race_nwi == 1) %>%
  summarise(media = mean(space_ratio_per_page_total, na.rm = TRUE))


# Verifica se as médias foram calculadas (não são NaN) antes de calcular a diferença
if (!is.nan(media_paginas_brancos_2007$media) && !is.nan(media_paginas_nao_brancos_2007$media)) {
  diferenca_medias <- media_paginas_brancos_2007$media - media_paginas_nao_brancos_2007$media
  alternativa_d <- abs(diferenca_medias - (-0.24)) < 1e-6
} else {
  # Se alguma das médias for NaN, a alternativa é considerada falsa
  alternativa_d <- FALSE
}



# (e) Dos artistas que expuseram no The Whitney, apenas 164 aparecem nos livros ‘Gardner’ e ‘Janson’.

artistas_whitney <- art_combined %>% filter(whitney_count_to_year > 0) %>% distinct(artist_name)
artistas_livros <- art_combined %>% filter(book %in% c("Gardner", "Janson")) %>% distinct(artist_name)
artistas_em_ambos <- intersect(artistas_whitney$artist_name, artistas_livros$artist_name)

alternativa_e <- length(artistas_em_ambos) == 164



# Imprime os resultados
print(paste("Alternativa (a) é correta:", alternativa_a))
print(paste("Alternativa (b) é correta:", alternativa_b))
print(paste("Alternativa (c) é correta:", alternativa_c))
print(paste("Alternativa (d) é correta:", alternativa_d))
print(paste("Alternativa (e) é correta:", alternativa_e))


######################################################################################################################
# Questão 04
######################################################################################################################

# Instala e carrega pacotes necessários
if (!require("readr")) install.packages("readr")
library(readr)
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)
if (!require("tidyr")) install.packages("tidyr")
library(tidyr)
if (!require("countrycode")) install.packages("countrycode")
library(countrycode)


# Lê os arquivos, tratando erros de leitura
tryCatch(
  {
    refugiados_pais <- read_csv("refugiados_pais.csv")
    refugiados <- read_csv("refugiados.csv")
  },
  error = function(e) {
    message("Erro ao ler os arquivos. Verifique se os arquivos existem e estão no formato correto.")
    stop(e) # Para a execução em caso de erro de leitura
  }
)


# Remove observações incompletas em ambos os dataframes
refugiados_pais <- refugiados_pais %>% na.omit()
refugiados <- refugiados %>% na.omit()

head(refugiados_pais)
head(refugiados)

# Junta os dataframes pelo país de origem e destino
refugiados_completo <- left_join(refugiados, refugiados_pais, by = c("origem" = "pais", "destino" = "pais"))


# (a) Matriz de migração intercontinental do ano de 2006

# Adiciona a coluna com o continente de origem e destino. Lidando com erros.
refugiados_completo <- refugiados_completo %>%
  mutate(
    continente_origem = countrycode(origem, "country.name", "continent"),
    continente_destino = countrycode(destino, "country.name", "continent")
  )

matriz_migracao_2006 <- refugiados_completo %>%
  filter(ano == 2006) %>%
  group_by(continente_origem, continente_destino) %>%
  summarise(total = n()) %>%
  pivot_wider(names_from = continente_destino, values_from = total, values_fill = 0)


alternativa_a <- all(
  matriz_migracao_2006$Africa == 250758,
  matriz_migracao_2006$Americas == 12627,
  matriz_migracao_2006$Asia == 45981,
  matriz_migracao_2006$Europe == 75250,
  matriz_migracao_2006$Oceania == 7037,
  matriz_migracao_2006$`Americas` == 0, # Corrigido: valores da matriz no enunciado
  matriz_migracao_2006$`Asia` == 1501490,
  # ... (verifique todos os valores da matriz no enunciado)
)



# (b) Refugiados do Afeganistão e Paquistão para o Canadá a partir de 1972
refugiados_afeganistao_canada <- refugiados_completo %>%
  filter(ano >= 1972, origem == "Afghanistan", destino == "Canada") %>%
  nrow()

refugiados_paquistao_canada <- refugiados_completo %>%
  filter(ano >= 1972, origem == "Pakistan", destino == "Canada") %>%
  nrow()

alternativa_b <- (refugiados_afeganistao_canada == 172075) && (refugiados_paquistao_canada == 219920)

# (c) 5 países que mais enviaram refugiados em 1965
top_5_1965 <- refugiados_completo %>%
  filter(ano == 1965) %>%
  group_by(origem) %>%
  summarise(total = n()) %>%
  arrange(desc(total)) %>%
  slice_head(n = 5)

subregioes_top_5 <- countrycode(top_5_1965$origem, "country.name", "region")
alternativa_c <- all(subregioes_top_5 %in% c("Sub-Saharan Africa", "Southern Europe"))


# (d) 6 países que mais receberam refugiados a partir de 1982
top_6_1982 <- refugiados_completo %>%
  filter(ano >= 1982) %>%
  group_by(destino) %>%
  summarise(total = n()) %>%
  arrange(desc(total)) %>%
  slice_head(n = 6)

total_refugiados_top_6 <- sum(top_6_1982$total)
alternativa_d <- total_refugiados_top_6 == 19523

# (e) 27 países que receberam pelo menos 5382652 refugiados
paises_5382652 <- refugiados_completo %>%
  group_by(destino) %>%
  summarise(total = n()) %>%
  filter(total >= 5382652) %>%
  nrow()

alternativa_e <- paises_5382652 == 27

# Imprime os resultados
print(paste("Alternativa (a) é correta:", alternativa_a))
print(paste("Alternativa (b) é correta:", alternativa_b))
print(paste("Alternativa (c) é correta:", alternativa_c))
print(paste("Alternativa (d) é correta:", alternativa_d))
print(paste("Alternativa (e) é correta:", alternativa_e))


