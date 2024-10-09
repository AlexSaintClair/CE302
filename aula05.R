library(tidyverse)
dados <- readr::read_csv("Mental Health Dataset.csv")
head(dados,2)
dados
## Para vermos o resumo dos dados, podemos utilizar a função glimpse()
glimpse(dados)
# %>% # CRTL+Shift M

require(magrittr)
set.seed(123)

rnorm(10) %>% 
  multiply_by(5) %>% 
  add(5)


require(dplyr)
## Atribuição explicita
meu_data_frame <- data.frame(
  nome = c("Alice", "Bob", "Carol", "Ana", "João", "Carlos", "Patrícia", "Leonardo"),
  idade = c(25, 30, 28, 20, 27, 50, 60, 45),
  salario = c(5000, 6000, 5500, 8000, 2000, 3500, 10000, 3800 ), 
  meio_de_transporte = c('onibus', 'bicicleta', 'onibus', 'carro', 'carro', 'onibus', 'onibus', 'bicicleta'))

## Atribuição implicita
meu_data_frame %<>% 
  mutate(idade_50 = idade > 50)
glimpse(meu_data_frame)


require(data.table)

require(dplyr)




car_crash = fread("Brazil Total highway crashes 2010 - 2023.csv")
# Dados extraídos de https://www.kaggle.com/datasets/liamarguedas/brazil-total-highway-crashes-2010-2023

glimpse(car_crash)
