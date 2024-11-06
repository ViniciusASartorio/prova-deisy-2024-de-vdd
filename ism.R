library(usethis)
gitcreds::gitcreds_set()
create_from_github()
usethis::create_github_token()
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




##bruhhhh
# Carregando as bibliotecas
library(dplyr)
library(readr)
library(lubridate)
library(tidyverse)
library(data.table)
library(stringr)

#QUESTAO 1#
#matrizes
A <- matrix(c(28, 32, 8, 9, 49,
              7, 21, 35, 28, 10,
              47, 43, 15, 34, 2,
              48, 42, 19, 32, 26,
              45, 44, 39, 50, 26), nrow = 5, byrow = TRUE)

B <- matrix(c(0, 26, 3, 8, 30,
              35, 12, 19, 27, 27,
              27, 24, 12, 17, 29,
              31, 36, 40, 35, 8,
              24, 43, 31, 21, 39), nrow = 5, byrow = TRUE)

C = solve(A%*%t(B))

P = B %*% (t(B) %*% B) %*% t(B)

#Considere:
#  C = (A · B T )−1
#e
#P = B · (B T · B) · B T
#Assinale todas as alternativas correta(s).

#(a) Considere a matriz de projeção P . A soma de seus autovetores é dada por: -1,1193.

autovetorP = eigen(P)
sum(autovetorP$vectors)

#(b) A soma dos valores absolutos da diagonal da matriz C é 0,0722.

sum(abs(diag(C)))

#(c) A soma de uma matriz triangular inferior para a matriz A é 233.

sum(A[lower.tri(A)])

#(d) O log10 do valor absoluto do determinante de A é 6,335. O log10 do valor absoluto 
#do determinante de B é 6,7168. O log10 do valor absoluto do determinante da matriz
#resultante do produto matricial entre A e B é 13,0518

log10(abs(det(A)))
log10(abs(det(B)))
log10(abs(det(AB)))
AB = A%*%B

#(e) O maior elemento da diagonal do inverso da matriz resultante do produto matricial 
#entre A e o transposto de B é 0,026.

ABT = A%*%t(B)
max(diag(solve(ABT)))



#QUESTAO 2#

chocolate = read.csv('/home/est/vls23/Downloads/P1_2023_deisy/chocolate.csv')

#A) Existem 2443 países que produzem chocolate.

length(unique(chocolate$local_compania))

----------------------------------------------
  
  chocolate %>%
  select(local_compania) %>%
  unique() %>%
  nrow()

#B) Existem 104 chocolates com 4 ingredientes que são descritos por 2 características.

num_ing <- str_count(chocolate$ingredientes, ",") +1
num_car <- str_count(chocolate$caracteristicas, ",") +1
choco4ing2car <- chocolate %>% mutate(num_car = str_count(chocolate$caracteristicas, ",")+1) %>% 
  filter(num_ing == 4, num_car == 2) %>% count() %>%
  pull(n)
choco4ing2car

-------------------------------------------
  
  chocolate %>%
  mutate(virg_carac = str_count(caracteristicas, ","), virg_ingred = str_count(ingredientes, ",")) %>%
  filter(virg_carac == 1 & virg_ingred == 3) %>%
  nrow()    


#C) A frequência absoluta para chocolates que contenham 5 ingredientes é 750.

chocolate %>% filter(num_ing == 5) %>%
  count() %>%
  pull(n)

#(d) As 8 caracterististicas mais marcantes dos chocolates são sweet, nutty, cocoa, roasty,
#creamy, earthy, sandy e fatty e juntas correspondem a 1663 descrições dos chocolates.

carac_separadas <- chocolate %>%
  separate_rows(caracteristicas, sep = ",") 


carac_separadas <- str_trim(carac_separadas$caracteristicas, side = "left")

carac_separadas <- as.data.frame(carac_separadas)

carac_separadas %>%
  group_by(carac_separadas) %>%
  summarise(frequencia = n()) %>%
  arrange(desc(frequencia)) %>%
  head(8) %>%
  summarise(total = sum(frequencia))

#(e) Existem 81 chocolates que incluem o ingrediente Adoçante em sua composição.

chocolate %>%
  filter(str_detect(ingredientes, 'S\\*')) %>%
  count() %>%
  pull(n)

---------------------------------------------
  
  sum(str_detect(chocolate$ingredientes, "[*]"))

# QUESTAO 3 #

art = read.csv('/home/est/vls23/Downloads/P1_2023_deisy/Art.csv')
art_moma = read.csv('/home/est/vls23/Downloads/P1_2023_deisy/Art_Moma.csv')
arts = art%>%inner_join(art_moma, by = 'artist_unique_id')

#(a) Os 3 artista(s) com mais exposições no The Whitney classificados em 
#ordem decrescente de exposições são: 
#Edward Hopper, Georgia O’Keeffe e Stuart Davis.

top_whitney_artists <- art_moma  %>%
  group_by(artist_unique_id) %>%
  summarise(total_exposicoes = sum(whitney_count_to_year, na.rm = TRUE)) %>%
  arrange(desc(total_exposicoes)) %>%
  slice(1:3) %>%
  pull(artist_unique_id)
ids = c(96,145,365)
art %>% filter(artist_unique_id %in% ids )

-------------------------------------------
  
  artist_id_especifico <- 96 #145 #365
artist_name <- art %>%
  filter(artist_unique_id == artist_id_especifico) %>%
  select(artist_name) %>%
  pull()
print(artist_name)   

-----------------------------------------------------
  arts %>%
  select(artist_name, whitney_count_to_year) %>%
  filter(whitney_count_to_year > 0) %>%
  group_by(artist_name) %>%
  summarize(soma = sum(whitney_count_to_year)) %>%
  arrange(desc(soma))

#(b) Do total de artistas, 152 são Swiss, Mexican ou Japanese.

art%>% filter(artist_nationality %in% c('Swiss','Mexican','Japanese'))
n_distinct(artist_name)

-----------------------------------------------------------
  
  count(art %>% filter(artist_nationality == 'Swiss'))
count(art %>% filter(artist_nationality == 'Mexican'))
count(art %>% filter(artist_nationality == 'Japanese'))

-----------------------------------------------------------
  
  art %>%
  select(artist_nationality) %>%
  filter(artist_nationality %in% c("Swiss", "Mexican", "Japanese")) %>%
  summarise(n=n())

#(c) Apenas 6 artista(s) com a nacionalidade Swiss tiveram entre 0 e 1 exposições no The
#Whitney.

arts %>%
  filter(artist_nationality == "Swiss" & whitney_count_to_year <= 1) %>%
  distinct(artist_name) %>%
  count()

--------------------------------------------------------------
  
  arts %>%
  filter(artist_nationality == "Swiss" & whitney_count_to_year < 2) %>%
  select(artist_name) %>%
  unique()

#(d) A diferença entre a média de páginas para artistas Brancos e Não Brancos no ano de
#2007 é -0,24.

media_paginas_2007 <- arts %>%
  filter(year == 2007) %>%
  group_by(artist_race_nwi) %>%
  summarise(media_paginas = mean(space_ratio_per_page_total, na.rm = TRUE)) %>%
  spread(artist_race_nwi, media_paginas)
diferenca_media <- media_paginas_2007$`White` - media_paginas_2007$`Non-White`
diferenca_media

----------------------------------------------
  
  arts %>%
  filter(year == 2007) %>%
  select(space_ratio_per_page_total, artist_race_nwi) %>%
  group_by(artist_race_nwi) %>%
  summarise(resultado = mean(space_ratio_per_page_total))


#(e) Dos artista(s) que expuseram no The Whitney, apenas 164 aparecem nos livros
#‘Gardner’ e ‘Janson’.

artistas_both_books <- arts %>%
  filter(book %in% c("Gardner", "Janson") & whitney_count_to_year > 0) %>%
  group_by(artist_name) %>%
  filter(n_distinct(book) == 2) %>%
  distinct(artist_name) %>%
  count()
artistas_both_books

-------------------------------------------------
  
  arts %>%
  filter(whitney_count_to_year > 0) %>%
  select(artist_name, book) %>%
  group_by(artist_name) %>%
  count(book) %>%
  arrange(artist_name) %>%
  count(artist_name) %>%
  filter(n==2)



#4. (3 pontos) Para esse exercício você deverá utilizar os banco de dados refugiados_pais.csv.gz
#e refugiados.csv.gz. Considere apenas observações completas.
#Assinale todas as alternativas correta(s).

refugiados = read.csv('/home/est/vls23/Downloads/P1_2023_deisy/refugiados.csv')
refugiados_pais = read.csv('/home/est/vls23/Downloads/P1_2023_deisy/refugiados_pais.csv')

#preparando os dados fazer juncoes e tirando os NA pra usar na questao

regiao_origem <- refugiados_pais %>%
  select(id, regiao, nome, subregiao) %>%
  rename(regiao_origem = regiao, nome_origem = nome,subregiao_origem = subregiao)

regiao_destino <- refugiados_pais %>%
  select(id, regiao, nome, subregiao) %>%
  rename(regiao_destino = regiao, nome_destino = nome, subregiao_destino = subregiao)

refu_juncao = refugiados %>%
  left_join(., regiao_origem, by = c("id_origem" = "id"))

refu_juncao = refu_juncao %>%
  left_join(., regiao_destino, by = c("id_destino" = "id"))

refu_juncao <- refu_juncao %>%
  filter(!is.na(regiao_origem)) %>%
  filter(!is.na(regiao_destino)) 


#(a) A matriz de migração [origem, destino] intercontinental do ano 2006 é dada por:

refu_juncao %>%
  filter(ano == 2006) %>%
  select(regiao_origem, regiao_destino, refugiados) %>%
  group_by(regiao_origem, regiao_destino) %>%
  summarise(n = sum(refugiados)) %>%
  pivot_wider(names_from = regiao_destino, values_from = n )

#(b) A partir de 1972 houveram 172075 refugiados partindo do país: Afghanistan para o
#país: Canada, e 219920 refugiados partindo do país: Pakistan para o país: Canada.

afg_canada <- refugiados %>%
  filter(id_origem == "AFG", id_destino == "CAN", ano >= 1972) %>%
  summarise(total = sum(refugiados)) %>%
  pull(total)

--------------------------------------------------
  
  refu_juncao %>%
  filter(ano >= 1972 & nome_origem == "Afghanistan" & nome_destino == "Canada") %>%
  summarise(sum(refugiados))

refu_juncao %>%
  filter(ano >= 1972 & nome_origem == "Pakistan" & nome_destino == "Canada") %>%
  summarise(sum(refugiados))


#(c) Os 5 países que mais enviaram refugiados no ano de 1965 pertencem às subregiões
#Sub-Saharan Africa e Southern Europe.

refu_juncao %>%
  filter(ano == 1965) %>%
  select(nome_origem, subregiao_origem, refugiados) %>%
  group_by(nome_origem, subregiao_origem) %>%
  summarise(n = sum(refugiados)) %>%
  arrange(desc(n))

#(d) Os 6 países que mais receberam refugiados a partir de 1982 receberam juntos
#19523 refugiados.

total_top_6 <- refugiados %>%
  filter(ano >= 1982) %>%
  count(id_destino, sort = TRUE) %>%
  slice(1:6) %>%
  summarise(total = sum(n)) %>%
  pull(total)

-----------------------------------------
  
  refu_juncao %>%
  filter(ano >= 1982) %>%
  select(nome_destino, refugiados) %>%
  group_by(nome_destino) %>%
  summarise(n = sum(refugiados)) %>%
  arrange(desc(n)) %>%
  head() %>%
  select(n) %>%
  sum()

#(e) Existem 27 países que receberam pelo menos 5382652 refugiados.

pais_5382652 <- refugiados %>%
  group_by(id_destino) %>%
  summarise(total_refugiados = sum(refugiados)) %>%
  filter(total_refugiados >= 5382652) 

-----------------------------------------------------
  
  refu_juncao %>%
  select(nome_destino, refugiados) %>%
  group_by(nome_destino) %>%
  summarise(n = sum(refugiados)) %>%
  filter(n >= 5382652) %>%
  arrange(desc(n)) %>%
  View()
