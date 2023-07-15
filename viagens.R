library(dplyr)
library(downloader)
library(lubridate)

memory.limit(24576)

url <- "https://portaldatransparencia.gov.br/download-de-dados/viagens/2023"
download(url, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip")

viagens <- read.csv2("2023_Viagem.csv", dec =",", fileEncoding='latin1')
file.remove(c('dataset.zip', '2023_Pagamento.csv', '2023_Passagem.csv', '2023_Trecho.csv', '2023_Viagem.csv'))

IFS <- viagens %>%
  filter(Código.órgão.solicitante == "26423",
         Situação == 'Realizada') %>% 
  select(nome=Nome, cargo=Cargo,inicio=Período...Data.de.início, fim=Período...Data.de.fim, destino=Destinos, valor_diaria=Valor.diárias, valor_passagem=Valor.passagens, urgencia=Viagem.Urgente) %>% 
  mutate(inicio= dmy(inicio), fim= dmy(fim))


saveRDS(IFS,'IFS.rds')
