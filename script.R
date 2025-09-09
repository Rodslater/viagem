library(dplyr)
library(purrr)
library(downloader)
library(lubridate)
library(data.table)
library(jsonlite)

# Função para baixar e processar dados de um ano específico
baixar_processar_ano <- function(ano) {
  cat("Processando ano:", ano, "\n")
  
  # URL para o ano específico
  url <- paste0("https://portaldatransparencia.gov.br/download-de-dados/viagens/", ano)
  
  # Nome do arquivo zip
  zip_file <- paste0("dataset_", ano, ".zip")
  csv_file <- paste0(ano, "_Viagem.csv")
  
  # Download do arquivo
  tryCatch({
    download(url, dest = zip_file, mode = "wb")
    unzip(zip_file)
    
    # Leitura do arquivo CSV
    viagens <- fread(csv_file, encoding = 'Latin-1')
    
    # Processamento dos dados do IFS
    IFS_ano <- viagens %>%
      filter(`Código órgão solicitante` == "26423",
             Situação == 'Realizada') %>% 
      select(nome = Nome, 
             cargo = Cargo,
             inicio = `Período - Data de início`, 
             fim = `Período - Data de fim`, 
             destino = Destinos, 
             valor_diaria = `Valor diárias`, 
             valor_passagem = `Valor passagens`, 
             urgencia = `Viagem Urgente`) %>% 
      mutate(inicio = dmy(inicio), 
             fim = dmy(fim),
             ano = ano) %>%  # Adiciona coluna do ano
      relocate(ano, .before = nome)  # Move a coluna ano para o início
    
    # Limpeza dos arquivos temporários
    arquivos_para_remover <- c(zip_file, 
                               paste0(ano, "_Pagamento.csv"),
                               paste0(ano, "_Passagem.csv"), 
                               paste0(ano, "_Trecho.csv"),
                               csv_file)
    
    # Remove apenas os arquivos que existem
    arquivos_existentes <- arquivos_para_remover[file.exists(arquivos_para_remover)]
    if(length(arquivos_existentes) > 0) {
      file.remove(arquivos_existentes)
    }
    
    cat("Ano", ano, "processado com sucesso.", nrow(IFS_ano), "viagens encontradas.\n")
    return(IFS_ano)
    
  }, error = function(e) {
    cat("Erro ao processar ano", ano, ":", e$message, "\n")
    return(NULL)
  })
}

# Anos para processar (ano atual e 3 anteriores)
ano_atual <- year(Sys.Date())
anos <- (ano_atual - 3):ano_atual

cat("Baixando dados de viagens do IFS para os anos:", paste(anos, collapse = ", "), "\n\n")

# Processar cada ano
lista_dataframes <- map(anos, baixar_processar_ano)

# Remove elementos NULL (anos que falharam)
lista_dataframes <- lista_dataframes[!sapply(lista_dataframes, is.null)]

# Combina todos os dataframes
if(length(lista_dataframes) > 0) {
  IFS_completo <- bind_rows(lista_dataframes)
  
  cat("\n=== RESUMO FINAL ===\n")
  cat("Total de viagens IFS encontradas:", nrow(IFS_completo), "\n")
  cat("Anos processados:", paste(unique(IFS_completo$ano), collapse = ", "), "\n")
  cat("Período:", min(IFS_completo$inicio, na.rm = TRUE), "a", max(IFS_completo$fim, na.rm = TRUE), "\n")
  
  # Resumo por ano
  cat("\nViagens por ano:\n")
  resumo_por_ano <- IFS_completo %>% 
    count(ano, sort = TRUE)
  print(resumo_por_ano)
  
  # Visualização das primeiras linhas
  cat("\nPrimeiras linhas do dataset completo:\n")
  print(head(IFS_completo))
  
} else {
  cat("Nenhum dado foi processado com sucesso.\n")
  IFS_completo <- NULL
}

# Limpeza final - remove qualquer arquivo temporário restante
arquivos_temp <- list.files(pattern = "dataset_.*\\.zip|\\d{4}_.*\\.csv")
if(length(arquivos_temp) > 0) {
  file.remove(arquivos_temp)
}

write_json(IFS_completo, "IFS_viagens.json", pretty = TRUE, auto_unbox = TRUE)
