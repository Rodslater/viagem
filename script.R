library(dplyr)
library(purrr)
library(downloader)
library(lubridate)
library(data.table)
library(jsonlite)
library(httr)  # Adiciona httr para webhook

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

# Anos para processar (ano atual e 4 anteriores)
ano_atual <- year(Sys.Date())
anos <- (ano_atual - 4):ano_atual

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
  
  # Salva o arquivo JSON
  write_json(IFS_completo, "IFS_viagens.json", pretty = TRUE, auto_unbox = TRUE)
  
  # WEBHOOK - Enviar notificação para n8n
  webhook_url <- "https://n8n.rodslater.com/webhook/ifs_viagens"
  
  payload <- list(
    status = "success",
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    message = "Dados de viagens IFS atualizados com sucesso",
    total_viagens = nrow(IFS_completo),
    anos_processados = unique(IFS_completo$ano),
    periodo_inicio = as.character(min(IFS_completo$inicio, na.rm = TRUE)),
    periodo_fim = as.character(max(IFS_completo$fim, na.rm = TRUE)),
    viagens_por_ano = resumo_por_ano,
    arquivo_atualizado = "IFS_viagens.json"
  )
  
  tryCatch({
    cat("Ativando webhook n8n...\n")
    
    response <- POST(
      url = webhook_url,
      body = payload,
      encode = "json",
      add_headers("Content-Type" = "application/json"),
      timeout(30)
    )
    
    if (status_code(response) == 200) {
      cat("✅ Webhook n8n ativado com sucesso!\n")
      cat("Response:", content(response, "text"), "\n")
    } else {
      cat("❌ Erro ao ativar webhook. Status code:", status_code(response), "\n")
      cat("Response:", content(response, "text"), "\n")
    }
    
  }, error = function(e) {
    cat("❌ Erro ao chamar webhook:", e$message, "\n")
    
    tryCatch({
      cat("Tentando webhook simples (GET)...\n")
      response_get <- GET(webhook_url, timeout(30))
      if (status_code(response_get) == 200) {
        cat("✅ Webhook ativado com GET!\n")
      }
    }, error = function(e2) {
      cat("❌ Falha total ao ativar webhook:", e2$message, "\n")
    })
  })
  
} else {
  cat("Nenhum dado foi processado com sucesso.\n")
  IFS_completo <- NULL
  
  # WEBHOOK para caso de erro
  webhook_url <- "https://n8n.rodslater.com/webhook/ifs_viagens"
  
  payload_erro <- list(
    status = "error",
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    message = "Falha no processamento dos dados de viagens IFS",
    total_viagens = 0,
    anos_tentados = anos,
    erro = "Nenhum arquivo foi processado com sucesso"
  )
  
  tryCatch({
    response <- POST(
      url = webhook_url,
      body = payload_erro,
      encode = "json",
      add_headers("Content-Type" = "application/json"),
      timeout(30)
    )
    
    if (status_code(response) == 200) {
      cat("✅ Webhook de erro enviado com sucesso!\n")
    }
  }, error = function(e) {
    cat("❌ Erro ao enviar webhook de erro:", e$message, "\n")
  })
}

# Limpeza final - remove qualquer arquivo temporário restante
arquivos_temp <- list.files(pattern = "dataset_.*\\.zip|\\d{4}_.*\\.csv")
if(length(arquivos_temp) > 0) {
  file.remove(arquivos_temp)
}

cat("Processamento concluído!\n")
