############################################################

# Brasília, 13 de abril de 2026
# Seleção IPEA - Chamada Pública nº 11/2026
# Teste remoto
# Candidato: Luís Gustavo do Amaral Vinha

# Desigualdades na conclusão do ensino médio segundo a
# PNAD Contínua – 2013 a 2019
#
############################################################

######################################################################
######################################################################

# Questão 2

######################################################################
######################################################################

# ==========================================================
# Pacotes
# ==========================================================
packages <- c("PNADcIBGE", "data.table", "survey", "ggplot2")

to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

invisible(lapply(packages, library, character.only = TRUE))

# Ajuste para estratos com uma única UPA
options(survey.lonely.psu = "adjust")

# ==========================================================
# Diretório para armazenamento dos microdados
# ==========================================================
dir_pnad <- "C:/IPEA_Selecao/pnad_dados"
if (!dir.exists(dir_pnad)) dir.create(dir_pnad, recursive = TRUE)

# ==========================================================
# Configurações fixas
# ==========================================================
anos <- 2013:2019
trimestre_fixo <- 4

# ==========================================================
# Função para leitura dos microdados trimestrais
# ----------------------------------------------------------
# A análise usa o 4º trimestre de cada ano para garantir
# comparabilidade temporal ao longo de 2013–2019.
# ==========================================================
ler_pnad_trim <- function(ano, trimestre = trimestre_fixo, savedir = dir_pnad) {
  message("Lendo ano: ", ano, " | trimestre: ", trimestre)
  
  obj <- get_pnadc(
    year = ano,
    quarter = trimestre,
    vars = c(
      "Ano", "Trimestre", "UF", "UPA", "Estrato", "V1028",
      "V2009",   # idade
      "V2010",   # raça/cor
      "VD3004",  # nível de instrução mais elevado alcançado
      "VD3005"   # anos de estudo
    ),
    labels = FALSE,
    design = FALSE,
    savedir = savedir
  )
  
  dt <- as.data.table(obj)
  dt[, ano_ref := ano]
  dt[, trimestre_ref := trimestre]
  
  return(dt)
}

# ==========================================================
# Leitura de todos os anos
# ==========================================================
pnad_lista <- lapply(anos, ler_pnad_trim)
names(pnad_lista) <- as.character(anos)

# Checagem opcional
# class(pnad_lista[["2019"]])
# dim(pnad_lista[["2019"]])
# names(pnad_lista[["2019"]])

# ==========================================================
# Função para processar um ano
# ----------------------------------------------------------
# Etapas:
# 1. Filtrar jovens de 15 a 18 anos
# 2. Definir conclusão do ensino médio
# 3. Recodificar raça/cor
# 4. Criar regiões
# 5. Aplicar desenho amostral
# 6. Estimar taxas para Brasil e regiões
# 7. Armazenar também o tamanho da amostra por célula
# ==========================================================
processar_ano <- function(dt_raw, ano_ref) {
  
  dt <- copy(as.data.table(dt_raw))
  
  # ---------------------------------------
  # 1. Checagem de variáveis mínimas
  # ---------------------------------------
  vars_necessarias <- c("UF", "UPA", "Estrato", "V1028", "V2009", "V2010", "VD3004")
  faltantes <- setdiff(vars_necessarias, names(dt))
  if (length(faltantes) > 0) {
    stop("Faltam variáveis na base: ", paste(faltantes, collapse = ", "))
  }
  
  # ---------------------------------------
  # 2. Faixa etária analisada
  # ---------------------------------------
  dt <- dt[V2009 %in% 15:18]
  
  # ---------------------------------------
  # 3. Indicador de conclusão do EM
  # VD3004 = nível de instrução mais elevado
  # Considera-se concluinte:
  # ensino médio completo ou nível superior
  # ---------------------------------------
  dt[, concluiu_em := ifelse(VD3004 >= 5, 1, 0)]
  
  # ---------------------------------------
  # 4. Recodificação de raça/cor
  # ---------------------------------------
  dt[, racacor := fifelse(
    V2010 == 1, "Branca",
    fifelse(V2010 == 2, "Preta",
            fifelse(V2010 == 3, "Amarela",
                    fifelse(V2010 == 4, "Parda",
                            fifelse(V2010 == 5, "Indígena", NA_character_))))
  )]
  
  # ---------------------------------------
  # 5. Criação da variável região
  # ---------------------------------------
  dt[, regiao := fifelse(
    UF %in% c(11, 12, 13, 14, 15, 16, 17), "Norte",
    fifelse(UF %in% c(21, 22, 23, 24, 25, 26, 27, 28, 29), "Nordeste",
            fifelse(UF %in% c(31, 32, 33, 35), "Sudeste",
                    fifelse(UF %in% c(41, 42, 43), "Sul",
                            fifelse(UF %in% c(50, 51, 52, 53), "Centro-Oeste", NA_character_))))
  )]
  
  # ---------------------------------------
  # 6. Remover casos inválidos
  # ---------------------------------------
  dt <- dt[
    !is.na(racacor) &
      !is.na(regiao) &
      !is.na(concluiu_em) &
      !is.na(V1028) &
      !is.na(UPA) &
      !is.na(Estrato)
  ]
  
  if (nrow(dt) == 0) return(data.table())
  
  # ---------------------------------------
  # 7. Desenho amostral complexo
  # ---------------------------------------
  des <- svydesign(
    ids = ~UPA,
    strata = ~Estrato,
    weights = ~V1028,
    data = dt,
    nest = TRUE
  )
  
  # ---------------------------------------
  # 8. Brasil: taxa de conclusão
  # ---------------------------------------
  brasil_taxa <- svyby(
    ~concluiu_em,
    ~V2009 + racacor,
    des,
    svymean,
    na.rm = TRUE
  )
  brasil_taxa <- as.data.table(brasil_taxa)
  brasil_taxa[, regiao := "Brasil"]
  brasil_taxa[, taxa := concluiu_em * 100]
  brasil_taxa[, ano := ano_ref]
  brasil_taxa[, idade := V2009]
  brasil_taxa <- brasil_taxa[, .(ano, idade, racacor, regiao, taxa)]
  
  # Brasil: tamanho da subamostra
  brasil_n <- dt[, .(n_amostra = .N), by = .(idade = V2009, racacor)]
  brasil_n[, regiao := "Brasil"]
  
  brasil <- merge(
    brasil_taxa,
    brasil_n,
    by = c("idade", "racacor", "regiao"),
    all.x = TRUE
  )
  
  # ---------------------------------------
  # 9. Regiões: taxa de conclusão
  # ---------------------------------------
  regioes_taxa <- svyby(
    ~concluiu_em,
    ~V2009 + racacor + regiao,
    des,
    svymean,
    na.rm = TRUE
  )
  regioes_taxa <- as.data.table(regioes_taxa)
  regioes_taxa[, taxa := concluiu_em * 100]
  regioes_taxa[, ano := ano_ref]
  regioes_taxa[, idade := V2009]
  regioes_taxa <- regioes_taxa[, .(ano, idade, racacor, regiao, taxa)]
  
  # Regiões: tamanho da subamostra
  regioes_n <- dt[, .(n_amostra = .N), by = .(idade = V2009, racacor, regiao)]
  
  regioes <- merge(
    regioes_taxa,
    regioes_n,
    by = c("idade", "racacor", "regiao"),
    all.x = TRUE
  )
  
  # ---------------------------------------
  # 10. Juntar Brasil + regiões
  # ---------------------------------------
  res <- rbindlist(list(brasil, regioes), use.names = TRUE, fill = TRUE)
  res[, ano := ano_ref]
  
  # Ordenação dos fatores
  res[, regiao := factor(
    regiao,
    levels = c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
  )]
  
  res[, racacor := factor(
    racacor,
    levels = c("Branca", "Preta", "Parda", "Amarela", "Indígena")
  )]
  
  setorder(res, idade, regiao, racacor, ano)
  
  return(res[])
}

# ==========================================================
# Teste com 2019
# ==========================================================
teste_2019 <- processar_ano(pnad_lista[["2019"]], 2019)

# Checagem opcional
# dim(teste_2019)
# head(teste_2019)

# ==========================================================
# Processar todos os anos
# ==========================================================
resultados <- rbindlist(
  lapply(seq_along(pnad_lista), function(i) {
    processar_ano(pnad_lista[[i]], anos[i])
  }),
  use.names = TRUE,
  fill = TRUE
)

# Checagens opcionais
# dim(resultados)
# head(resultados)
# tail(resultados)

# ==========================================================
# Função para montar tabela final da nota
# ----------------------------------------------------------
# Formato de cada célula: taxa (n)
# ==========================================================
montar_tabela_idade_texto <- function(dt_resultados, idade_alvo) {
  tab <- copy(dt_resultados[idade == idade_alvo])
  
  tab[, regiao := factor(
    regiao,
    levels = c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
  )]
  
  tab[, racacor := factor(
    racacor,
    levels = c("Branca", "Preta", "Parda", "Amarela", "Indígena")
  )]
  
  tab[, celula := paste0(
    format(round(taxa, 2), nsmall = 2, decimal.mark = ","),
    " (", n_amostra, ")"
  )]
  
  tab_wide <- dcast(
    tab,
    regiao + racacor ~ ano,
    value.var = "celula"
  )
  
  setorder(tab_wide, regiao, racacor)
  return(tab_wide)
}

# ==========================================================
# 10. Tabelas finais da nota
# ==========================================================
tab_15_final <- montar_tabela_idade_texto(resultados, 15)
tab_16_final <- montar_tabela_idade_texto(resultados, 16)
tab_17_final <- montar_tabela_idade_texto(resultados, 17)
tab_18_final <- montar_tabela_idade_texto(resultados, 18)

# Visualização opcional
# tab_15_final
# tab_16_final
# tab_17_final
 tab_18_final

# ==========================================================
# 11. Bases para gráficos
# ----------------------------------------------------------
# Nos gráficos da nota entram apenas:
# Branca, Preta e Parda
# ==========================================================
filtrar_grafico <- function(dt_resultados, idade_alvo) {
  dt <- copy(dt_resultados[idade == idade_alvo & racacor %in% c("Branca", "Preta", "Parda")])
  dt[, racacor := factor(racacor, levels = c("Branca", "Preta", "Parda"))]
  return(dt)
}

graf_15 <- filtrar_grafico(resultados, 15)
graf_16 <- filtrar_grafico(resultados, 16)
graf_17 <- filtrar_grafico(resultados, 17)
graf_18 <- filtrar_grafico(resultados, 18)

# ==========================================================
# Função para gráficos
# ==========================================================
plot_idade <- function(dt_graf, idade_alvo) {
  ggplot(dt_graf, aes(x = ano, y = taxa, color = racacor, group = racacor)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8) +
    facet_wrap(~regiao, ncol = 3) +
    scale_x_continuous(breaks = 2013:2019) +
    labs(
      title = paste("Taxa de conclusão do ensino médio -", idade_alvo, "anos"),
      x = "Ano",
      y = "Percentual de concluintes",
      color = "Raça/cor"
    ) +
    theme_minimal()
}

# ==========================================================
# Gráficos finais da nota
# ==========================================================
g15 <- plot_idade(graf_15, 15)
g16 <- plot_idade(graf_16, 16)
g17 <- plot_idade(graf_17, 17)
g18 <- plot_idade(graf_18, 18)

# Visualização opcional
# g15
# g16
# g17
 g18

# ==========================================================
# Exportação opcional
# ==========================================================
# fwrite(tab_15_final, "C:/IPEA_Selecao/tab_15_final.csv")
# fwrite(tab_16_final, "C:/IPEA_Selecao/tab_16_final.csv")
# fwrite(tab_17_final, "C:/IPEA_Selecao/tab_17_final.csv")
# fwrite(tab_18_final, "C:/IPEA_Selecao/tab_18_final.csv")

# ggsave("C:/IPEA_Selecao/grafico_15anos.png", g15, width = 12, height = 7, dpi = 300)
# ggsave("C:/IPEA_Selecao/grafico_16anos.png", g16, width = 12, height = 7, dpi = 300)
# ggsave("C:/IPEA_Selecao/grafico_17anos.png", g17, width = 12, height = 7, dpi = 300)
# ggsave("C:/IPEA_Selecao/grafico_18anos.png", g18, width = 12, height = 7, dpi = 300)