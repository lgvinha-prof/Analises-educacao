######################################################################
######################################################################

# Brasília, 13 de abril de 2026
# Seleção IPEA - Chamada Pública nº 11/2026
# Teste remoto
# Candidato: Luís Gustavo do Amaral Vinha

# Fontes de dados:
# IDEB: https://www.gov.br/inep/pt-br/areas-de-atuacao/pesquisas-estatisticas-e-indicadores/ideb/resultados
# Censo: https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-escolar

# Os arquivos relativos ao IDEB e Censo foram baixados e salvos
# em pasta de trabalho indicada.

######################################################################
######################################################################

# Questão 1

######################################################################
######################################################################


# =========================
# Pacotes
# =========================
packages <- c(
  "data.table", "readxl", "janitor", "ggplot2", "psych"
)

to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(packages, library, character.only = TRUE))

# =========================
# Diretório de dados
# =========================
dir_dados <- "C:/IPEA_Selecao/Dados_primarios"

# =========================
# Configurações fixas
# =========================
anos <- c(2017, 2019, 2021, 2023)
etapas <- c("ai" = "Anos iniciais", "af" = "Anos finais", "em" = "Ensino médio")


# Indicação de arquivos de dados
# IDEB
arquivos_ideb <- list(
  ai = file.path(dir_dados, "IDEB_anos_iniciais.xlsx"),
  af = file.path(dir_dados, "IDEB_anos_finais.xlsx"),
  em = file.path(dir_dados, "IDEB_ensino_medio.xlsx")
)

# Censo
arquivos_censo <- setNames(
  file.path(dir_dados, paste0("microdados_ed_basica_", anos, ".csv")),
  anos
)

# Variáveis utilizadas na composição do indicador de infraestrutura
# presentes nos arquivos do Censo Escolar 

# Variáveis por dimensão
vars_basica <- c(
  "in_agua_rede_publica",
  "in_energia_rede_publica",
  "in_esgoto_rede_publica"
)

vars_fisica_por_ano <- list(
  `2017` = c(
    "in_area_verde",
    "in_auditorio",
    "in_banheiro_dentro_predio",
    "in_banheiro_chuveiro",
    "in_cozinha",
    "in_despensa",
    "in_patio_coberto",
    "in_quadra_esportes",
    "in_refeitorio",
    "in_sala_diretoria",
    "in_sala_professor",
    "in_secretaria"
  ),
  `2019` = c(
    "in_area_verde",
    "in_auditorio",
    "in_banheiro",
    "in_banheiro_chuveiro",
    "in_cozinha",
    "in_despensa",
    "in_patio_coberto",
    "in_quadra_esportes",
    "in_refeitorio",
    "in_sala_diretoria",
    "in_sala_professor",
    "in_secretaria"
  ),
  `2021` = c(
    "in_area_verde",
    "in_auditorio",
    "in_banheiro",
    "in_banheiro_chuveiro",
    "in_cozinha",
    "in_despensa",
    "in_patio_coberto",
    "in_quadra_esportes",
    "in_refeitorio",
    "in_sala_diretoria",
    "in_sala_professor",
    "in_secretaria"
  ),
  `2023` = c(
    "in_area_verde",
    "in_auditorio",
    "in_banheiro",
    "in_banheiro_chuveiro",
    "in_cozinha",
    "in_despensa",
    "in_patio_coberto",
    "in_quadra_esportes",
    "in_refeitorio",
    "in_sala_diretoria",
    "in_sala_professor",
    "in_secretaria"
  )
)

vars_pedagogica <- c(
  "in_biblioteca",
  "in_sala_leitura",
  "in_sala_atendimento_especial",
  "in_laboratorio_ciencias",
  "in_laboratorio_informatica"
)

vars_acessibilidade <- c(
  "in_banheiro_pne",
  "in_acessibilidade_corrimao",
  "in_acessibilidade_elevador",
  "in_acessibilidade_pisos_tateis",
  "in_acessibilidade_vao_livre",
  "in_acessibilidade_rampas",
  "in_acessibilidade_sinal_sonoro",
  "in_acessibilidade_sinal_tatil",
  "in_acessibilidade_sinal_visual"

)

vars_equip <- c(
  "in_equip_copiadora",
  "in_equip_impressora",
  "in_equip_impressora_mult",
  "in_equip_dvd",
  "in_equip_som",
  "in_equip_tv",
  "in_equip_multimidia"
)

vars_tic <- c(
  "in_computador",
  "in_desktop_aluno",
  "in_comp_portatil_aluno",
  "in_tablet_aluno",
  "in_internet",
  "in_internet_alunos",
  "in_internet_administrativo",
  "in_internet_aprendizagem",
  "in_acesso_internet_computador",
  "in_banda_larga"
)

# =========================
# Funções utilitárias
# =========================

# Funções utilizadas para construção das medidas e análises dos
# dados

# Função para assegurar valores 0 e 1
to_binary01 <- function(x) {
  ifelse(x == 1, 1,
         ifelse(x == 0, 0, NA_real_))
}


# Função para impor estrutura de fatores para variáveis
fix_fatores <- function(dt) {
  dt[, etapa := factor(etapa, levels = c("Anos iniciais", "Anos finais", "Ensino médio"))]
  dt[, rede_nome := factor(rede_nome, levels = c("Municipal", "Estadual"))]
  dt
}

# Calcula um indicador sintético por linha a partir da média
# das variáveis binárias disponíveis em uma dimensão.
# Mantém NA como ausente e cria a nova variável com NA caso
# nenhuma das variáveis informadas exista no banco.
safe_mean_dim <- function(dt, vars, newname) {
  vars_ok <- vars[vars %in% names(dt)]
  
  if (length(vars_ok) == 0) {
    dt[, (newname) := NA_real_]
    return(dt)
  }
  
  dt[, (vars_ok) := lapply(.SD, function(x) {
    ifelse(x == 1, 1,
           ifelse(x == 0, 0, NA_real_))
  }), .SDcols = vars_ok]
  
  dt[, (newname) := rowMeans(.SD, na.rm = TRUE), .SDcols = vars_ok]
  
  return(dt)
}


# Funções para leitura das bases
ler_ideb <- function(arquivo) {
  dt <- read_excel(arquivo, skip = 9) |>
    clean_names() |>
    as.data.table()
  
  dt <- dt[!is.na(id_escola)]
  dt[dt == "ND"] <- NA
  dt[dt == "-"] <- NA
  
  cols_ideb <- names(dt)[grepl("vl_observado", names(dt))]
  dt[, (cols_ideb) := lapply(.SD, function(x) as.numeric(gsub(",", ".", x))), .SDcols = cols_ideb]
  
  dt <- dt[, .(
    UF = sg_uf,
    COD_MUN = co_municipio,
    CO_ENTIDADE = id_escola,
    TP_DEPENDENCIA = rede,
    ideb_2017 = vl_observado_2017,
    ideb_2019 = vl_observado_2019,
    ideb_2021 = vl_observado_2021,
    ideb_2023 = vl_observado_2023
  )]
  
  dt[, id_escola := as.character(CO_ENTIDADE)]
  dt
}

ler_censo <- function(arquivo, ano) {
  dt <- fread(arquivo, sep = ";", encoding = "Latin-1")
  dt <- clean_names(dt)
  dt[, ano := ano]
  dt[, id_escola := as.character(co_entidade)]
  dt
}

# FUNÇÃO PRINCIPAL

# ----------------------------------------------------------
# Função para construção dos indicadores de infraestrutura
# ----------------------------------------------------------
# Objetivo:
# Para um determinado ano do Censo Escolar, calcular:
# 1) os indicadores sintéticos das dimensões de infraestrutura;
# 2) o indicador geral de infraestrutura;
# 3) versões dos indicadores reescaladas de 0 a 10;
# 4) uma checagem da consistência da medida por meio de análise fatorial.
#
# Entrada:
# - censo_dt: base de dados do Censo Escolar para um ano específico
# - ano: ano de referência da base
#
# Saída:
# - uma lista contendo:
#   * data: base com os indicadores adicionados
#   * vars_indicadores: nomes dos indicadores dimensionais usados no ano
#   * fa: resultado da análise fatorial exploratória
# ----------------------------------------------------------
construir_indicadores <- function(censo_dt, ano) {
  
  # Converte o ano para texto, pois os conjuntos de variáveis
  # dependentes do ano estão organizados em listas nomeadas por string
  ano_chr <- as.character(ano)
  
  # --------------------------------------------------------
  # Construção dos indicadores por dimensão
  # --------------------------------------------------------
  # Em cada caso, a função safe_mean_dim:
  # - verifica quais variáveis existem na base;
  # - recodifica os itens para 0/1;
  # - calcula a média dos itens válidos por escola.
  # O resultado pode ser interpretado como o percentual de itens
  # presentes naquela dimensão.
  
  # Dimensão 1: infraestrutura básica
  # Ex.: água, energia, esgoto, saneamento etc.
  censo_dt <- safe_mean_dim(censo_dt, vars_basica, "infra_basica")
  
  # Dimensão 2: infraestrutura física
  # Ex.: salas, banheiros, quadra, cozinha, secretaria etc.
  # Observação: o conjunto de variáveis varia conforme o ano.
  censo_dt <- safe_mean_dim(censo_dt, vars_fisica_por_ano[[ano_chr]], "infra_fisica")
  
  # Dimensão 3: infraestrutura pedagógica
  # Ex.: biblioteca, laboratório, sala de leitura etc.
  censo_dt <- safe_mean_dim(censo_dt, vars_pedagogica, "infra_pedagogica")
  
  # Dimensão 4: acessibilidade
  # Para 2017, esse conjunto de variáveis não está disponível
  # de forma válida/comparável, por isso a dimensão é definida como NA.
  if (ano == 2017) {
    censo_dt[, infra_acessibilidade := NA_real_]
  } else {
    censo_dt <- safe_mean_dim(censo_dt, vars_acessibilidade, "infra_acessibilidade")
  }
  
  # Dimensão 5: equipamentos
  # Ex.: computador, impressora, TV, multimídia etc.
  censo_dt <- safe_mean_dim(censo_dt, vars_equip, "infra_equip")
  
  # Dimensão 6: TIC (tecnologias de informação e comunicação)
  # Ex.: internet, banda larga, computadores para alunos etc.
  # Assim como em acessibilidade, em 2017 essa dimensão não foi calculada
  # por indisponibilidade de informação válida/comparável.
  if (ano == 2017) {
    censo_dt[, infra_tic := NA_real_]
  } else {
    censo_dt <- safe_mean_dim(censo_dt, vars_tic, "infra_tic")
  }
  
  # --------------------------------------------------------
  # Construção do indicador geral de infraestrutura
  # --------------------------------------------------------
  # O indicador total é definido como a média simples entre as dimensões.
  # Para 2017, como acessibilidade e TIC não foram calculadas,
  # o indicador total utiliza apenas as dimensões disponíveis.
  vars_indicadores <- c("infra_basica", "infra_fisica", "infra_pedagogica", "infra_equip")
  
  if (ano != 2017) {
    vars_indicadores <- c(vars_indicadores, "infra_acessibilidade", "infra_tic")
  }
  
  # Calcula a média entre os indicadores dimensionais válidos
  censo_dt[, infra_total := rowMeans(.SD, na.rm = TRUE), .SDcols = vars_indicadores]
  
  # --------------------------------------------------------
  # Reescala dos indicadores para a faixa de 0 a 10
  # --------------------------------------------------------
  # Como os indicadores dimensionais e o indicador total variam
  # originalmente entre 0 e 1, aqui eles são multiplicados por 10
  # para facilitar a interpretação e a apresentação dos resultados.
  vars_scale <- c("infra_basica", "infra_fisica", "infra_pedagogica", "infra_equip", "infra_total")
  
  if (ano != 2017) {
    vars_scale <- c(vars_scale, "infra_acessibilidade", "infra_tic")
  }
  
  censo_dt[, paste0(vars_scale, "_10") := lapply(.SD, function(x) x * 10), .SDcols = vars_scale]
  
  # --------------------------------------------------------
  # Checagem da consistência da medida por análise fatorial
  # --------------------------------------------------------
  # A análise fatorial é aplicada aos indicadores das dimensões
  # (e não aos itens individuais) para verificar se eles se associam
  # a um fator comum de infraestrutura.
  
  # Subconjunto contendo apenas os indicadores dimensionais
  dt_factor <- censo_dt[, ..vars_indicadores]
  
  # Análise paralela:
  # auxilia na avaliação do número de fatores a serem retidos
  fa.parallel(dt_factor, fa = "fa", fm = "minres")
  
  # Ajuste de uma solução unifatorial:
  # utilizada para verificar se as dimensões se organizam em torno
  # de um fator geral de infraestrutura
  fa1 <- fa(dt_factor, nfactors = 1, fm = "minres")
  
  # Impressão das cargas fatoriais acima de 0.30
  print(fa1$loadings, cutoff = 0.3)
  
  # --------------------------------------------------------
  # Saída da função
  # --------------------------------------------------------
  # Retorna:
  # - a base com os indicadores adicionados;
  # - os nomes dos indicadores utilizados naquele ano;
  # - o objeto com o resultado da análise fatorial.
  list(
    data = censo_dt,
    vars_indicadores = vars_indicadores,
    fa = fa1
  )
}

# Montagem das bases IDEB + Censo por ano
prep_base_etapa <- function(ideb_sel, censo_dt, ano) {
  ideb_ano <- ideb_sel[, .(
    id_escola,
    ideb = get(paste0("ideb_", ano)),
    TP_DEPENDENCIA = TP_DEPENDENCIA
  )]
  
  ideb_ano <- ideb_ano[!is.na(ideb)]
  
  base <- merge(ideb_ano, censo_dt, by = "id_escola", all.x = TRUE)
  base <- base[tp_dependencia %in% c(2, 3)]
  base[, rede_nome := fifelse(tp_dependencia == 2, "Estadual", "Municipal")]
  base
}

prep_bases_ano <- function(ano, censo_dt, ideb_list) {
  list(
    ai = prep_base_etapa(ideb_list$ai, censo_dt, ano),
    af = prep_base_etapa(ideb_list$af, censo_dt, ano),
    em = prep_base_etapa(ideb_list$em, censo_dt, ano)
  )
}



# Tabelas e bases para gráficos
# Essas funções realizam os cálculos e montam os gráficos
# para cada ano

# Função para tabela com IDEB por rede
tab_ideb_ano <- function(bases_ano) {
  out <- rbindlist(lapply(names(bases_ano), function(et) {
    bases_ano[[et]][, .(
      etapa = etapas[[et]],
      n_escolas = .N,
      ideb_media = mean(ideb, na.rm = TRUE),
      ideb_dp = sd(ideb, na.rm = TRUE),
      ideb_min = min(ideb, na.rm = TRUE),
      ideb_max = max(ideb, na.rm = TRUE)
    ), by = rede_nome]
  }))
  setcolorder(out, c("rede_nome", "etapa", "n_escolas", "ideb_media", "ideb_dp", "ideb_min", "ideb_max"))
  out[]
}

# Função para tabela com infraestrutura por rede
tab_infra_ano <- function(bases_ano, ano) {
  vars_tab <- c("infra_total_10", "infra_basica_10", "infra_fisica_10", "infra_pedagogica_10", "infra_equip_10")
  nomes <- c("infra_total", "infra_basica", "infra_fisica", "infra_pedagogica", "infra_equip")
  if (ano != 2017) {
    vars_tab <- c(vars_tab, "infra_acessibilidade_10", "infra_tic_10")
    nomes <- c(nomes, "infra_acessibilidade", "infra_tic")
  }
  
  out <- rbindlist(lapply(names(bases_ano), function(et) {
    dt <- bases_ano[[et]]
    dt[, c(
      list(etapa = etapas[[et]], n_escolas = .N),
      setNames(lapply(vars_tab, function(v) mean(get(v), na.rm = TRUE)), nomes),
      list(dp_infra_total = sd(infra_total_10, na.rm = TRUE))
    ), by = rede_nome]
  }), fill = TRUE)
  
  out[]
}

# Função para tabela com correlação entre IDEB e infraestrutura por rede  
tab_corr_ano <- function(bases_ano, ano) {
  vars_corr <- c("infra_total", "infra_basica", "infra_fisica", "infra_pedagogica", "infra_equip")
  if (ano != 2017) vars_corr <- c(vars_corr, "infra_acessibilidade", "infra_tic")
  
  long <- rbindlist(lapply(names(bases_ano), function(et) {
    dt <- bases_ano[[et]]
    y <- as.numeric(dt[["ideb"]])
    data.table(
      etapa = etapas[[et]],
      indicador = vars_corr,
      correlacao = sapply(vars_corr, function(v) {
        x <- as.numeric(dt[[v]])
        cor(x, y, use = "pairwise.complete.obs")
      })
    )
  }))
  
  wide <- dcast(long, indicador ~ etapa, value.var = "correlacao")
  list(long = long, wide = wide)
}

# Função para preparação dos dados para 
# construção dos gráficos box-plot para Ideb
base_plot_ideb <- function(bases_ano) {
  dt <- rbindlist(lapply(names(bases_ano), function(et) {
    bases_ano[[et]][, .(etapa = etapas[[et]], rede_nome, ideb)]
  }))
  fix_fatores(dt)
}

# Função para preparação dos dados para
# construção dos gráficos box-plot para infraestrutura geral
base_plot_infra <- function(bases_ano) {
  dt <- rbindlist(lapply(names(bases_ano), function(et) {
    bases_ano[[et]][, .(etapa = etapas[[et]], rede_nome, infra_total_10)]
  }))
  fix_fatores(dt)
}

# Função para preparação dos dados para
# para construção de diagramas de dispersão entre 
# Ideb e indicadores de infraestrutura
base_plot_scatter <- function(bases_ano) {
  dt <- rbindlist(lapply(names(bases_ano), function(et) {
    bases_ano[[et]][, .(etapa = etapas[[et]], rede_nome, ideb, infra_total)]
  }))
  fix_fatores(dt)
}


# Função para construção dos box-plots do Ideb
grafico_box_ideb <- function(plot_dt, ano) {
  ggplot(plot_dt, aes(x = rede_nome, y = ideb, fill = rede_nome)) +
    geom_boxplot(alpha = 0.7) +
    coord_cartesian(ylim = c(0, 10)) +
    facet_wrap(~etapa, scales = "free_y") +
    labs(x = "Rede", y = "IDEB", fill = "Rede") +
    theme_minimal()
}

# Função para construção dos box-plots da infraestrutura geral
grafico_box_infra <- function(plot_dt, ano) {
  ggplot(plot_dt, aes(x = rede_nome, y = infra_total_10, fill = rede_nome)) +
    geom_boxplot() +
    facet_wrap(~etapa) +
    coord_cartesian(ylim = c(0, 10)) +
    labs(x = "", y = "Infraestrutura (0–10)", fill = "Rede") +
    theme_minimal()
}


# Função para construção do diagrama de dispersão entre
# Ideb e indicadores de infraestutura
grafico_scatter_etapa <- function(dt_scatter, etapa_nome) {
  ggplot(dt_scatter[etapa == etapa_nome], aes(x = infra_total, y = ideb, color = rede_nome)) +
    geom_point(alpha = 0.25) +
    labs(x = "Indicador geral de infraestrutura", y = "IDEB", color = "Rede") +
    theme_minimal()
}

# =========================
# Execução
# =========================

# Leitura dos dados do IDEB
ideb_list <- lapply(arquivos_ideb, ler_ideb)

# Leitura dos dados do Censo
censos <- lapply(anos, function(a) ler_censo(arquivos_censo[as.character(a)], a))
names(censos) <- as.character(anos)

# Construção dos indicadores por ano
indicadores <- lapply(anos, function(a) construir_indicadores(censos[[as.character(a)]], a))
names(indicadores) <- as.character(anos)

# atualizar censos com indicadores
censos <- lapply(indicadores, function(x) x$data)

# Montar bases IDEB + Censo por ano
bases <- lapply(anos, function(a) prep_bases_ano(a, censos[[as.character(a)]], ideb_list))
names(bases) <- as.character(anos)

# Gerar saídas por ano
resultados <- lapply(anos, function(a) {
  bases_ano <- bases[[as.character(a)]]
  list(
    tab_ideb = tab_ideb_ano(bases_ano),
    tab_infra = tab_infra_ano(bases_ano, a),
    tab_corr = tab_corr_ano(bases_ano, a),
    plot_ideb = base_plot_ideb(bases_ano),
    plot_infra = base_plot_infra(bases_ano),
    plot_scatter = base_plot_scatter(bases_ano)
  )
})
names(resultados) <- as.character(anos)

# =========================
# Geração das tabelas e gráficos
# =========================

# Tabelas
resultados[["2023"]]$tab_ideb
resultados[["2023"]]$tab_infra
resultados[["2023"]]$tab_corr$wide

# Gráficos
grafico_box_ideb(resultados[["2023"]]$plot_ideb, 2023)
grafico_box_infra(resultados[["2023"]]$plot_infra, 2023)
grafico_scatter_etapa(resultados[["2021"]]$plot_scatter, "Anos iniciais")
grafico_scatter_etapa(resultados[["2021"]]$plot_scatter, "Anos finais")
grafico_scatter_etapa(resultados[["2021"]]$plot_scatter, "Ensino médio")


