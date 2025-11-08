install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("sidrar")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(sidrar)

# Tabela 9552 - Moradores em domicílios (mil pessoas)
dados <- get_sidra(api = "/t/9552/n1/all/n2/all/v/10114/p/all/c1/allxt/c12404/allxt")

# Tratando os dados
dados <- dados %>% 
  select(
    regioes = `Brasil e Grande Região`,
    moradores_mil_pessoas = Valor,
    variável = Variável,
    situacao = `Situação do domicílio`,
    ano = Ano,
    seguranca_alimentar = `Situação de segurança alimentar existente no domicílio`)

# Agrupando e calculando totais por região, ano e segurança alimentar
dados_agrupados <- dados %>%
  group_by(regioes, ano, seguranca_alimentar) %>%
  summarise(total_moradores = sum(moradores_mil_pessoas, na.rm = TRUE), .groups = "drop")

# Calculando o total por região e ano (para calcular a porcentagem)
# 1. Filtramos para excluir a categoria totalizadora "Com insegurança alimentar"
# 2. As categorias que SÃO somadas são: "Com segurança alimentar", "Leve", "Moderada", "Grave".
totais_por_regiao <- dados_agrupados %>%
  filter(seguranca_alimentar != "Com insegurança alimentar") %>% 
  group_by(regioes, ano) %>%
  summarise(total_regiao = sum(total_moradores), .groups = "drop")

# Juntando e calculando as porcentagens
dados_final <- dados_agrupados %>%
  left_join(totais_por_regiao, by = c("regioes", "ano")) %>%
  mutate(porcentagem = (total_moradores / total_regiao) * 100)


# ------------ Mapas ----------

library(ggplot2)
library(showtext)
library(dplyr)
library(geobr)
library(sf)
library(patchwork)

font_add_google("Ubuntu", "Ubuntu")
showtext_auto()

# Baixando os dados geográficos das regiões do Brasil
regioes_geo <- read_region(year = 2020)

# ========== MAPA: Com insegurança alimentar grave ==========

# Mapa 2024
dados_mapa_2024_grave <- dados_final %>%
  filter(regioes != "Brasil", 
         ano == 2024,
         seguranca_alimentar == "Com insegurança alimentar grave") %>%
  mutate(name_region = case_when(
    regioes == "Região Norte" ~ "Norte",
    regioes == "Região Nordeste" ~ "Nordeste",
    regioes == "Região Sudeste" ~ "Sudeste",
    regioes == "Região Sul" ~ "Sul",
    regioes == "Região Centro-Oeste" ~ "Centro Oeste",
    grepl("Centro", regioes, ignore.case = TRUE) ~ "Centro Oeste",
    TRUE ~ regioes
  ))

mapa_dados_2024_grave <- regioes_geo %>%
  left_join(dados_mapa_2024_grave, by = "name_region")

centroides_2024_grave <- st_centroid(mapa_dados_2024_grave)
centroides_coords_2024_grave <- st_coordinates(centroides_2024_grave)
mapa_dados_2024_grave$lon <- centroides_coords_2024_grave[,1]
mapa_dados_2024_grave$lat <- centroides_coords_2024_grave[,2]

mapa_2024_grave <- ggplot(mapa_dados_2024_grave) +
  geom_sf(aes(fill = porcentagem), color = "white", size = 0.8) +
  geom_text(aes(x = lon, y = lat, 
                label = paste0(name_region, "\n", round(porcentagem, 1), "%")),
            family = "Ubuntu", size = 3.5, fontface = "bold") +
  scale_fill_gradient(low = "#FFCCCC", high = "#CC0000",
                      name = "Porcentagem (%)",
                      breaks = seq(0, 10, by = 1)) +
  labs(subtitle = "2024") +
  theme_void(base_family = "Ubuntu") +
  theme(
    plot.subtitle = element_text(face = "bold", family = "Ubuntu", 
                                 size = 13, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.background = element_rect(fill = "#D4C4B0", color = NA),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  )

# Mapa 2023
dados_mapa_2023_grave <- dados_final %>%
  filter(regioes != "Brasil", 
         ano == 2023,
         seguranca_alimentar == "Com insegurança alimentar grave") %>%
  mutate(name_region = case_when(
    regioes == "Região Norte" ~ "Norte",
    regioes == "Região Nordeste" ~ "Nordeste",
    regioes == "Região Sudeste" ~ "Sudeste",
    regioes == "Região Sul" ~ "Sul",
    regioes == "Região Centro-Oeste" ~ "Centro Oeste",
    grepl("Centro", regioes, ignore.case = TRUE) ~ "Centro Oeste",
    TRUE ~ regioes
  ))

mapa_dados_2023_grave <- regioes_geo %>%
  left_join(dados_mapa_2023_grave, by = "name_region")

centroides_2023_grave <- st_centroid(mapa_dados_2023_grave)
centroides_coords_2023_grave <- st_coordinates(centroides_2023_grave)
mapa_dados_2023_grave$lon <- centroides_coords_2023_grave[,1]
mapa_dados_2023_grave$lat <- centroides_coords_2023_grave[,2]

mapa_2023_grave <- ggplot(mapa_dados_2023_grave) +
  geom_sf(aes(fill = porcentagem), color = "white", size = 0.8) +
  geom_text(aes(x = lon, y = lat, 
                label = paste0(name_region, "\n", round(porcentagem, 1), "%")),
            family = "Ubuntu", size = 3.5, fontface = "bold") +
  scale_fill_gradient(low = "#FFCCCC", high = "#CC0000",
                      name = "Porcentagem (%)",
                      breaks = seq(0, 10, by = 1)) +
  labs(subtitle = "2023") +
  theme_void(base_family = "Ubuntu") +
  theme(
    plot.subtitle = element_text(face = "bold", family = "Ubuntu", 
                                 size = 13, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.background = element_rect(fill = "#D4C4B0", color = NA),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  )

# Combinando os mapas
mapa_grave_final <- mapa_2024_grave + mapa_2023_grave +
  plot_annotation(
    title = "Proporção de pessoas em insegurança alimentar grave por Região do Brasil",
    subtitle = "Comparação entre 2023 e 2024",
    caption = "Fonte: PNAD/SIDRA (Tabela 9751) | Autor: João Pedro",
    theme = theme(
      plot.title = element_text(face = "bold", family = "Ubuntu", 
                                size = 16, hjust = 0.5),
      plot.subtitle = element_text(face = "italic", family = "Ubuntu", 
                                   size = 13, hjust = 0.5),
      plot.caption = element_text(face = "italic", family = "Ubuntu", size = 12),
      plot.background = element_rect(fill = "#D4C4B0", color = NA)
    )
  )

print(mapa_grave_final)

# ========== MAPA: Com insegurança alimentar moderada ==========

# Mapa 2024
dados_mapa_2024_moderada <- dados_final %>%
  filter(regioes != "Brasil", 
         ano == 2024,
         seguranca_alimentar == "Com insegurança alimentar moderada") %>%
  mutate(name_region = case_when(
    regioes == "Região Norte" ~ "Norte",
    regioes == "Região Nordeste" ~ "Nordeste",
    regioes == "Região Sudeste" ~ "Sudeste",
    regioes == "Região Sul" ~ "Sul",
    regioes == "Região Centro-Oeste" ~ "Centro Oeste",
    grepl("Centro", regioes, ignore.case = TRUE) ~ "Centro Oeste",
    TRUE ~ regioes
  ))

mapa_dados_2024_moderada <- regioes_geo %>%
  left_join(dados_mapa_2024_moderada, by = "name_region")

centroides_2024_moderada <- st_centroid(mapa_dados_2024_moderada)
centroides_coords_2024_moderada <- st_coordinates(centroides_2024_moderada)
mapa_dados_2024_moderada$lon <- centroides_coords_2024_moderada[,1]
mapa_dados_2024_moderada$lat <- centroides_coords_2024_moderada[,2]

mapa_2024_moderada <- ggplot(mapa_dados_2024_moderada) +
  geom_sf(aes(fill = porcentagem), color = "white", size = 0.8) +
  geom_text(aes(x = lon, y = lat, 
                label = paste0(name_region, "\n", round(porcentagem, 1), "%")),
            family = "Ubuntu", size = 3.5, fontface = "bold") +
  scale_fill_gradient(low = "#FFE5CC", high = "#FF6600",
                      name = "Porcentagem (%)",
                      breaks = seq(0, 20, by = 1)) +
  labs(subtitle = "2024") +
  theme_void(base_family = "Ubuntu") +
  theme(
    plot.subtitle = element_text(face = "bold", family = "Ubuntu", 
                                 size = 13, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.background = element_rect(fill = "#D4C4B0", color = NA),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  )

# Mapa 2023
dados_mapa_2023_moderada <- dados_final %>%
  filter(regioes != "Brasil", 
         ano == 2023,
         seguranca_alimentar == "Com insegurança alimentar moderada") %>%
  mutate(name_region = case_when(
    regioes == "Região Norte" ~ "Norte",
    regioes == "Região Nordeste" ~ "Nordeste",
    regioes == "Região Sudeste" ~ "Sudeste",
    regioes == "Região Sul" ~ "Sul",
    regioes == "Região Centro-Oeste" ~ "Centro Oeste",
    grepl("Centro", regioes, ignore.case = TRUE) ~ "Centro Oeste",
    TRUE ~ regioes
  ))

mapa_dados_2023_moderada <- regioes_geo %>%
  left_join(dados_mapa_2023_moderada, by = "name_region")

centroides_2023_moderada <- st_centroid(mapa_dados_2023_moderada)
centroides_coords_2023_moderada <- st_coordinates(centroides_2023_moderada)
mapa_dados_2023_moderada$lon <- centroides_coords_2023_moderada[,1]
mapa_dados_2023_moderada$lat <- centroides_coords_2023_moderada[,2]

mapa_2023_moderada <- ggplot(mapa_dados_2023_moderada) +
  geom_sf(aes(fill = porcentagem), color = "white", size = 0.8) +
  geom_text(aes(x = lon, y = lat, 
                label = paste0(name_region, "\n", round(porcentagem, 1), "%")),
            family = "Ubuntu", size = 3.5, fontface = "bold") +
  scale_fill_gradient(low = "#FFE5CC", high = "#FF6600",
                      name = "Porcentagem (%)",
                      breaks = seq(0, 20, by = 1)) +
  labs(subtitle = "2023") +
  theme_void(base_family = "Ubuntu") +
  theme(
    plot.subtitle = element_text(face = "bold", family = "Ubuntu", 
                                 size = 13, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.background = element_rect(fill = "#D4C4B0", color = NA),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  )

# Combinando os mapas
mapa_moderada_final <- mapa_2024_moderada + mapa_2023_moderada +
  plot_annotation(
    title = "Proporção de pessoas em insegurança alimentar moderada por Região do Brasil",
    subtitle = "Comparação entre 2023 e 2024",
    caption = "Fonte: PNAD/SIDRA (Tabela 9751) | Autor: João Pedro",
    theme = theme(
      plot.title = element_text(face = "bold", family = "Ubuntu", 
                                size = 16, hjust = 0.5),
      plot.subtitle = element_text(face = "italic", family = "Ubuntu", 
                                   size = 13, hjust = 0.5),
      plot.caption = element_text(face = "italic", family = "Ubuntu", size = 12),
      plot.background = element_rect(fill = "#D4C4B0", color = NA)
    )
  )

print(mapa_moderada_final)


# ========== MAPA: Com insegurança alimentar leve ==========

# Mapa 2024
dados_mapa_2024_leve <- dados_final %>%
  filter(regioes != "Brasil", 
         ano == 2024,
         seguranca_alimentar == "Com insegurança alimentar leve") %>%
  mutate(name_region = case_when(
    regioes == "Região Norte" ~ "Norte",
    regioes == "Região Nordeste" ~ "Nordeste",
    regioes == "Região Sudeste" ~ "Sudeste",
    regioes == "Região Sul" ~ "Sul",
    regioes == "Região Centro-Oeste" ~ "Centro Oeste",
    grepl("Centro", regioes, ignore.case = TRUE) ~ "Centro Oeste",
    TRUE ~ regioes
  ))

mapa_dados_2024_leve <- regioes_geo %>%
  left_join(dados_mapa_2024_leve, by = "name_region")

centroides_2024_leve <- st_centroid(mapa_dados_2024_leve)
centroides_coords_2024_leve <- st_coordinates(centroides_2024_leve)
mapa_dados_2024_leve$lon <- centroides_coords_2024_leve[,1]
mapa_dados_2024_leve$lat <- centroides_coords_2024_leve[,2]

mapa_2024_leve <- ggplot(mapa_dados_2024_leve) +
  geom_sf(aes(fill = porcentagem), color = "white", size = 0.8) +
  geom_text(aes(x = lon, y = lat, 
                label = paste0(name_region, "\n", round(porcentagem, 1), "%")),
            family = "Ubuntu", size = 3.5, fontface = "bold") +
  scale_fill_gradient(low = "#FFF5E6", high = "#FFB366",
                      name = "Porcentagem (%)",
                      breaks = seq(0, 30, by = 1)) +
  labs(subtitle = "2024") +
  theme_void(base_family = "Ubuntu") +
  theme(
    plot.subtitle = element_text(face = "bold", family = "Ubuntu", 
                                 size = 13, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.background = element_rect(fill = "#D4C4B0", color = NA),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  )

# Mapa 2023
dados_mapa_2023_leve <- dados_final %>%
  filter(regioes != "Brasil", 
         ano == 2023,
         seguranca_alimentar == "Com insegurança alimentar leve") %>%
  mutate(name_region = case_when(
    regioes == "Região Norte" ~ "Norte",
    regioes == "Região Nordeste" ~ "Nordeste",
    regioes == "Região Sudeste" ~ "Sudeste",
    regioes == "Região Sul" ~ "Sul",
    regioes == "Região Centro-Oeste" ~ "Centro Oeste",
    grepl("Centro", regioes, ignore.case = TRUE) ~ "Centro Oeste",
    TRUE ~ regioes
  ))

mapa_dados_2023_leve <- regioes_geo %>%
  left_join(dados_mapa_2023_leve, by = "name_region")

centroides_2023_leve <- st_centroid(mapa_dados_2023_leve)
centroides_coords_2023_leve <- st_coordinates(centroides_2023_leve)
mapa_dados_2023_leve$lon <- centroides_coords_2023_leve[,1]
mapa_dados_2023_leve$lat <- centroides_coords_2023_leve[,2]

mapa_2023_leve <- ggplot(mapa_dados_2023_leve) +
  geom_sf(aes(fill = porcentagem), color = "white", size = 0.8) +
  geom_text(aes(x = lon, y = lat, 
                label = paste0(name_region, "\n", round(porcentagem, 1), "%")),
            family = "Ubuntu", size = 3.5, fontface = "bold") +
  scale_fill_gradient(low = "#FFF5E6", high = "#FFB366",
                      name = "Porcentagem (%)",
                      breaks = seq(0, 30, by = 1)) +
  labs(subtitle = "2023") +
  theme_void(base_family = "Ubuntu") +
  theme(
    plot.subtitle = element_text(face = "bold", family = "Ubuntu", 
                                 size = 13, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.background = element_rect(fill = "#D4C4B0", color = NA),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  )

# Combinando os mapas
mapa_leve_final <- mapa_2024_leve + mapa_2023_leve +
  plot_annotation(
    title = "Proporção de pessoas em insegurança alimentar leve por Região do Brasil",
    subtitle = "Comparação entre 2023 e 2024",
    caption = "Fonte: PNAD/SIDRA (Tabela 9751) | Autor: João Pedro",
    theme = theme(
      plot.title = element_text(face = "bold", family = "Ubuntu", 
                                size = 16, hjust = 0.5),
      plot.subtitle = element_text(face = "italic", family = "Ubuntu", 
                                   size = 13, hjust = 0.5),
      plot.caption = element_text(face = "italic", family = "Ubuntu", size = 12),
      plot.background = element_rect(fill = "#D4C4B0", color = NA)
    )
  )

print(mapa_leve_final)


# ========== MAPA: Com segurança alimentar ==========

# Mapa 2024
dados_mapa_2024_seguranca <- dados_final %>%
  filter(regioes != "Brasil", 
         ano == 2024,
         seguranca_alimentar == "Com segurança alimentar") %>%
  mutate(name_region = case_when(
    regioes == "Região Norte" ~ "Norte",
    regioes == "Região Nordeste" ~ "Nordeste",
    regioes == "Região Sudeste" ~ "Sudeste",
    regioes == "Região Sul" ~ "Sul",
    regioes == "Região Centro-Oeste" ~ "Centro Oeste",
    grepl("Centro", regioes, ignore.case = TRUE) ~ "Centro Oeste",
    TRUE ~ regioes
  ))

mapa_dados_2024_seguranca <- regioes_geo %>%
  left_join(dados_mapa_2024_seguranca, by = "name_region")

centroides_2024_seguranca <- st_centroid(mapa_dados_2024_seguranca)
centroides_coords_2024_seguranca <- st_coordinates(centroides_2024_seguranca)
mapa_dados_2024_seguranca$lon <- centroides_coords_2024_seguranca[,1]
mapa_dados_2024_seguranca$lat <- centroides_coords_2024_seguranca[,2]

mapa_2024_seguranca <- ggplot(mapa_dados_2024_seguranca) +
  geom_sf(aes(fill = porcentagem), color = "white", size = 0.8) +
  geom_text(aes(x = lon, y = lat, 
                label = paste0(name_region, "\n", round(porcentagem, 1), "%")),
            family = "Ubuntu", size = 3.5, fontface = "bold") +
  scale_fill_gradient(low = "#E6F7E6", high = "#2D882D",
                      name = "Porcentagem (%)",
                      breaks = seq(0, 100, by = 10)) + 
  labs(subtitle = "2024") +
  theme_void(base_family = "Ubuntu") +
  theme(
    plot.subtitle = element_text(face = "bold", family = "Ubuntu", 
                                 size = 13, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.background = element_rect(fill = "#D4C4B0", color = NA),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  )

# Mapa 2023
dados_mapa_2023_seguranca <- dados_final %>%
  filter(regioes != "Brasil", 
         ano == 2023,
         seguranca_alimentar == "Com segurança alimentar") %>%
  mutate(name_region = case_when(
    regioes == "Região Norte" ~ "Norte",
    regioes == "Região Nordeste" ~ "Nordeste",
    regioes == "Região Sudeste" ~ "Sudeste",
    regioes == "Região Sul" ~ "Sul",
    regioes == "Região Centro-Oeste" ~ "Centro Oeste",
    grepl("Centro", regioes, ignore.case = TRUE) ~ "Centro Oeste",
    TRUE ~ regioes
  ))

mapa_dados_2023_seguranca <- regioes_geo %>%
  left_join(dados_mapa_2023_seguranca, by = "name_region")

centroides_2023_seguranca <- st_centroid(mapa_dados_2023_seguranca)
centroides_coords_2023_seguranca <- st_coordinates(centroides_2023_seguranca)
mapa_dados_2023_seguranca$lon <- centroides_coords_2023_seguranca[,1]
mapa_dados_2023_seguranca$lat <- centroides_coords_2023_seguranca[,2]

mapa_2023_seguranca <- ggplot(mapa_dados_2023_seguranca) +
  geom_sf(aes(fill = porcentagem), color = "white", size = 0.8) +
  geom_text(aes(x = lon, y = lat, 
                label = paste0(name_region, "\n", round(porcentagem, 1), "%")),
            family = "Ubuntu", size = 3.5, fontface = "bold") +
  scale_fill_gradient(low = "#E6F7E6", high = "#2D882D",
                      name = "Porcentagem (%)",
                      breaks = seq(0, 100, by = 10)) + 
  labs(subtitle = "2023") +
  theme_void(base_family = "Ubuntu") +
  theme(
    plot.subtitle = element_text(face = "bold", family = "Ubuntu", 
                                 size = 13, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.background = element_rect(fill = "#D4C4B0", color = NA),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  )

# Combinando os mapas
mapa_seguranca_final <- mapa_2024_seguranca + mapa_2023_seguranca +
  plot_annotation(
    title = "Proporção de pessoas em segurança alimentar por Região do Brasil",
    subtitle = "Comparação entre 2023 e 2024",
    caption = "Fonte: PNAD/SIDRA (Tabela 9751) | Autor: João Pedro",
    theme = theme(
      plot.title = element_text(face = "bold", family = "Ubuntu", 
                                size = 16, hjust = 0.5),
      plot.subtitle = element_text(face = "italic", family = "Ubuntu", 
                                   size = 13, hjust = 0.5),
      plot.caption = element_text(face = "italic", family = "Ubuntu", size = 12),
      plot.background = element_rect(fill = "#D4C4B0", color = NA)
    )
  )

print(mapa_seguranca_final)


# ----- Criação das tabelas
library(dplyr)
library(scales)
library(tidyverse)

# 1. FILTRAR E SELECIONAR OS DADOS
inseguranca_por_regiao_2024 <- dados_final %>%
  filter(ano == 2024,
         seguranca_alimentar == "Com insegurança alimentar") %>%
  # Selecionamos as colunas relevantes
  select(regioes, total_moradores) %>%
  # Adicionamos a coluna em números absolutos
  mutate(total_pessoas = total_moradores * 1000)

# 2. FORMATAR E MOSTRAR O RESULTADO (Usando 'for loop' para evitar erro de pwalk)
cat("Pessoas com 'Com Insegurança Alimentar' por Região (2024)\n")
cat("------------------------------------------------------\n")

# Itera sobre o número de linhas da tabela
for (i in 1:nrow(inseguranca_por_regiao_2024)) {
  
  # Acessa os valores da linha 'i' pelo nome da coluna ($)
  regiao_val <- inseguranca_por_regiao_2024$regioes[i]
  milhares_val <- inseguranca_por_regiao_2024$total_moradores[i]
  pessoas_val <- inseguranca_por_regiao_2024$total_pessoas[i]
  
  cat("Região:", regiao_val, "\n")
  cat("  Em Milhares:", format(milhares_val, big.mark = ".", decimal.mark = ","), "\n")
  cat("  Pessoas (Absoluto):", format(pessoas_val, big.mark = ".", decimal.mark = ","), "\n")
  cat("------------------------------------------------------\n")
}

# ------ Tabela 1 Corrigida (Pessoas vivendo em algum grau de insegurança alimentar por região - 2024)
library(dplyr)
library(knitr)
library(kableExtra)
library(scales)

# 1. PREPARAÇÃO DA BASE (Regiões Individuais)
# A base 'inseguranca_por_regiao_2024' deve estar definida no seu ambiente
tabela_abnt_dados_regioes <- inseguranca_por_regiao_2024 %>%
  # Remove a linha "Brasil"
  filter(regioes != "Brasil") %>%
  
  # Renomeia as colunas
  rename("Região" = regioes,
         "Moradores em domicílios (mil unidades)" = total_moradores,
         "Moradores (absoluto)" = total_pessoas)

# 2. CALCULAR A LINHA DE TOTAL (Soma das Regiões)
total_row_data <- tabela_abnt_dados_regioes %>%
  # Sumariza todas as colunas numéricas
  summarise(
    "Moradores em domicílios (mil unidades)" = sum(`Moradores em domicílios (mil unidades)`),
    "Moradores (absoluto)" = sum(`Moradores (absoluto)`)
  ) %>%
  # Cria a coluna de texto para a Região
  mutate(Região = "Total (Brasil)") %>%
  # Reorganiza as colunas
  select(Região, everything())

# 3. FORMATAR E COMBINAR OS DADOS (Regiões + Total)
tabela_abnt_final_dados <- tabela_abnt_dados_regioes %>%
  # Adiciona a linha de total
  bind_rows(total_row_data) %>%
  
  # Aplica a formatação de números
  mutate(`Moradores em domicílios (mil unidades)` = number(`Moradores em domicílios (mil unidades)`, big.mark = ".", decimal.mark = ",", accuracy = 0.001),
         `Moradores (absoluto)` = number(`Moradores (absoluto)`, big.mark = ".", decimal.mark = ",", accuracy = 1))

# 4. GERAÇÃO DA TABELA (Formato ABNT com Ajuste de Cor e Fonte)
n_rows_tabela_1 <- nrow(tabela_abnt_final_dados)

tabela_final_abnt <- tabela_abnt_final_dados %>%
  kable(
    # Aplica a cor preta no TÍTULO (caption)
    caption = '<span style="color: black;">Tabela 1 – Pessoas vivendo em algum grau de insegurança alimentar por região do Brasil (2024)</span>',
    format = "html", 
    align = "l" 
  ) %>%
  kable_styling(
    bootstrap_options = "striped", 
    full_width = FALSE,
    latex_options = c("striped", "repeat_header")
  ) %>%
  # Adiciona linha abaixo do cabeçalho
  row_spec(0, extra_css = "border-bottom: 2px solid black;") %>% 
  # Usa variável pré-calculada e aplica hline
  row_spec(n_rows_tabela_1, bold = TRUE, hline_after = TRUE) %>%
  # ADICIONA A FONTE/NOTA (placement removido)
  add_footnote(
    label = "Fonte: PNAD/SIDRA (Tabela 9751) | Autor: João Pedro", 
    notation = "none", # Remove a marcação (a, b, c...)
    escape = FALSE # Permite o uso de caracteres especiais ou HTML (se necessário)
  )

tabela_final_abnt

# --------- Tabela 2 Corrigida (Variação de Insegurança Alimentar por Região)
# --------- Tabela 2 Corrigida (Variação de Insegurança Alimentar por Região)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(scales)

# 1. PREPARAÇÃO E CÁLCULO DA VARIAÇÃO PERCENTUAL (2023 -> 2024)
dados_variacao <- dados_final %>%
  filter(seguranca_alimentar == "Com insegurança alimentar") %>%
  select(regioes, ano, porcentagem) %>%
  pivot_wider(
    names_from = ano,
    values_from = porcentagem,
    names_prefix = "perc_"
  ) %>%
  mutate(
    variacao_percentual = ((perc_2024 - perc_2023) / perc_2023) * 100
  ) %>%
  
  # Ordem dos selects (perc_2024 antes de perc_2023)
  select(regioes, perc_2024, perc_2023, variacao_percentual) %>%
  
  mutate(regioes = factor(regioes, levels = c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"))) %>%
  arrange(regioes)

# 2. CALCULA O NÚMERO DE LINHAS ANTES DO PIPE
n_rows_data <- nrow(dados_variacao)

# 3. FORMATAÇÃO CIENTÍFICA (ABNT) E GERAÇÃO DA TABELA
tabela_abnt_reducao <- dados_variacao %>%
  
  # Formata os números para a exibição na tabela
  mutate(
    `Proporção 2024 (%)` = number(perc_2024, accuracy = 0.1, decimal.mark = ","),
    `Proporção 2023 (%)` = number(perc_2023, accuracy = 0.1, decimal.mark = ","),
    `Variação Percentual (%)` = number(variacao_percentual, accuracy = 0.1, decimal.mark = ",")
  ) %>%
  
  # Seleciona as colunas a serem exibidas na ordem final
  select(regioes, `Proporção 2024 (%)`, `Proporção 2023 (%)`, `Variação Percentual (%)`) %>%
  
  # Renomeia a coluna principal
  rename("Região" = regioes) %>%
  
  # Gera a tabela usando kable
  kable(
    # CSS para garantir cor preta
    caption = '<span style="color: black;">Tabela 2 – Variação percentual de pessoas em insegurança alimentar por região do Brasil (2023–2024)</span>',
    format = "html", # Linha limpa
    align = "l"
  ) %>%
  kable_styling(
    bootstrap_options = "striped",
    full_width = FALSE
  ) %>%
  # Adiciona bordas conforme ABNT
  row_spec(0, extra_css = "border-bottom: 2px solid black;") %>% # Linha abaixo do cabeçalho
  
  # Usa a variável numérica pré-calculada para o índice da última linha
  row_spec(n_rows_data, hline_after = TRUE) %>% # Linha abaixo da última linha de dados
  
  # Destaca a linha do Brasil (que é o índice 1)
  row_spec(1, bold = TRUE) %>%
  # ADICIONA A FONTE/NOTA
  add_footnote(
    label = "Fonte: PNAD/SIDRA (Tabela 9751) | Autor: João Pedro", 
    notation = "none" 
  )

tabela_abnt_reducao

# --------- Tabela 2 Corrigida (Variação de Insegurança Alimentar por Região)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(scales)

# 1. PREPARAÇÃO E CÁLCULO DA VARIAÇÃO PERCENTUAL (2023 -> 2024)
dados_variacao <- dados_final %>%
filter(seguranca_alimentar == "Com insegurança alimentar") %>%
  select(regioes, ano, porcentagem) %>%
  pivot_wider(names_from = ano,
              values_from = porcentagem,
              names_prefix = "perc_") %>%
  mutate(variacao_percentual = ((perc_2024 - perc_2023) / perc_2023) * 100) %>%

# Ordem dos selects (perc_2024 antes de perc_2023)
  select(regioes, perc_2024, perc_2023, variacao_percentual) %>%
  mutate(regioes = factor(regioes, levels = c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"))) %>%
  arrange(regioes)

# 2. CALCULA O NÚMERO DE LINHAS ANTES DO PIPE
n_rows_data <- nrow(dados_variacao)

# 3. FORMATAÇÃO CIENTÍFICA (ABNT) E GERAÇÃO DA TABELA
tabela_abnt_reducao <- dados_variacao %>%
# Formata os números para a exibição na tabela
  mutate(
    `Proporção 2024 (%)` = number(perc_2024, accuracy = 0.1, decimal.mark = ","),
    `Proporção 2023 (%)` = number(perc_2023, accuracy = 0.1, decimal.mark = ","),
    `Variação Percentual (%)` = number(variacao_percentual, accuracy = 0.1, decimal.mark = ",")
    ) %>%
  
# Seleciona as colunas a serem exibidas na ordem final
  select(regioes, `Proporção 2024 (%)`, `Proporção 2023 (%)`, `Variação Percentual (%)`) %>%
  
#Renomeia a coluna principal
  rename("Região" = regioes) %>%
  
# Gera a tabela usando kable
kable(
  caption = '<span style="color: black;">Tabela 2 – Variação Percentual de Pessoas em Insegurança Alimentar (IA Total) por Região do Brasil (2023–2024)</span>',
  format = "html",
  align = "l") %>%
  kable_styling(
    bootstrap_options = "striped",full_width = FALSE) %>%

# Adiciona bordas 
  row_spec(0, extra_css = "border-bottom: 2px solid black;") %>%
  
# Usa a variável numérica pré-calculada para o índice da última linha
  row_spec(n_rows_data, hline_after = TRUE) %>%
  row_spec(1, bold = TRUE) %>%
  
  add_footnote(
    label = "Fonte: PNAD/SIDRA (Tabela 9751) | Autor: João Pedro", 
    notation = "none" 
  )

tabela_abnt_reducao

# --------------- Tabela 3 Corrigida
library(dplyr) 
library(tidyr)
library(knitr)
library(kableExtra)
library(scales)

# 1. PREPARAÇÃO E CÁLCULO DA VARIAÇÃO PERCENTUAL (2023 -> 2024)
dados_niveis_variacao <- dados_final %>%
  # Filtramos apenas para o Brasil
  filter(regioes == "Brasil") %>%
  
  # Filtramos as 4 categorias mutuamente exclusivas
  filter(seguranca_alimentar %in% c("Com segurança alimentar",
                                    "Com insegurança alimentar leve",
                                    "Com insegurança alimentar moderada",
                                    "Com insegurança alimentar grave")) %>%
  
  # Selecionamos as colunas de ano e porcentagem
  select(seguranca_alimentar, ano, porcentagem) %>%
  
  # Pivotamos a tabela
  pivot_wider(
    names_from = ano,
    values_from = porcentagem,
    names_prefix = "perc_"
  ) %>%
  
  # Calcula a variação percentual
  mutate(
    variacao_percentual = ((perc_2024 - perc_2023) / perc_2023) * 100
  ) %>%
  
  # ORDEM DE COLUNAS: 2024 antes de 2023
  select(seguranca_alimentar, perc_2024, perc_2023, variacao_percentual) %>%
  
  # Renomeia as categorias para serem mais claras na tabela
  mutate(
    seguranca_alimentar = case_when(
      seguranca_alimentar == "Com segurança alimentar" ~ "Com Segurança Alimentar",
      seguranca_alimentar == "Com insegurança alimentar leve" ~ "Insegurança Alimentar Leve",
      seguranca_alimentar == "Com insegurança alimentar moderada" ~ "Insegurança Alimentar Moderada",
      seguranca_alimentar == "Com insegurança alimentar grave" ~ "Insegurança Alimentar Grave",
      TRUE ~ seguranca_alimentar
    )
  ) %>%
  
  mutate(seguranca_alimentar = factor(seguranca_alimentar,
                                      levels = c("Com Segurança Alimentar",
                                                 "Insegurança Alimentar Leve",
                                                 "Insegurança Alimentar Moderada",
                                                 "Insegurança Alimentar Grave"))) %>%
  arrange(seguranca_alimentar)


# 2. FORMATAÇÃO CIENTÍFICA (ABNT) E GERAÇÃO DA TABELA
n_rows_data <- nrow(dados_niveis_variacao)

tabela_abnt_niveis_brasil <- dados_niveis_variacao %>%
  
  # Formata os números para a exibição na tabela
  mutate(
    `Proporção 2024 (%)` = number(perc_2024, accuracy = 0.1, decimal.mark = ","),
    `Proporção 2023 (%)` = number(perc_2023, accuracy = 0.1, decimal.mark = ","),
    `Variação Percentual (%)` = number(variacao_percentual, accuracy = 0.1, decimal.mark = ",")
  ) %>%

  select(`Nível de Segurança Alimentar` = seguranca_alimentar,
         `Proporção 2024 (%)`,
         `Proporção 2023 (%)`,
         `Variação Percentual (%)`) %>%
  
  kable(
    caption = '<span style="color: black;">Tabela 3 – Variação percentual dos níveis de segurança alimentar do Brasil (2023–2024)</span>',
    format = "html",
    align = "l"
  ) %>%
  kable_styling(
    bootstrap_options = "striped",
    full_width = FALSE
  ) %>%
  # Adiciona bordas conforme ABNT
  row_spec(0, extra_css = "border-bottom: 2px solid black;") %>%
  row_spec(n_rows_data, hline_after = TRUE) %>%
  add_footnote(
    label = "Fonte: PNAD/SIDRA (Tabela 9751) | Autor: João Pedro", 
    notation = "none" 
  )

tabela_abnt_niveis_brasil


# 1. PREPARAÇÃO E CÁLCULO DA VARIAÇÃO
dados_niveis_variacao_abs <- dados_final %>%
  filter(regioes == "Brasil") %>%
  filter(seguranca_alimentar %in% c("Com segurança alimentar",
                                    "Com insegurança alimentar leve",
                                    "Com insegurança alimentar moderada",
                                    "Com insegurança alimentar grave")) %>%
  select(seguranca_alimentar, ano, total_moradores, porcentagem) %>%
  pivot_wider(
    names_from = ano,
    values_from = c(total_moradores, porcentagem),
    names_prefix = c("abs_", "perc_")
  ) %>%
  mutate(
    variacao_moradores = abs_2024 - abs_2023,
    variacao_percentual = ((abs_2024 - abs_2023) / abs_2023) * 100
  ) %>%
  select(
    seguranca_alimentar, 
    Moradores_2024 = abs_2024, 
    Moradores_2023 = abs_2023, 
    Variacao_Absoluta = variacao_moradores, 
    Variacao_Percentual = variacao_percentual
  ) %>%
  mutate(
    seguranca_alimentar = case_when(
      seguranca_alimentar == "Com segurança alimentar" ~ "Com Segurança Alimentar",
      seguranca_alimentar == "Com insegurança alimentar leve" ~ "Insegurança Alimentar Leve",
      seguranca_alimentar == "Com insegurança alimentar moderada" ~ "Insegurança Alimentar Moderada",
      seguranca_alimentar == "Com insegurança alimentar grave" ~ "Insegurança Alimentar Grave",
      TRUE ~ seguranca_alimentar
    ),
    seguranca_alimentar = factor(seguranca_alimentar,
                                 levels = c("Com Segurança Alimentar",
                                            "Insegurança Alimentar Leve",
                                            "Insegurança Alimentar Moderada",
                                            "Insegurança Alimentar Grave"))
  ) %>%
  arrange(seguranca_alimentar)

# 2. PRINT DO RESULTADO FINAL
print(dados_niveis_variacao_abs)


# --------------- Tabela 4 (Moradores, Variação Absoluta e Percentual por Nível)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(scales)

# 1. PREPARAÇÃO E CÁLCULO DA VARIAÇÃO
dados_filtrados <- dados_final %>%
  filter(regioes == "Brasil") %>%
  filter(seguranca_alimentar %in% c("Com segurança alimentar",
                                    "Com insegurança alimentar leve",
                                    "Com insegurança alimentar moderada",
                                    "Com insegurança alimentar grave")) %>%
  
  # Aplica a multiplicação por 1000 na coluna total_moradores (agora População)
  mutate(
    total_populacao = total_moradores * 1000 
  ) %>%
  select(regioes, seguranca_alimentar, ano, total_populacao, porcentagem)

# 1A. PIVOTAGEM 1: População (Valores Absolutos)
dados_populacao <- dados_filtrados %>%
  select(-porcentagem) %>%
  pivot_wider(
    names_from = ano,
    values_from = total_populacao,
    names_prefix = "abs_" 
  )

# 1B. PIVOTAGEM 2: Proporção (Valores Percentuais)
dados_proporcao <- dados_filtrados %>%
  select(-total_populacao) %>%
  pivot_wider(
    names_from = ano,
    values_from = porcentagem,
    names_prefix = "perc_"
  )

# JUNÇÃO DOS DADOS E CÁLCULO FINAL
dados_tabela_6_final <- dados_populacao %>%
  left_join(dados_proporcao, by = c("regioes", "seguranca_alimentar")) %>%
  
  # Calcula as Variações
  mutate(
    variacao_populacao = abs_2024 - abs_2023,
    variacao_percentual = ((abs_2024 - abs_2023) / abs_2023) * 100 
  ) %>%
  
  # Renomeia e ordena as categorias de forma lógica
  mutate(
    seguranca_alimentar = case_when(
      seguranca_alimentar == "Com segurança alimentar" ~ "Com Segurança Alimentar",
      seguranca_alimentar == "Com insegurança alimentar leve" ~ "Insegurança Alimentar Leve",
      seguranca_alimentar == "Com insegurança alimentar moderada" ~ "Insegurança Alimentar Moderada",
      seguranca_alimentar == "Com insegurança alimentar grave" ~ "Insegurança Alimentar Grave",
      TRUE ~ seguranca_alimentar
    ),
    seguranca_alimentar = factor(seguranca_alimentar,
                                 levels = c("Com Segurança Alimentar",
                                            "Insegurança Alimentar Leve",
                                            "Insegurança Alimentar Moderada",
                                            "Insegurança Alimentar Grave"))
  ) %>%
  arrange(seguranca_alimentar)


# Tabela 4
n_rows_data <- nrow(dados_tabela_6_final)

tabela_abnt_tabela_6_completa <- dados_tabela_6_final %>%
  
  # Formata os números para exibição
  mutate(
    `População 2024` = number(abs_2024, big.mark = ".", decimal.mark = ",", accuracy = 1),
    `População 2023` = number(abs_2023, big.mark = ".", decimal.mark = ",", accuracy = 1),
    # Variação Absoluta
    `Variação Absoluta` = number(variacao_populacao, big.mark = ".", decimal.mark = ",", accuracy = 1, style_positive = "plus"),
    # Variação Percentual
    `Variação Percentual (%)` = number(variacao_percentual, accuracy = 0.1, decimal.mark = ",", style_positive = "plus")
  ) %>%
  
  # Seleciona e renomeia as colunas na ordem final
  select(`Nível de Segurança Alimentar` = seguranca_alimentar,
         `População 2024`,
         `População 2023`,
         `Variação Absoluta`,
         `Variação Percentual (%)`) %>%
  
  kable(
    caption = '<span style="color: black;">Tabela 4 – População, Variação Absoluta e Percentual por Nível de Segurança Alimentar no Brasil (2023–2024)</span>',
    format = "html",
    align = "l"
  ) %>%
  kable_styling(
    bootstrap_options = "striped",
    full_width = FALSE
  ) %>%
  row_spec(0, extra_css = "border-bottom: 2px solid black;") %>% 
  row_spec(n_rows_data, hline_after = TRUE) %>% 

  add_footnote(
    label = "Fonte: PNAD/SIDRA (Tabela 9751) | Autor: João Pedro", 
    notation = "none" 
  )

tabela_abnt_tabela_6_completa
