install.packages("readxl")
install.packages("gtsummary")
install.packages("UpSetR")
install.packages('eulerr')
install.packages('gt')
library(readxl)
library(tidyverse)
library(tidyr)
library(lubridate)
library(janitor)
library(skimr)
library(ggplot2)
library(gtsummary)
library(UpSetR)
library(eulerr)
library(gt)

setwd("/Users/rafaellatoledoaranha/Desktop")
df_comunitaria <- read_excel("dados_bgn.xlsx")
glimpse(df_comunitaria)

## 1. PERFIL CLINICO E DEMOGRAFICO
demografia_inicial <- df_comunitaria %>% ## selecionando variaveis demograficas
  select(
    sexo, idade, setor_bacteremia, charlson, sofa, pitt, dm,
    hepatopatia, pneumopatia, doenca_cardiovasc, neurologico,
    alteracoes_tu, drc, neoplasia
  ) %>%
  mutate(
    across(
      c(
        sexo, setor_bacteremia, dm, hepatopatia, pneumopatia,
        doenca_cardiovasc, neurologico, alteracoes_tu, drc, neoplasia
      ),
      factor
    )
  )
summary(demografia_inicial)
skim(demografia_inicial)

tabela1 <- df_comunitaria %>% ## tabela com dados mais importantes
  select(idade, sexo, setor_bacteremia, charlson, sofa, pitt) %>%
  tbl_summary(
    type = list(pitt ~ "continuous"),
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  ) %>%
  bold_labels()
tabela1

comorbidades_bar <- df_comunitaria %>% ## preparando dados de comorbidades para analise
  transmute(
    `Diabetes mellitus` = if_else(dm == "S", 1, 0),
    `Doença renal crônica` = if_else(drc %in% c("Não dialítico"), 1, 0),
    `Neoplasia` = if_else(neoplasia == "Sólida", 1, 0),
    `Doença cardiovascular` = if_else(doenca_cardiovasc == "S", 1, 0),
    `Pneumopatia` = if_else(pneumopatia == "S", 1, 0),
    `Hepatopatia` = if_else(hepatopatia == "S", 1, 0),
    `Alterações de Trato Urinário` = if_else(alteracoes_tu == "S", 1, 0),
    `Doença Neurológica` = if_else(neurologico == "S", 1, 0)
  ) %>%
  as.data.frame()

comorbidades_prev <- comorbidades_bar %>%
  summarise(across(everything(), ~ mean(.x) * 100)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "comorbidade",
    values_to = "prevalencia"
  )

tabela2 <- comorbidades_prev %>% ## tabela com prevalencia de comorbidades
  summarise(across(everything(), ~ sum(.x))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Comorbidade",
    values_to = "n"
  ) %>%
  mutate(
    `%` = round(n / nrow(comorbidades_bar) * 100, 1),
    `n (%)` = paste0(n, " (", `%`, "%)")
  ) %>%
  select(Comorbidade, `n (%)`) %>%
  gt() %>%
  tab_header(
    title = md("**Perfil de Comorbidades da População Estudada**")
  ) %>%
  cols_label(
    Comorbidade = md("**Comorbidade**"),
    `n (%)` = md("**n (%)**")
  )
tabela2

ggplot(comorbidades_prev, ## grafico de barras para comorbidades
       aes(x = reorder(comorbidade, prevalencia),
           y = prevalencia)) +
  geom_col(fill = "#2C7FB8") +
  geom_text(
    aes(label = sprintf("%.1f%%", prevalencia)),
    hjust = -0.1,
    size = 4
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Prevalência (%)",
    title = "Prevalência de comorbidades na população estudada"
  ) +
  theme_minimal(base_size = 13) +
  ylim(0, max(comorbidades_prev$prevalencia) + 10)

upset( ## upset plot relacionando as comorbidades
  comorbidades_bar,
  sets = colnames(comorbidades_bar),
  order.by = "freq",
  keep.order = TRUE
)

euler_data <- list( ## euler plot relacionando as comorbidades
  "Diabetes mellitus" = which(df_comunitaria$dm == "S"),
  "Doença renal crônica" = which(df_comunitaria$drc == "Não dialítico"),
  "Doença cardiovascular" = which(df_comunitaria$doenca_cardiovasc == "S"),
  "Doença neurológica" = which(df_comunitaria$neurologico == "S"),
  "Neoplasia" = which(df_comunitaria$neoplasia == "Sólida"),
  "Pneumopatia" = which(df_comunitaria$pneumopatia == "S"),
  "Hepatopatia" = which(df_comunitaria$hepatopatia == "S"),
  "Alterações de Trato Urinário" = which(df_comunitaria$alteracoes_tu == "S")
)

euler_data <- euler_data[sapply(euler_data, length) > 0]

fit <- euler(euler_data)

cores <- c(
  "#4DAF4A",  # verde
  "#377EB8",  # azul
  "#E41A1C",  # vermelho
  "#984EA3",  # roxo
  "#FF7F00",  # laranja
  "#A65628",  # marrom
  "#F781BF",  # rosa
  "#999999"   # cinza
)

plot(
  fit,
  fills = list(
    fill = cores[1:length(euler_data)],
    alpha = 0.65
  ),
  edges = FALSE,
  labels = TRUE
)

scores <- df_comunitaria %>% ## selecionando os scores de gravidade
  select(pitt, sofa, charlson)

cor_spearman <- cor( ## a pontuacao dos pacientes nos tres scores esta relacionada??
  scores,
  method = "spearman"
)
cor_spearman