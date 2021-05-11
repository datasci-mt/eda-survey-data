library(tidyverse)

load_obj <- function(f) {
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

dados <- bind_rows(
  load_obj('dados_2018.rda'),
  load_obj('dados_2019.rda') %>% 
    mutate(Grau.Instrução = as.numeric(Grau.Instrução))
)

cbo <- read_csv2(
  file = 'CBO2002 - Ocupacao.csv', 
  locale = locale(encoding = 'Latin1'),
  col_types = 'ic'
)

cbo_analise <- cbo %>% 
  filter(
    CODIGO %in% c(
      142330,
      142335,
      211205,
      211210,
      211215,
      212305,
      203105,
      212205,
      212210,
      212215,
      211120,
      212205,
      212215,
      212320
    )
  )

ds_data <- dados %>% 
  inner_join(cbo_analise, by = c('CBO.2002.Ocupação' = 'CODIGO')) %>% 
  mutate(
    salario_mensal = as.numeric(str_replace(`Salário.Mensal`, ",", ".")),
    tipo = ifelse(Admitidos.Desligados == 1, "Admissão", "Desligamento"),
    dataref = lubridate::ymd(paste0(`Competência.Declarada`, "01"))
  )

ds_data %>% 
  group_by(tipo, dataref) %>% 
  summarise(
    min = min(salario_mensal),
    q1 = quantile(salario_mensal,0.25),
    md = median(salario_mensal),
    q3 = quantile(salario_mensal,0.75),
    max = max(salario_mensal),
    media = mean(salario_mensal),
    desv.pad = sd(salario_mensal),
    qtd = n()
  ) %>% 
  ggplot(aes(x = dataref, y = qtd, color = tipo)) +
  geom_line() +
  labs(
    title = 'Quantidade de Admissões ou Desligamentos',
    subtitle = '2018 a 2019',
    x = 'Competência (Mês e Ano)',
    y = 'Quantidade de enventos',
    color = 'Admissão ou desligamento',
    caption = 'Fonte: CAGED'
  ) +
  theme_light()


saldo_data <- ds_data %>% 
  group_by(tipo, dataref) %>% 
  summarise(
    qtd = n()
  ) %>% 
  pivot_wider(
    names_from = tipo, 
    values_from = qtd
  ) %>% 
  mutate(saldo = Admissão  - Desligamento) 

saldo_data %>% 
  ggplot(aes(x = dataref, y = saldo)) +
  geom_line() +
  geom_hline(
    yintercept = mean(saldo_data$saldo),
    color = "gray"
  ) + 
  annotate(
    geom = "label", 
    x = as.Date('2019-11-01'), 
    y = mean(saldo_data$saldo),
    label = paste0(
      "Média: ", 
      round(mean(saldo_data$saldo), 1)
    ), 
    parse = TRUE,
    fill = 'gray'
  ) +
  labs(
    title = 'Entrada de profissionais no mercado de trabalho',
    subtitle = '2018 a 2019',
    x = 'Competência (Mês e Ano)',
    y = 'Quantidade de eventos',
    caption = 'Fonte: CAGED'
  ) +
  scale_x_date(breaks = "4 months") +
  theme_light()



