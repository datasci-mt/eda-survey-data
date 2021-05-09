# libraries used
library(tidyverse)


# Reading the data
df_dh <- read_csv(
  file = "survey_data-hackers/datahackers-survey-2019-anonymous-responses.csv"
)

cols <- read_lines(
  file = "survey_kaggle/kaggle_survey_2020_responses.csv", 
  n_max = 1
  ) %>% 
  str_split(",", simplify = T)

df_kg <- read_csv(
  file = "survey_kaggle/kaggle_survey_2020_responses.csv", 
  skip = 2, 
  col_names = cols
)

# Quantidade de respondentes
nrow(df_dh)
nrow(df_kg)


# Respondents by role
table(df_dh$`('D6', 'anonymized_role')`)
table(df_kg$Q5)

# Transformation dictionaries
dic_role <- list("Outras" = "Other", 
                 "Data Analyst/Analista de Dados" = "Data Analyst", 
                 "Business Intelligence/Analista de BI" = "Business Analyst",  
                 "Desenvolvedor ou Engenheiro de Software" = "Software Engineer", 
                 "Data Scientist/Cientista de Dados" = "Data Scientist", 
                 "Analista de Inteligência de Mercado" = "Other", 
                 "Engenheiro" = "Other", 
                 "Business Analyst/Analista de Negócios" = "Other", 
                 "Data Engineer/Engenheiro de Dados" = "Data Engineer", 
                 "DBA/Administrador de Banco de Dados" = "Other", 
                 "Analista de Marketing" = "Other", 
                 "Estatístico" = "Statistician", 
                 "Engenheiro de Machine Learning" = "ML Engineer", 
                 "Economista" = "Other",
                 "Student" = "Other", 
                 "Data Engineer" = "Data Engineer", 
                 "Software Engineer" = "Software Engineer", 
                 "Data Scientist" = "Data Scientist",
                 "Data Analyst" = "Data Analyst",
                 "Research Scientist" = "Other",
                 "Other" = "Other",
                 "Currently not employed" = "Other",   
                 "Statistician" = "Statistician",
                 "Product/Project Manager" = "Other",
                 "Machine Learning Engineer" = "ML Engineer",
                 "Business Analyst" = "Business Analyst",
                 "DBA/Database Engineer" = "Other")

dic_salary <- list("Menos de R$ 1.000/mês" = 500,
                   "de R$ 1.001/mês a R$ 2.000/mês" = 1500,
                   "de R$ 2.001/mês a R$ 3000/mês" = 2500,
                   "de R$ 3.001/mês a R$ 4.000/mês" = 3500,
                   "de R$ 4.001/mês a R$ 6.000/mês" = 5000,
                   "de R$ 6.001/mês a R$ 8.000/mês" = 7000,  
                   "de R$ 8.001/mês a R$ 12.000/mês" = 10000,
                   "de R$ 12.001/mês a R$ 16.000/mês" = 14000,                               
                   "de R$ 16.001/mês a R$ 20.000/mês" = 18000,
                   "de R$ 20.001/mês a R$ 25.000/mês" = 22500,
                   "Acima de R$ 25.001/mês" = 25000)

dic_degree <- list("Bachelor's degree" = "Bachelor's",
     "Graduação/Bacharelado" = "Bachelor's",
     "Master's degree" = "Master's",
     "Mestrado" = "Master's",
     "Doctoral degree" = "Doc./Phd",
     "Doutorado ou Phd" = "Doc./Phd",
     "Professional degree" = "Professional",
     "Pós-graduação" = "Professional",
     "Some college/university study without earning a bachelor's degree" = "Grad. Student",
     "Estudante de Graduação" = "Grad. Student",
     "No formal education past high school" = "No formal ed.",
     "Não tenho graduação formal" = "No formal ed.",
     "I prefer not to answer" = "Not informed",
     "Prefiro não informar" = "Not informed")

# Gráfico - Distribuição salarial
df_salary <- df_dh %>% 
  select(`('D6', 'anonymized_role')`, `('P16', 'salary_range')`) %>% 
  drop_na() %>% 
  mutate(Cargo = map_chr(.x = `('D6', 'anonymized_role')`, 
                         .f = ~dic_role[[.x]]),
         Gargo = as.factor(Cargo),
         Salario = map_dbl(.x = `('P16', 'salary_range')`, 
                           .f = ~dic_salary[[.x]]))


df_salary_avg <- df_salary %>% 
  group_by(Cargo) %>% 
  summarise(Quartil1 = quantile(Salario, 0.25),
            Media = mean(Salario),
            Quartil3 = quantile(Salario, 0.75))

df_salary$Cargo <- fct_reorder(df_salary$Cargo, df_salary$Salario, mean)
df_salary$Cargo <- fct_reorder(df_salary$Cargo, df_salary$Salario, mean)

ggplot(df_salary) + 
  geom_violin(aes(x = Cargo, 
                  y = Salario), 
              fill = "dodgerblue", 
              color = "dodgerblue") + 
  geom_pointrange(data = df_salary_avg, 
                  aes(x = Cargo, 
                      ymin = Quartil1, ymax = Quartil3, 
                      y = Media), color = "white") +
  coord_flip() + 
  theme_bw()

ggsave(device = "png", filename = "faixa_salarial.png", width = 5, height = 4, dpi = 600)

# Cargos ocupados
role_kg <- tibble(Survey = "Kaggle",
                  Role = map_chr(.x = df_kg$Q5[!is.na(df_kg$Q5)], 
                                 .f = ~dic_role[[.x]]))
                  
role_dh <- tibble(Survey = "Data Hackers",
                  Role = map_chr(.x = df_dh$`('D6', 'anonymized_role')`[!is.na(df_dh$`('D6', 'anonymized_role')`)], 
                                 .f = ~dic_role[[.x]]))

df_role <- bind_rows(role_dh, role_kg) %>% 
  filter(Role != "Other") %>% 
  group_by(Survey, Role) %>% 
  summarise(N = n()) %>% 
  group_by(Survey) %>% 
  mutate(Proportion = N/sum(N))

ggplot(df_role) + 
  geom_col(aes(x = Role, y = Proportion, fill = Survey), 
           position = "dodge") + 
  scale_fill_manual(values = c("Kaggle" = "dodgerblue",
                               "Data Hackers" = "purple")) +
  coord_flip() + 
  theme_bw()

ggsave(device = "png", filename = "cargos.png", width = 5, height = 4, dpi = 600)


# Idade dos respondentes
age_dh <- df_dh %>% 
  transmute(Survey = "Data Hackers",
            Age = `('P1', 'age')`,
            Role = `('D6', 'anonymized_role')`) %>% 
  drop_na() %>% 
  mutate(Role = map_chr(Role, ~dic_role[[.x]]))

age_kg <- df_kg %>% 
  transmute(Survey = "Kaggle",
            Age = Q1,
            Role = Q5) %>% 
  drop_na() %>% 
  mutate(Role = map_chr(Role, ~dic_role[[.x]]))

df_age <- bind_rows(age_dh, age_kg)


# Nível de Ensino por cargo
degree_dh <- df_dh %>% 
  transmute(Survey = "Data Hackers",
            Role = `('D6', 'anonymized_role')`,
            Degree = `('P8', 'degreee_level')`) %>% 
  drop_na() %>% 
  mutate(Role = map_chr(Role, ~dic_role[[.x]]),
         Degree = map_chr(Degree, ~dic_degree[[.x]])) %>% 
  filter(Role != "Other", Degree != "Not informed")

degree_kg <- df_kg %>% 
  transmute(Survey = "Kaggle",
            Role = Q5,
            Degree = Q4) %>% 
  drop_na() %>% 
  mutate(Role = map_chr(Role, ~dic_role[[.x]]),
         Degree = map_chr(Degree, ~dic_degree[[.x]])) %>% 
  filter(Role != "Other", Degree != "Not informed")

df_degree <- bind_rows(degree_dh, degree_kg) %>% 
  mutate(Degree = factor(Degree, ordered = T, 
                         levels = c("No formal ed.", "Grad. Student", 
                         "Bachelor's", "Master's", "Doc./Phd",
                         "Professional"))) %>% 
  group_by(Role, Degree) %>% 
  summarise(N = n()) %>% 
  group_by(Role) %>% 
  mutate(Proportion = N/sum(N))


ggplot(df_degree) + 
  geom_tile(aes(x = Role, y = Degree, fill = Proportion)) + 
  scale_fill_viridis_c() + 
  coord_flip() + 
  theme_bw()

ggsave(device = "png", filename = "formação.png", width = 5, height = 4, dpi = 600)

df_degree_survey <- bind_rows(degree_dh, degree_kg) %>%  
  mutate(Degree = factor(Degree, ordered = T, 
                         levels = c("No formal ed.", "Grad. Student", 
                                    "Bachelor's", "Master's", "Doc./Phd",
                                    "Professional"))) %>% 
  group_by(Survey, Role, Degree) %>% 
  summarise(N = n()) %>% 
  group_by(Survey, Role) %>% 
  mutate(Proportion = N/sum(N))


ggplot(df_degree_survey %>% filter(Survey == "Data Hackers")) + 
  geom_tile(aes(x = Role, y = Degree, fill = Proportion)) + 
  scale_fill_viridis_c() + 
  coord_flip() + 
  theme_bw()

ggsave(device = "png", filename = "formação_dh.png", width = 5, height = 4, dpi = 600)


ggplot(df_degree_survey %>% filter(Survey == "Kaggle")) + 
  geom_tile(aes(x = Role, y = Degree, fill = Proportion)) + 
  scale_fill_viridis_c() + 
  coord_flip() + 
  theme_bw()

ggsave(device = "png", filename = "formação_kg.png", width = 5, height = 4, dpi = 600)


 