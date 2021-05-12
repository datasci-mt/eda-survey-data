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

# Transformation dictionaries
dic_role <- list("Outras" = "Outros", 
                 "Data Analyst/Analista de Dados" = "Analista de Dados", 
                 "Business Intelligence/Analista de BI" = "Analista de BI",  
                 "Desenvolvedor ou Engenheiro de Software" = "Eng. de Software", 
                 "Data Scientist/Cientista de Dados" = "Cientista de Dados", 
                 "Analista de Inteligência de Mercado" = "Outros", 
                 "Engenheiro" = "Outros", 
                 "Business Analyst/Analista de Negócios" = "Outros", 
                 "Data Engineer/Engenheiro de Dados" = "Eng. de Dados", 
                 "DBA/Administrador de Banco de Dados" = "Outros", 
                 "Analista de Marketing" = "Outros", 
                 "Estatístico" = "Estatístico", 
                 "Engenheiro de Machine Learning" = "Engenheiro de ML", 
                 "Economista" = "Outros",
                 "Student" = "Outros", 
                 "Data Engineer" = "Eng. de Dados", 
                 "Software Engineer" = "Eng. de Software", 
                 "Data Scientist" = "Cientista de Dados",
                 "Data Analyst" = "Analista de Dados",
                 "Research Scientist" = "Outros",
                 "Other" = "Outros",
                 "Currently not employed" = "Outros",   
                 "Statistician" = "Estatístico",
                 "Product/Project Manager" = "Outros",
                 "Machine Learning Engineer" = "Engenheiro de ML",
                 "Business Analyst" = "Analista de BI",
                 "DBA/Database Engineer" = "Outros")

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

dic_degree <- list("Bachelor's degree" = "Graduação",
     "Graduação/Bacharelado" = "Graduação",
     "Master's degree" = "Mestrado",
     "Mestrado" = "Mestrado",
     "Doctoral degree" = "Doutorado/Phd",
     "Doutorado ou Phd" = "Doutorado/Phd",
     "Professional degree" = "Pós-Graduação",
     "Pós-graduação" = "Pós-Graduação",
     "Some college/university study without earning a bachelor's degree" = "Estudante Grad.",
     "Estudante de Graduação" = "Estudante Grad.",
     "No formal education past high school" = "S/ Ed. Formal",
     "Não tenho graduação formal" = "S/ Ed. Formal",
     "I prefer not to answer" = "Não informado",
     "Prefiro não informar" = "Não informado")

dic_age <- list("35-39" = 37,
                "30-34" = 32,
                "22-24" = 23,
                "25-29" = 27,
                "18-21" = 19.5,
                "55-59" = 57,
                "50-54" = 52,
                "40-44" = 42,
                "60-69" = 64.5,
                "45-49" = 47,
                "70+" = 70)

dic_sex <- list("Masculino" = "Masculino",
                "Feminino" = "Feminino",
                "Man" = "Masculino",
                "Woman" = "Feminino",
                "Prefer to self-describe" = "Auto-declarado",
                "Prefer not to say" = "Não informado",
                "Nonbinary" = "Não-binário",
                "Não informado" = "Não informado")


# Cargos ocupados
role_kg <- tibble(Survey = "Kaggle",
                  Role = map_chr(.x = df_kg$Q5[!is.na(df_kg$Q5)], 
                                 .f = ~dic_role[[.x]]))

role_dh <- tibble(Survey = "Data Hackers",
                  Role = map_chr(.x = df_dh$`('D6', 'anonymized_role')`[!is.na(df_dh$`('D6', 'anonymized_role')`)], 
                                 .f = ~dic_role[[.x]]))

df_role <- bind_rows(role_dh, role_kg) %>% 
  filter(Role != "Outros") %>% 
  group_by(Survey, Role) %>% 
  summarise(N = n()) %>% 
  group_by(Survey) %>% 
  mutate(Proportion = N/sum(N))

ggplot(df_role) + 
  geom_col(aes(x = fct_reorder(Role, Proportion), y = Proportion, fill = Survey), 
           position = "dodge") + 
  scale_fill_manual(values = c("Kaggle" = "dodgerblue",
                               "Data Hackers" = "purple")) +
  labs(title = "Proporção de respostas por cargo e pesquisa",
       x = NULL, y = NULL,
       fill = "Origem") + 
  coord_flip() + 
  theme_bw()

ggsave(device = "png", filename = "cargos.png", width = 7, height = 4, dpi = 600)

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
  mutate(Role = map_chr(Role, ~dic_role[[.x]]),
         Age = map_dbl(Age, ~dic_age[[.x]]))

df_age <- bind_rows(age_dh, age_kg) %>% 
  filter(Role != "Outros")

df_age_avg <- df_age %>% 
  group_by(Role) %>% 
  summarise(Quartil1 = quantile(Age, 0.25),
            Media = mean(Age),
            Quartil3 = quantile(Age, 0.75))


ggplot(df_age %>% filter(Role != "Outros")) + 
  geom_violin(aes(x = fct_reorder(Role, Age, mean), 
                  y = Age), 
              fill = "dodgerblue", 
              color = "dodgerblue") + 
  geom_pointrange(data = df_age_avg, 
                  aes(x = Role, 
                      ymin = Quartil1, ymax = Quartil3, 
                      y = Media), color = "white") +
  labs(title = "Idade por cargo da área de dados",
       x = NULL,
       y = NULL) + 
  coord_flip() + 
  theme_bw()

ggsave(device = "png", filename = "idade.png", width = 5, height = 4, dpi = 600)


# Sexo dos participantes
gender_dh <- df_dh %>% 
  mutate(Survey = "Data Hackers",
         Role = `('D6', 'anonymized_role')`,
         Gender = `('P2', 'gender')`) %>% 
  transmute(Survey,
            Gender = ifelse(is.na(Gender), "Não informado", Gender),
            Role) %>% 
  drop_na() %>% 
  mutate(Role = map_chr(Role, ~dic_role[[.x]]),
         Gender = map_chr(Gender, ~dic_sex[[.x]]))

gender_kg <- df_kg %>% 
  transmute(Survey = "Kaggle",
            Role = Q5,
            Gender = Q2) %>% 
  drop_na() %>% 
  mutate(Role = map_chr(Role, ~dic_role[[.x]]),
         Gender = map_chr(Gender, ~dic_sex[[.x]]))

df_gender <- bind_rows(gender_dh, gender_kg) %>% 
  filter(Role != "Outros") %>% 
  group_by(Survey, Role, Gender) %>% 
  summarise(N = n()) %>% 
  group_by(Survey, Role) %>% 
  mutate(Proportion = N/sum(N))

ggplot(df_gender %>% filter(Gender == "Masculino")) + 
  geom_col(aes(x = fct_reorder(Role, Proportion), y = Proportion, fill = Survey), 
           position = "dodge") + 
  scale_fill_manual(values = c("Kaggle" = "dodgerblue",
                               "Data Hackers" = "purple")) + 
  labs(title = "Proporção de homens por cargo e origem",
       x = NULL, y = NULL) +
  coord_flip() + 
  theme_bw()

ggsave(device = "png", filename = "genero.png", width = 7, height = 4, dpi = 600)

# Gráfico - Distribuição salarial ---
df_salary <- df_dh %>% 
  select(`('D6', 'anonymized_role')`, `('P16', 'salary_range')`) %>% 
  drop_na() %>% 
  mutate(Role = map_chr(.x = `('D6', 'anonymized_role')`, 
                         .f = ~dic_role[[.x]]),
         Salary = map_dbl(.x = `('P16', 'salary_range')`, 
                           .f = ~dic_salary[[.x]])) %>% 
  filter(Role != "Outros")

df_salary_avg <- df_salary %>% 
  group_by(Role) %>% 
  summarise(Quartil1 = quantile(Salary, 0.25),
            Media = mean(Salary),
            Quartil3 = quantile(Salary, 0.75))

ggplot(df_salary) + 
  geom_violin(aes(x = fct_reorder(Role, Salary, mean), 
                  y = Salary), 
              fill = "dodgerblue", 
              color = "dodgerblue") + 
  geom_pointrange(data = df_salary_avg, 
                  aes(x = Role, 
                      ymin = Quartil1, ymax = Quartil3, 
                      y = Media), color = "white") +
  labs(title = "Salário por cargo na área de dados", x = NULL, y = NULL) +
  coord_flip() + 
  theme_bw()

ggsave(device = "png", filename = "faixa_salarial.png", width = 5, height = 4, dpi = 600)


# Nível de Ensino por cargo
degree_dh <- df_dh %>% 
  transmute(Survey = "Data Hackers",
            Role = `('D6', 'anonymized_role')`,
            Degree = `('P8', 'degreee_level')`) %>% 
  drop_na() %>% 
  mutate(Role = map_chr(Role, ~dic_role[[.x]]),
         Degree = map_chr(Degree, ~dic_degree[[.x]])) %>% 
  filter(Role != "Outros", Degree != "Não informado")

degree_kg <- df_kg %>% 
  transmute(Survey = "Kaggle",
            Role = Q5,
            Degree = str_replace(Q4, "’", "'")) %>% 
  drop_na() %>% 
  mutate(Role = map_chr(Role, ~dic_role[[.x]]),
         Degree = map_chr(Degree, ~dic_degree[[.x]])) %>% 
  filter(Role != "Outros", Degree != "Não informado")

df_degree <- bind_rows(degree_dh, degree_kg) %>% 
  mutate(Degree = factor(Degree, ordered = T, 
                         levels = c("S/ Ed. Formal", "Estudante Grad.", 
                                    "Graduação", "Pós-Graduação", 
                                    "Mestrado", "Doutorado/Phd"))) %>% 
  group_by(Role, Degree) %>% 
  summarise(N = n()) %>% 
  group_by(Role) %>% 
  mutate(Proportion = N/sum(N))

ggplot(df_degree) + 
  geom_tile(aes(x = Role, y = Degree, fill = Proportion)) + 
  scale_fill_viridis_c() + 
  labs(title = "Distribuição de nível de formação por cargo",
       x = NULL,
       y = NULL, 
       fill = "Proporção") +
  coord_flip() + 
  theme_bw()

ggsave(device = "png", filename = "formação.png", width = 8, height = 4, dpi = 1200)

df_degree_survey <- bind_rows(degree_dh, degree_kg) %>% 
  mutate(Degree = factor(Degree, ordered = T, 
                         levels = c("S/ Ed. Formal", "Estudante Grad.", 
                                    "Graduação", "Pós-Graduação", 
                                    "Mestrado", "Doutorado/Phd"))) %>% 
  group_by(Survey, Role, Degree) %>% 
  summarise(N = n()) %>% 
  group_by(Survey, Role) %>% 
  mutate(Proportion = N/sum(N))

ggplot(df_degree_survey %>% filter(Survey == "Data Hackers")) + 
  geom_tile(aes(x = Role, y = Degree, fill = Proportion)) + 
  scale_fill_viridis_c() + 
  labs(title = "Distribuição de nível de formação por cargo - Data hackers",
       x = NULL,
       y = NULL, 
       fill = "Proporção") +
  coord_flip() + 
  theme_bw()

ggsave(device = "png", filename = "formação_dh.png", width = 8, height = 4, dpi = 600)


ggplot(df_degree_survey %>% filter(Survey == "Kaggle")) + 
  geom_tile(aes(x = Role, y = Degree, fill = Proportion)) + 
  scale_fill_viridis_c() + 
  labs(title = "Distribuição de nível de formação por cargo - Kaggle",
       x = NULL,
       y = NULL, 
       fill = "Proporção") +
  coord_flip() + 
  theme_bw()

ggsave(device = "png", filename = "formação_kg.png", width = 8, height = 4, dpi = 600)


# Gráfico de tendência do Google
df_rl_br <- read_csv("roles_timeline_brazil.csv") %>% 
  mutate_at(vars(-Mes), ~as.double(str_replace(.,"<1", "0.5"))) %>% 
  mutate_at(vars(-Mes), ~zoo::rollmean(., 6, c(NA, 0, NA))) %>% 
  pivot_longer(-Mes, names_to = "Role", values_to = "Interest")

df_rl_ww <- read_csv("roles_timeline_world.csv") %>% 
  mutate_at(vars(-Mes), ~as.double(str_replace(.,"<1", "0.5"))) %>% 
  mutate_at(vars(-Mes), ~zoo::rollmean(., 6, fill = c(NA, 0, NA))) %>% 
  pivot_longer(-Mes, names_to = "Role", values_to = "Interest")


ggplot(df_rl_br) + 
  geom_line(aes(x = Mes, y = Interest, 
                group = Role, color = Role), size = 0.5) + 
  scale_x_discrete(breaks = paste0(2010:2021, "-01"), 
                   labels = 2010:2021) + 
  scale_color_manual(values = c("Data Scientist" = "seagreen",
                                "Data Analyst" = "dodgerblue",
                                "Data Engineer" = "orange",
                                "ML Engineer" = "gray",
                                "Business Analyst" = "firebrick")) + 
  labs(title = "Google Trends - Brasil", color = "Cargo", 
       x = NULL, y = "Interesse") +
  theme_bw()

ggsave(device = "png", filename = "trends_br.png", width = 10, height = 4, dpi = 1200)

ggplot(df_rl_ww) + 
  geom_line(aes(x = Mes, y = Interest, 
                group = Role, color = Role), size = 0.5) + 
  scale_x_discrete(breaks = paste0(2010:2021, "-01"), 
                   labels = 2010:2021) + 
  scale_color_manual(values = c("Data Scientist" = "seagreen",
                                "Data Analyst" = "dodgerblue",
                                "Data Engineer" = "orange",
                                "ML Engineer" = "gray",
                                "Business Analyst" = "firebrick")) + 
  labs(title = "Google Trends - Mundo", color = "Cargo", 
       x = NULL, y = "Interesse") +
  theme_bw()

ggsave(device = "png", filename = "trends_ww.png", width = 10, height = 4, dpi = 1200)
