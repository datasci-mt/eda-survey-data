# libraries and functions used ----
source(file = "source_code.R", encoding = "UTF-8")

# Reading the data ----
df_dh <- get_survey_data_dh(
  file_path = "survey_data-hackers/datahackers-survey-2019-anonymous-responses.csv") %>% 
  mutate(
    Survey = "Data Hackers",
    Role = get_role(`('D6', 'anonymized_role')`),
    Salary = get_salary(`('P16', 'salary_range')`),
    Degree = get_degree(`('P8', 'degreee_level')`),
    Gender = get_gender(`('P2', 'gender')`),
    Age = `('P1', 'age')`
  )

df_kg <- get_survey_data_kg(
  file_path = "survey_kaggle/kaggle_survey_2020_responses.csv") %>% 
  mutate(
    Survey = "Kaggle",
    Role = get_role(Q5),
    Salary = NA,
    Degree = get_degree(Q4),
    Gender = get_gender(Q2),
    Age = get_age(Q1)
  )

# Cargos ocupados ----
df_role <- bind_rows(df_kg %>% select(Survey, Role),
                     df_dh %>% select(Survey, Role)) %>% 
  drop_na() %>% 
  filter(Role != "Outros") %>% 
  group_by(Survey, Role) %>% 
  summarise(N = n()) %>% 
  group_by(Survey) %>% 
  mutate(Proportion = N/sum(N))

ggplot(df_role) + 
  geom_col(aes(x = fct_reorder(Role, Proportion), 
               y = Proportion, 
               fill = Survey), 
           position = "dodge") + 
  scale_fill_manual(values = c("Kaggle" = "dodgerblue",
                               "Data Hackers" = "purple")) +
  labs(title = "Proporção de respostas por cargo e pesquisa",
       x = NULL, 
       y = NULL,
       fill = "Origem") + 
  coord_flip() + 
  theme_bw()

ggsave(filename = "img/cargos.png", device = "png", width = 7, height = 4, dpi = 600)

# Idade dos respondentes ----
df_age <- bind_rows(df_dh %>% select(Survey, Age, Role),
                    df_kg %>% select(Survey, Age, Role)) %>% 
  drop_na() %>% 
  filter(Role != "Outros")

df_age_avg <- df_age %>% 
  group_by(Role) %>% 
  summarise(Quartil1 = quantile(Age, 0.25),
            Media = mean(Age),
            Quartil3 = quantile(Age, 0.75))


ggplot(df_age) + 
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

ggsave(filename = "img/idade.png", device = "png", width = 5, height = 4, dpi = 600)


# Gênero dos participantes ----
df_gender <- bind_rows(df_kg %>% select(Survey, Role, Gender),
                       df_dh %>% select(Survey, Role, Gender)) %>% 
  drop_na() %>% 
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

ggsave(filename = "img/genero.png", device = "png", width = 7, height = 4, dpi = 600)

# Distribuição salarial no Data Hackers ----
df_salary <- df_dh %>% 
  select(Role, Salary) %>% 
  drop_na() %>% 
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

ggsave(filename = "img/faixa_salarial.png", device = "png", width = 5, height = 4, dpi = 600)


# Nível de Ensino por cargo ----
df_degree <- bind_rows(df_dh %>% select(Survey, Role, Degree),
                       df_kg %>% select(Survey, Role, Degree)) %>% 
  drop_na() %>% 
  filter(Role != "Outros", Degree != "Não informado") %>% 
  mutate(Degree = factor(Degree, ordered = T, 
                         levels = c("S/ Ed. Formal", "Estudante Grad.", 
                                    "Graduação", "Pós-Graduação", 
                                    "Mestrado", "Doutorado/Phd"))) %>% 
  group_by(Survey, Role, Degree) %>% 
  summarise(N = n()) %>% 
  group_by(Survey, Role) %>% 
  mutate(Proportion = N/sum(N))

ggplot(df_degree %>% filter(Survey == "Data Hackers")) + 
  geom_tile(aes(x = Role, y = Degree, fill = Proportion)) + 
  scale_fill_viridis_c() + 
  labs(title = "Distribuição de nível de formação por cargo - Data hackers",
       x = NULL,
       y = NULL, 
       fill = "Proporção") +
  coord_flip() + 
  theme_bw()

ggsave(filename = "img/formação_dh.png", device = "png", width = 8, height = 4, dpi = 600)


ggplot(df_degree %>% filter(Survey == "Kaggle")) + 
  geom_tile(aes(x = Role, y = Degree, fill = Proportion)) + 
  scale_fill_viridis_c() + 
  labs(title = "Distribuição de nível de formação por cargo - Kaggle",
       x = NULL,
       y = NULL, 
       fill = "Proporção") +
  coord_flip() + 
  theme_bw()

ggsave(filename = "img/formação_kg.png", device = "png", width = 8, height = 4, dpi = 600)

# Linguagens Usadas ----
lang_dh <- df_dh %>%
  select(Role, starts_with("('P21'")) %>% 
  drop_na() %>% 
  pivot_longer(-Role, names_to = "lang", values_to = "use") %>% 
  mutate(Survey = "Data Hackers",
         lang = str_sub(lang, 10, str_length(lang)-2))

lang_kg <- df_kg %>% 
  select(Role, starts_with("Q7")) %>% 
  pivot_longer(-Role, names_to = "lang", values_to = "use") %>% 
  filter(!is.na(Role))

match <- lang_kg %>% 
  drop_na() %>% 
  select(-Role) %>% 
  unique()

lang_kg <- lang_kg %>% 
  mutate(Survey = "Kaggle",
         lang = map_chr(lang, ~match$use[match$lang == .x]),
         use = ifelse(is.na(use), 0, 1))

df_lang <- bind_rows(lang_dh, lang_kg) %>% 
  mutate(lang = get_language(lang)) %>% 
  group_by(Survey, Role, lang) %>% 
  summarise(use = mean(use)) %>% 
  filter(Role != "Outros")


ggplot(df_lang %>% filter(Survey == "Data Hackers")) + 
  geom_tile(aes(x = fct_reorder(lang, use, mean, .desc = TRUE), 
                y = Role, 
                fill = use)) + 
  scale_fill_viridis_c(limits = c(0, 1)) + 
  labs(title = "Pergunta: 'Quais linguages você usa?' - Data Hackers",
       x = NULL, y = NULL, fill = "Proporção") +
  theme_bw()

ggsave(filename = "img/linguagens_dh.png", device = "png", width = 9, height = 4, dpi = 1200)

ggplot(df_lang %>% filter(Survey == "Kaggle")) + 
  geom_tile(aes(x = fct_reorder(lang, use, mean, .desc = TRUE), 
                y = Role, 
                fill = use)) + 
  scale_fill_viridis_c(limits = c(0, 1)) + 
  labs(title = "Pergunta: 'Quais linguages você usa?' - Kaggle",
       x = NULL, y = NULL, fill = "Proporção") +
  theme_bw()

ggsave(filename = "img/linguagens_kg.png", device = "png", width = 9, height = 4, dpi = 1200)


# Gráfico de tendência do Google ----
df_rl_br <- get_google_trends_data(
  file_path = "google_trends/roles_timeline_brazil.csv"
  )

df_rl_ww <- get_google_trends_data(
  file_path = "google_trends/roles_timeline_world.csv"
)

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

ggsave(filename = "img/trends_br.png", device = "png", width = 10, height = 4, dpi = 1200)

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

ggsave(filename = "img/trends_ww.png", device = "png", width = 10, height = 4, dpi = 1200)
