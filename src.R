library(tidyverse)


get_survey_data_from_data_hackerz <- function(file_path){
  #' get_survey_data_from_data_hackerz
  #'
  #' @param file_path 
  #'
  #' @return tibble with data loaded from file
  
  read_csv(file = file_path)
}

get_survey_data_from_kaggle <- function(file_path){
  #' get_survey_data_from_kaggle
  #'
  #' @param file_path 
  #'
  #' @return tibble with data loaded from file
  
  cols <- read_lines(
    file = file_path, 
    n_max = 1
  ) %>% 
    str_split(",", simplify = T)
  
  read_csv(
    file = file_path, 
    skip = 2, 
    col_names = cols
  )

}

get_role_bind_data <- function(df_data_hackers, df_kaggle) {
  #' get_role_bind_data
  #'
  #' @param df_data_hackers 
  #' @param df_kaggle 
  #'
  #' @return a binded dataframe

  dic_role <- list(
    "Outras" = "Other", 
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
    "DBA/Database Engineer" = "Other"
  )  
    
  
  # Cargos ocupados
  role_kg <- tibble(
    Survey = "Kaggle",
    Role = map_chr(
      .x = df_kaggle$Q5[!is.na(df_kaggle$Q5)], 
      .f = ~dic_role[[.x]])
  )
  
  role_dh <- tibble(
    Survey = "Data Hackers",
    Role = map_chr(
      .x = df_data_hackers$`('D6', 'anonymized_role')`[!is.na(df_data_hackers$`('D6', 'anonymized_role')`)], 
      .f = ~dic_role[[.x]])
  )
  
  bind_rows(role_dh, role_kg) %>% 
    filter(Role != "Other") %>% 
    group_by(Survey, Role) %>% 
    summarise(N = n()) %>% 
    group_by(Survey) %>% 
    mutate(Proportion = N/sum(N))
  
}
