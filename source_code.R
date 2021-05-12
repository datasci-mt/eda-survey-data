library(tidyverse)


get_survey_data_dh <- function(file_path){
  #' get_survey_data_from_data_hackers
  #'
  #' @param file_path 
  #'
  #' @return tibble with data loaded from file
  
  read_csv(file = file_path)
}

get_survey_data_kg <- function(file_path){
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

get_google_trends_data <- function(file_path){
  #' get_google_trends_data
  #'
  #' @param file_path 
  #'
  #' @return tibble with data moving averages wih 6 months of google trends data
  
  data <- read_csv(file_path) %>% 
    mutate_at(
      .vars = vars(-Mes), 
      .funs = ~as.double(
                str_replace(
                  string = .,
                  pattern = "<1", 
                  replacement = "0.5"
                  )
                )
      ) %>% 
    mutate_at(
      .vars = vars(-Mes), 
      .funs = ~zoo::rollmean(
        x = ., 
        k = 6, 
        fill = c(NA, 0, NA)
        )
      ) %>% 
    pivot_longer(
      cols = -Mes, 
      names_to = "Role", 
      values_to = "Interest"
      )
  
  return(data)
}

get_role <- function(role_string) {
  #' get_role_bind_data
  #'
  #' @param role_string
  #'
  #' @return a vector of cleared role names

  dic_role <- list(
    "Outras" = "Outros", 
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
    "DBA/Database Engineer" = "Outros"
  )  
    
  
  roles <- map_chr(.x = {{ role_string }}, 
                   .f = ~ifelse(is.na(.x), NA, dic_role[[.x]]))
  
  return(roles)
}

get_salary <- function(salary_category) {
  #' get_salary
  #'
  #' @param salary_category
  #'
  #' @return a vector of averages from salary categories
  
  dic_salary <- list(
    "Menos de R$ 1.000/mês" = 500,
    "de R$ 1.001/mês a R$ 2.000/mês" = 1500,
    "de R$ 2.001/mês a R$ 3000/mês" = 2500,
    "de R$ 3.001/mês a R$ 4.000/mês" = 3500,
    "de R$ 4.001/mês a R$ 6.000/mês" = 5000,
    "de R$ 6.001/mês a R$ 8.000/mês" = 7000,  
    "de R$ 8.001/mês a R$ 12.000/mês" = 10000,
    "de R$ 12.001/mês a R$ 16.000/mês" = 14000,                               
    "de R$ 16.001/mês a R$ 20.000/mês" = 18000,
    "de R$ 20.001/mês a R$ 25.000/mês" = 22500,
    "Acima de R$ 25.001/mês" = 25000
  )  
  
  
  salaries <- map_dbl(.x = {{ salary_category }}, 
                   .f = ~ifelse(is.na(.x), NA, dic_salary[[.x]]))
  
  return(salaries)
}

get_degree <- function(degree_string) {
  #' get_degree
  #'
  #' @param degree_string
  #'
  #' @return a vector of cleared degree strings
  
  dic_degree <- list(
    "Bachelor's degree" = "Graduação",
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
    "Prefiro não informar" = "Não informado",
    "Doctoral degree" = "Doutorado/Phd",                                                  
    "Master’s degree" = "Mestrado",                                                  
    "Bachelor’s degree" = "Graduação",                                                
    "No formal education past high school" = "S/ Ed. Formal",
    "Some college/university study without earning a bachelor’s degree" = "Estudante Grad.",
    "Professional degree" = "Pós-Graduação",                                           
    "I prefer not to answer" = "Não informado"
  )  
  
  
  degree <- map_chr(.x = {{ degree_string }}, 
                    .f = ~ifelse(is.na(.x), NA, dic_degree[[.x]]))
  
  return(degree)
}

get_gender <- function(gender_string) {
  #' get_gender
  #'
  #' @param gender_string
  #'
  #' @return a vector of cleared gender strings
  
  dic_gender <- list(
    "Masculino" = "Masculino",
    "Feminino" = "Feminino",
    "Man" = "Masculino",
    "Woman" = "Feminino",
    "Prefer to self-describe" = "Auto-declarado",
    "Prefer not to say" = "Não informado",
    "Nonbinary" = "Não-binário",
    "Não informado" = "Não informado"
  )  
  
  
  gender <- map_chr(.x = {{ gender_string }}, 
                    .f = ~ifelse(is.na(.x), "Não informado", dic_gender[[.x]]))
  
  return(gender)
}

get_age <- function(age_category) {
  #' get_age
  #'
  #' @param age_category
  #'
  #' @return a vector of averages of age categories
  
  dic_age <- list(
    "35-39" = 37,
    "30-34" = 32,
    "22-24" = 23,
    "25-29" = 27,
    "18-21" = 19.5,
    "55-59" = 57,
    "50-54" = 52,
    "40-44" = 42,
    "60-69" = 64.5,
    "45-49" = 47,
    "70+" = 70
  )  
  
  
  age <- map_dbl(.x = {{ age_category }}, 
                 .f = ~ifelse(is.na(.x), NA, dic_age[[.x]]))
  
  return(age)
}

get_language <- function(language_string) {
  #' get_language
  #'
  #' @param language_string
  #'
  #' @return a vector of cleared programming languages
  
  dic_language <- list(
    "sql_" = "SQL",
    "r" = "R",
    "python" = "Python",
    "c_c++_c#" = "C/C++/C#",
    "dotnet" = "DotNet",
    "java" = "Java",
    "julia" = "Julia",
    "sas_stata" = "SAS/Stata",
    "visual_basic_vba" = "VBA",
    "scala" = "Scala",
    "matlab" = "MATLAB",
    "php" = "PHP",
    "no_listed_languages" = "Outra",
    "Python" = "Python",
    "R" = "R",
    "SQL" = "SQL",
    "C" = "C",
    "C++" = "C++",
    "Java" = "Java",
    "Javascript" = "Javascript",
    "Julia" = "Julia",
    "Swift" = "Swift",
    "Bash" = "Bash",
    "MATLAB" = "MATLAB",
    "None" = "Nenhuma",
    "Other" = "Outra"
  )  
  
  
  language <- map_chr(.x = {{ language_string }}, 
                 .f = ~ifelse(is.na(.x), NA, dic_language[[.x]]))
  
  return(language)
}
