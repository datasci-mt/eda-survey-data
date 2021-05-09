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