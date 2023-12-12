#' Check Observations
#' 
#' @description Checks if you have the expected number of observations by group. Default behavior
#' assumes you are considering date data and allows you to set the expected frequency (ex: daily or
#' weekly). TODO: allow behavior to check number of observations by arbitrary groups (example use
#' case: checking number of followups for a longitudinal study or number of households per area).
#' todo: option to complete if obs are missing
#'
#'
check_observations <- function(df, groups = NULL, frequency = 'weekly', 
                               n_groups = NULL, n_observations = NULL) {
  df
}





#df <- df_new

#df %>%
  #count(region, district, year) %>%
  #arrange(year) %>%
  ##filter(n < 52) %>%
  #pivot_wider(names_from = year,
              #values_from = n) %>%
  #as.data.frame()


#df %>% 
  #filter(year == 2017) %>%
  #select(district) %>%
  #distinct() %>%
  #arrange() %>%
  #rio::export('2017_names.csv')
  ##n_distinct() 

#df %>%
  #filter(year == 2013, 
         #district == 'Filingue') %>%
  #mutate(week = lubridate::isoweek(date)) %>%
  #as.data.frame()
