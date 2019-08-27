#' Collection of skill scores 
#' 
#' 
#' @filename skill_scores.R
#' 
NULL

MSE_SkillScore = function(o,p,ref) {
  #' Calc the Skill Score with the Mean Squared Error as skill
  #' 
  #' Can be interpreted as reduction of variance 
  #' Skill of 1 is perfect; Skill of 0 no improvement
  #' 
  #' @version 0.1 2018-12 new simple fun used in BSRN clear sky study 
  #' @references http://www.cawcr.gov.au/projects/verification/
  #' @param o vector of observation
  #' @param p vector of prediction whose skill is evaluated 
  #' @param ref the reference prediction (naive, prior etc)
  #' @value Skill score of MSE
  #' @details rows with missing values are removed 
  # Skill score - Equation for equation for skill score
  # Answers the question: What is the relative improvement of the forecast over some reference forecast?
  # Range: Lower bound depends on what score is being used to compute skill and what reference forecast is used,
  # but upper bound is always 1; 0 indicates no improvement over the reference forecast. Perfect score: 1.
  # Characteristics: Implies information about the value or worth of a forecast relative to an alternative (reference) forecast.
  # In meteorology the reference forecast is usually persistence (no change from most recent observation) or climatology. 
  # The skill score can be unstable for small sample sizes. 
  # When MSE is the score used in the above expression then the resulting statistic is called the reduction of variance. 
  dtopr = data.table(o,p,ref)
  dtnonan = na.omit(dtopr) 
  dtnonan
  (MSEp = dtnonan[ , 1/.N * sum( (p -o)^2 )])
  (MSEref = dtnonan[ , 1/.N * sum( (ref -o)^2 )])
  (MSEp - MSEref)  / ( 0 - MSEref)
}


rmse <- function(obs, pred) sqrt(mean((obs-pred)^2,na.rm=TRUE))

