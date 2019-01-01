
#' @title Scoring Function
#' @name scores
#' @description  This function allows you to calculated z-scores and 5 level gated scores
#' @param df a dataframe for scoring
#' @param var_names a vector of variables used for each domain
#' @return a dataframe with total score for the domain, zscores and gated scores from the total score
#' @export

scores <- function(df,var_names) {

  .pick_vars(df,var_names)%>%rowSums()->tot
  zscore <- round(( tot-mean(tot,na.rm=T))/sd(tot,na.rm=T),2)
  gated <- ifelse(
    zscore <= -2,1,
    ifelse(
      zscore > -2 &
        zscore <= -1,2,
      ifelse(
        zscore > -1 &
          zscore <= 1 , 3,
        ifelse(
          zscore > 1 &
            zscore <= 2 , 4, 5
        ))))
  return(data.frame(tot,zscore,gated))
}

#' @title All Scoring Function
#' @name AllGatedScores
#' @description  This function allows you to calculated the 5 level gated scores for all reports
#' @param df a dataframe for scoring
#' @param colname column name for gated score
#' @return a dataframe with gated scores from z scores
#' @export

AllGatedScores <- function(df,colname) {

  zscore <- round(( df[[colname]]-mean(df[[colname]],na.rm=T))/sd(df[[colname]],na.rm=T),2)
  gated <- ifelse(
    zscore <= -2,1,
    ifelse(
      zscore > -2 &
        zscore <= -1.5,2,
      ifelse(
        zscore > -1.5 &
          zscore <= 1 , 3,
        ifelse(
          zscore > 1 &
            zscore <= 2 , 4, 5
        ))))
  return(data.frame(df,gated))
}

