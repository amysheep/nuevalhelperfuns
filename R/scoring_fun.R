#' Scoring Function 
#'
#' This function allows you to calculated z-scores and 5 level gated scores
#' @param df a dataframe for scoring
#' @param var_names a vector of variables used for each domain 
#' @keywords z-scores , gated scores
#' @return a dataframe with total score for the domain, zscores and gated scores from the total score 
#' @examples
#' @export
pick_vars <- function(df,var_names){
  select(df,!!! var_names)
}

# sum of vars,z-score, gate score
scores <- function(df,var_names) {
  pick_vars(df,var_names)%>%rowSums()->tot
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
