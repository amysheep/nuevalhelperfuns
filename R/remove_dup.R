#' Removing Duplicated Tests Function
#'
#' This function allows you to remove students with more than one test on the same cases within same exam and only keep the first time score
#' @name rmdup
#' @param data a dataframe for scoring
#' @param examName Exam name i.e. InternalMed, PCC, Peds...
#' @keywords
#' @return a dataframe with exam name attached to it
#' @examples
#' @export
rmdup <- function(data, examName){
  out <- data%>%
    group_by(case, ID)%>%
    filter(row_number()==1)
  out$exam = examName
  return(out)
}
