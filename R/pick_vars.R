
# Title: pick var helper fun
# Name: .pick_vars
.pick_vars <- function(df,var_names){
  select(df,!!! var_names)
}
