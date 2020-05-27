f_number_of_controls <- function(v_cases, v_prev){
  v_controls = v_cases*((1-v_prev)/prev)
  return(list("Benötigte Kontrollen" = v_controls))

}
