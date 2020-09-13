spotreba = 50
calculate_series <- function(dta,spotreba){
  serie = (dta[,(pr)-spotreba])
  return(data.table(dta,serie))
}
