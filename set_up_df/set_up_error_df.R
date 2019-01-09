setup_errordf_qc <- function() {
  return(data.frame(
    date_time = as.character("Time for data quality checking"),
    variable = as.character("Rule violated variables"),
    descripition = as.character("Word description of violations"),
    flag = as.character("QC or import"),
    stringsAsFactors = FALSE,
    row.names = NULL
  ))
}
