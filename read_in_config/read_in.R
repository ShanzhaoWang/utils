# for the template that I use
for (i in 1:2) {dev_config[,i] <- stringr::str_trim(dev_config[,i])} #str_trim to remove whitespaces before and after
# remove comments
indexes_comment <- which(sapply(dev_config[,1], startsWith, "#"))
if (length(indexes_comment)>0){
  dev_config <- dev_config[-indexes_comment,]
}
dev_config <- data.frame(t(dev_config), stringsAsFactors = FALSE)

names(dev_config) <- as.character(dev_config[1,])
dev_config <- dev_config[-1,]


### prompt user_name and password
    if (is.na(OS_PASSWORD)) {
      # only ask for password if not entered already
      OS_USERNAME <<- readline(prompt="OVGTB username: ")
      OS_PASSWORD <<- readline(prompt="OVGTB password: ")
    }

### read all sheets in all excel files within a folder with some name requirements
ottaid_voablock_fromation <- function(w_dir = "/Users/szw/Desktop/nanostring", folder_name) {
  data_file <- list.files(path = paste0(w_dir, "/", folder_name), pattern = ".xlsx")
  df <- tibble::data_frame(path = data_file) %>%
    dplyr::mutate(., path = paste0(w_dir, "/", folder_name, "/", data_file)) %>%
    dplyr::mutate(., sheet_name = purrr::map(path, readxl::excel_sheets)) %>%
    tidyr::unnest(.) %>%
    dplyr::mutate(., data = purrr::map2(
      path, sheet_name,
      ~readxl::read_excel(.x, .y)))
  
  # store list of data frames in df_b
  df_b <- df$data
  
  # convert df_b into a dataframe by combining all dataframes with columns "OTTA ID" and "BLOCK ID"
  df_b <- do.call("rbind", lapply(df_b, function(x) {
    if(all(c("OTTA ID", "BLOCK ID") %in% colnames(x))) {
      col_selected <- x %>% 
      dplyr::select(., c("OTTA ID", "BLOCK ID"))
      return(col_selected)
    }
  }))
  
  # filter out all TVAN2 from the file in data1 folder
  df_b <- df_b[which(stringr::str_detect(df_b$`OTTA ID`, "TVAN2")), ] %>% unique(.)
  return(df_b)
}  

