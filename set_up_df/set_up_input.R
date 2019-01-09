setup_inputdf_qc <- function(input_file, sep= ",", 
 # Here, we can use a c() function instead of another df to do the name mapping
 colnames_map=c(
  "phn"="phn",
  "last_name"="surname",
  "first_name"="fst_name",
  "birth_date"="birth_date",
  "death_date"="death_date",
  "visit_date"="diagnosis_date",
  "site"="site",
  "site_desc"="site_desc",
  "laterality"="laterality",
  "laterality_desc"="laterality_desc",
  "hist1"="hist1",
  "hist1_desc"="hist1_desc",
  "pat_status"="pat_status",
  "death_cause_original"="death_cause_original",
  "death_cause_orig_desc"="death_cause_orig_desc",
  "last_attended_appt"="last_attended_appt",
  "grade"="grade",
  "grade_desc"="grade_desc",
  "oth_clin_surg_stg"="oth_clin_surg_stg",
  "oth_clin_surg_stg_desc"="oth_clin_surg_stg_desc",
  "oth_clin_surg_sys_desc"="oth_clin_surg_sys_desc"
  )) 
  {
  dat <- read.delim(file = input_file, sep = sep, header = TRUE, stringsAsFactors = FALSE)

  # do some column names re-ordering
  vars_to_read <- colnames_map
  
  # The following checks if all names in the name mapping are indeed in the input file
  assertthat::assert_that(sum(vars_to_read%in%names(dat))==length(vars_to_read))
  
  dat <- dat[,vars_to_read]

  standardized_colnames <- names(colnames_map) # read all names on the left, which are the standardized names
  
  dat <- dat %>% # input_file is supposed to be .txt
    dplyr::select(vars_to_read) %>% # using name from template "q16_copy.txt", choosing diagnosis_date as visit_date
    magrittr::set_colnames(.,standardized_colnames) %>%
    dplyr::mutate_if(is.factor, as.character) %>% naniar::replace_with_na_all(condition = ~.x %in% c("", " ", NULL)) %>% 
    as.data.frame(.) %>% unique(.) # replace empty/nonsense stuff with NA

  # since we are not sure about which date format the incoming file is using so we decide to grab all dates and check their format
  
  # select all 'xx_date' columns and unify their formats to "yyyy-mm-dd"
  date_col <- colnames(dat)[grep("date|last_attended_appt", colnames(dat))]
  all_dates <- unlist(dat[, date_col], use.names = FALSE)[!is.na(unlist(dat[, date_col], use.names = FALSE))] # obtain a vector of all non-NA dates

  grab_twodigits_in_dates <- function(x) as.numeric(strsplit(x,"/")[[1]][1])
  if (any(sapply(all_dates, grab_twodigits_in_dates) > 12)) { # it means date format is "dd/mm/yyyy"
    for (i in 1:length(date_col)) {
      dat[, date_col[i]] <- as.Date(dat[, date_col[i]], "%d/%m/%Y")
    }
  } else { # it means date format is "mm/dd/yyyy"
    for (i in 1:length(date_col)) {
      dat[, date_col[i]] <- as.Date(dat[, date_col[i]], "%m/%d/%Y")
    }
  }

  if (is.character(dat$phn)) { # if phn is in the form character(9XXX XXX XXX), change it to numeric(9XXXXXXXXX) so that it can be put into search_patient() as well as update_deathdate()
    remove_space <- function(x) paste(strsplit(stringr::str_trim(x),"[[:space:]]")[[1]],collapse="")
    dat$phn <- sapply(dat$phn,remove_space)
  }
  
  # replace values in pat_status if available
  # for registry file, pat_status won't be available ... 
  if ("pat_status" %in% names(dat)) {
    dat$pat_status[dat$pat_status == "A"] <- "Alive"
    dat$pat_status[dat$pat_status == "D"] <- "Dead"
  } else {
    # figure out pat_stat from death_date
    dat$pat_status <- sapply(dat$death_date,function(x){
      if (is.na(x)) { # see above, dat$death_date is of class Date
        return("Alive")
      } else {
        return("Dead")
      }
    })
  }
  
  
  return(dat)
}
