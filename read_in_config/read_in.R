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
