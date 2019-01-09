# list all files in the current directory
dir()
# list all files with certain name pattern
files <- list.files("*.txt")
# read all files ending with .txt and combine them as one single df
do.call("rbind",lapply(files, FUN=function(x){read.delim(x,stringsAsFactors = FALSE, sep=",")}))
