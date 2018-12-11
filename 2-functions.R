library(plyr)
library(purrr)
source("requests.r")

get_funs_file <- function(src) {
  lines <- strsplit(src,"\n")
  sf <- srcfile("txt")
  try(parse(text = lines, srcfile = sf))
  df <- getParseData(sf)
  df[df$token == "SYMBOL_FUNCTION_CALL", "text"]
}

get_funs_repo <- function(repo_obj) {
  print(repo_obj$info$full_name)
  src <- character(length(repo_obj$r_files))
  i <- 1
  for (file in repo_obj$r_files) {
    tmp <- github_get(URLencode(paste0("https://raw.githubusercontent.com/", repo_obj$info$full_name, "/master/", file)))
    if (!is.null(tmp)) {
      src[i] <- tmp
      i <- i + 1
    }
  }
  
  funs <- unlist(lapply(src, get_funs_file))
  
  if(length(funs) == 0) {
    name <- NA
    freq <- NA
  } else {
    name <- unique(funs) 
    freq <- as.numeric(table(funs))  
  }
  
  data.frame(name = repo_obj$info$full_name, 
             created_at = repo_obj$info$created_at, 
             fun_name = name,
             fun_freq = freq)
}

repos <- llply(dir("cache-repo-fun", full.names = TRUE), readRDS)
names(repos) <- vapply(repos, function(x) x$info$full_name, character(1))

repo_funs <- lapply(repos, safely(get_funs_repo))
saveRDS(repo_funs, file = "repo_funs_list.rds")