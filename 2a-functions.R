library(dplyr)
library(forcats)
library(ggplot2)
library(lubridate)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(tidyr)

find_package <- function(fun_name) {
  if (!is.na(fun_name)) {
    res <- find(fun_name)
    if (length(res) != 0) {
      return(res[1])
    }
  }
  NA
}

pkgs <- c("dplyr", "forcats", "ggplot2", "lubridate", "purrr", "readr", "stringr", "tibble", "tidyr")

# get all functions in package
ns <- lapply(pkgs, getNamespaceExports)
pkg_funs <- data.frame(library = rep(pkgs, sapply(ns, length)), fun = unlist(ns))

repo_funs_list <- readRDS("repo_funs_list.rds")
repo_funs <- bind_rows(transpose(repo_funs_list)[["result"]])

repo_funs[["pkg"]] <- map_chr(repo_funs_df$fun_name, find_package)

saveRDS(repo_funs, "repo_funs.rds")


repo_funs %>% 
  group_by(pkg, fun_name) %>% 
  summarize(n = sum(fun_freq)) %>% 
  group_by(pkg) %>% arrange(desc(n)) %>% 
  slice(1:5) %>% ggplot(aes(x = fun_name, y = n)) + geom_col() + facet_wrap(~pkg, scales="free")