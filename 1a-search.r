library(plyr)
library(lubridate)
source("requests.r")

search_repo <- function(query, page = NULL) {
  Sys.sleep(12)
  path <- paste0(base, "/search/repositories")
  qs <- list(q = paste("language:r", query), per_page = 100, page = page)
  req <- GET(path, config, query = qs)
  c <- content(req)
  
  if (req$status_code != 200) {
    browser()
    stop(c$message, call. = FALSE)
  }
  
  # Github API has a limit of 1000 search results
  n <- min(c$total_count, 1000)
  items <- c$items
  
  # Only one page of results
  if (!is.null(page) || n <= 100) {
    message(n, " repos")
    return(items)
  }
  
  # Multiple pages of results
  if (n > 100) {
    warning(n , " repos. Only first 100 returned", call. = FALSE)
  } else {
    message(n, " repos")
  }
  
  # Combine individual pages
  max_page <- ceiling(n / 100)
  results <- lapply(2:max_page, search_repo, query = query)
  c(items, unlist(results, recursive = FALSE))
}

if (!file.exists("cache/prehistory.rds")) {
  prehistory <- search_repo("created:<2011")
  saveRDS(prehistory, "cache/prehistory.rds") 
} else {
  prehistory <- readRDS("cache/prehistory.rds")
}

get_month <- function(year, month) {
  cache_path <- paste0("cache/", year, "-", month, ".rds")
  if (file.exists(cache_path)) return(readRDS(cache_path))
  
  message("Downloading repos created in ", year, "-", month)
  query <- paste0("created:", ymd(paste(year, month, 1, sep="-")), "..", ymd(paste(year, month+1, 1, sep="-")))
  results <- search_repo(query)
  
  saveRDS(results, cache_path)
  results
}

all_months <- function() {
  cur_year <- year(today())
  
  past <- expand.grid(year = 2011:(cur_year - 1), month = 1:12)
  pres <- data.frame(year = cur_year, month = 1:(month(today()) - 1))
  all <- rbind(past, pres)
  
  unlist(mlply(all, get_month), recursive = FALSE)
}


get_range <- function(start, end) {

  cache_path <- paste0("cache/", start, "_", end, ".rds")
  if (file.exists(cache_path)) return(readRDS(cache_path))
  
  message("Downloading repos from ", start, " to ", end)
  query <- paste0("created:", start, "..", end)
  results <- search_repo(query)
  
  saveRDS(results, cache_path)
  results
}


all_weeks <- function(year) {
  first <- ymd(paste0(year, "-01-01"))
  last  <- ymd(paste0(year, "-12-31"))
  start <- seq(from=first, to=last, by='week')
  end <- start - 1
  end[length(end)] <- last
  all <- data.frame(start=start[-length(start)], end=end[-1])
  unlist(mlply(all, get_range), recursive = FALSE)
}

year <- 2015
repos <- all_weeks(year)
forks <- sapply(repos, "[[", "fork")

names <- unique(unname(sapply(repos, "[[", "full_name")))
saveRDS(names, paste0("repos", year, ".rds"))

# Distribution of repo counts

owners <- count(sapply(repos, function(x) x$owner$login))
subset(arrange(owners, desc(freq)), freq > 2)
