
raw_dir <- file.path(devtools::package_file(), "data-raw")
expt <- "visualRecollection"
cutoff <- 1

targets <- readr::read_csv(file.path(raw_dir, 'objectNames_2afc.csv') )

out <- list()
for (i in 1:dim(targets[1])){
  out[i] = list(c(targets[i,]$name1, targets[i,]$name2,
                  targets[i,]$name3, targets[i,]$name4, targets[i,]$name5,
                  targets[i,]$name6, targets[i,]$name7, targets[i,]$name8, targets[i,]$name9,
                  targets[i,]$name10, targets[i,]$name11, targets[i,]$name12, targets[i,]$name13,
                  targets[i,]$name14, targets[i,]$name15))
}

s_dirs <- list.files(path = raw_dir, pattern = "sub*")
files <- list.files(path = file.path(raw_dir, s_dirs, 'beh', expt), pattern = "n.csv", full.names = TRUE)

d <- lapply(files, readr::read_csv, col_types = readr::cols(pas = readr::col_character())) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(targets = out[object],
                dl = dplyr::case_when(phase == "name" ~ purrr::map2(targets, response,
                                                                    ~stringdist::stringdist(.x, .y, method="dl")),
                                      phase != "name" ~ list(Inf)),
                mindist = purrr::map_dbl(dl, ~min(.x, na.rm = TRUE)),
                correct = dplyr::case_when(phase == "noise" & gonogo == "go" ~ dplyr::if_else(response=="Return",TRUE,FALSE),
                                           phase == "noise" & gonogo == "nogo" ~ dplyr::if_else(response=="NO RESPONSE",TRUE,FALSE),
                                           phase == "study" ~ NA,
                                           phase == "name" & mindist < cutoff ~ TRUE,
                                           phase == "name" & mindist < cutoff ~ FALSE)
  ) %>%
  dplyr::mutate(correct_fct = factor(correct)) %>%
  dplyr::mutate(pas = dplyr::case_when(stringr::str_detect(pas, "12") ~ NA_character_,
                                       stringr::str_detect(pas, "02") ~ NA_character_,
                                       stringr::str_detect(pas, "1!02@") ~ NA_character_,
                                       stringr::str_detect(pas, "1") ~ "1",
                                       stringr::str_detect(pas, "2") ~ "2",
                                       stringr::str_detect(pas, "3") ~ "3",
                                       stringr::str_detect(pas, "0") ~ "0"),
                pas = as.numeric(pas))

usethis::use_data(d)
