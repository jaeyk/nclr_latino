## ---------------------------------
# Import pkgs
pacman::p_load(
  pdftools,
  tidyverse,
  purrr,
  furrr,
  tm,
  here,
  tidytext, # text analysis 
  SnowballC, # stemming 
  textclean, # text preprocessing 
  ggthemes,
  text2vec, # word embedding 
  widyr,
  patchwork, # arranging ggplots
  glue,
  reactable,
  zoo
)

theme_set(theme_clean())

source(here("functions", "utils.r"))


## ----eval = FALSE-----------------
## # File name list
## filename <- list.files(here("raw_data", "nclr"))


## ----eval = FALSE-----------------
## # Excluding codebook
## filename <- filename[!str_detect(filename, "codebook")]
## 
## # Mapping
## text_list <- map(here("raw_data", "nclr", filename), pdf_text)
## 
## # List into a data frame
## df <- text_list %>%
##   map_df(enframe, .id = "ListElement")
## 
## # Reformat filename
## date_list <- gsub(".pdf", "", filename)


## ----eval = FALSE-----------------
## # for loop
## 
## for (i in seq(date_list)) {
##   df$ListElement[df$ListElement == paste(i)] <- date_list[i]
## }
## 
## # Rename and mutate
## df <- df %>%
##   rename(
##     "date" = "ListElement",
##     "page_number" = "name"
##   ) %>%
##   mutate(date = str_replace_all(date, "_", "-")) %>%
##   mutate(date = lubridate::ymd(date))
## 
## # Ignore page numbers and concatenate texts by date
## df <- df %>%
##   group_by(date) %>%
##   summarise(value = paste(value, collapse = ","))
## 
## # Save the df
## saveRDS(df, file = here("processed_data/nclr.Rdata"))


## ---------------------------------
# Load the df 
df <- readRDS(here("processed_data/nclr.Rdata"))

new_df <- separate(df, date, into = c("year", "month", "month_int"), sep = "-")
new_date_nclr <- glue("{new_df$year}-{new_df$month}")

df$date <- zoo::as.yearmon(new_date_nclr)

asian <- read_csv(here("raw_data", "AsianMag_articles.csv")) # Gidra (1969-1974) and Bridge (1970-1982)

new_date_asian <- glue("{asian$year}-{asian$month}") %>%
  readr::parse_number() # only numeric 

asian$date <- zoo::as.yearmon(new_date_asian)

asian <- asian %>%
  rename(value = text) %>%
  select(value, date)


## ----eval = FALSE-----------------
## df <- clean_text(df)
## asian <- clean_text(asian)
## 
## # Call stop words dictionary
## data("stop_words")


## ----eval = FALSE-----------------
## # Using stemming
## tf_idf_stem_nclr <- get_word_count(df, stem = TRUE)
## tf_idf_stem_asian <- get_word_count(asian, stem = TRUE)
## 
## # Save the objects
## save(df, tf_idf_stem_nclr,
##      asian, tf_idf_stem_asian,
##      file = here("processed_data/processed_text.Rdata"))


## ---------------------------------
load(file = here("processed_data/processed_text.Rdata"))

tf_idf_stem <- bind_rows(
  mutate(tf_idf_stem_nclr, group = "Hispanic"), 
  mutate(tf_idf_stem_asian, group = "Asian"))

#tf_idf_stem$stem[str_detect(tf_idf_stem$stem, "recog")]


## ---------------------------------
ggplot2::theme_set(ggthemes::theme_clean())

plot_track_keyword(tf_idf_stem, "visibl") /
plot_track_keyword(tf_idf_stem, "recog")

ggsave(here("outputs", "desc.png"), width = 10, height = 10)


## ---------------------------------
asian <- asian %>% filter(!is.na(date))

tidy_pmi_nclr <- df2pmi(df)
tidy_pmi_asian <- df2pmi(asian)

save(tidy_pmi_nclr, 
     tidy_pmi_asian, 
     file = here("processed_data", "tidy_pmi.Rdata"))


## ----eval = FALSE-----------------
## knitr::purl(input = here("code", "01_EDA.Rmd"),
##             output = here("code", "01_EDA.r"))

