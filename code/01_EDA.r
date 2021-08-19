## ------------------------------------
# Import pkgs
pacman::p_load(
  pdftools,
  readtext,
  tidyverse,
  purrr,
  furrr,
  tm,
  here,
  tidytext, # text analysis 
  SnowballC, # stemming 
  textclean, # text preprocessing 
  ggthemes,
  wesanderson,
  text2vec, # word embedding 
  widyr,
  patchwork, # arranging ggplots
  glue,
  reactable,
  zoo
)

theme_set(theme_clean())

source(here("functions", "utils.r"))


## ----eval = FALSE--------------------
## # File name list
## filename <- list.files(here("raw_data", "nclr"))
## 
## asian_filename <- list.files(here("raw_data", "clean_magazines_txts"))


## ----eval = FALSE--------------------
## # Excluding codebook
## filename <- filename[!str_detect(filename, "codebook")]
## 
## gidra <- asian_filename[str_detect(asian_filename, "Gidra")]
## 
## bridge <- asian_filename[str_detect(asian_filename, "Bridge")]


## ------------------------------------
# Mapping 
text_list <- map(here("raw_data", "nclr", filename), pdf_text)

# List into a data frame
df <- text_list %>%
  map_df(enframe, .id = "ListElement")

# Reformat filename
date_list <- gsub(".pdf", "", filename)

# for loop

for (i in seq(date_list)) {
  df$ListElement[df$ListElement == paste(i)] <- date_list[i]
}

# Rename and mutate
df <- df %>%
  rename(
    "date" = "ListElement",
    "page_number" = "name"
  ) %>% 
  mutate(date = str_replace_all(date, "_", "-")) %>%
  mutate(date = lubridate::ymd(date))

# Ignore page numbers and concatenate texts by date 
df <- df %>% 
  group_by(date) %>%
  summarise(value = paste(value, collapse = ","))

# Save the df
saveRDS(df, file = here("processed_data/nclr.Rdata"))


## ------------------------------------
gidra_text <- map_dfr(here("raw_data", "clean_magazines_txts", gidra), parse_text)

bridge_text <- map_dfr(here("raw_data", "clean_magazines_txts", bridge), parse_text)

asian_text <- bind_rows(gidra_text, bridge_text)

# Save the asian_text
saveRDS(asian_text, file = here("processed_data/asian.Rdata"))


## ------------------------------------
# Load the df 
df <- readRDS(here("processed_data/nclr.Rdata"))

new_df <- separate(df, date, into = c("year", "month", "month_int"), sep = "-")

new_date_nclr <- glue("{new_df$year}-{new_df$month}")

new_date_asian <- glue("{asian_text$year}-{asian_text$month}")

df$date <- zoo::as.yearmon(new_date_nclr)
df$year <- new_df$year
  
asian_text$date <- zoo::as.yearmon(new_date_asian)


## ----eval = FALSE--------------------
## df <- clean_text(df)
## asian_text <- clean_text(asian_text)
## 
## # Call stop words dictionary
## data("stop_words")


## ----eval = FALSE--------------------
## # Using stemming
## tf_idf_stem_nclr <- get_word_count(df, stem = TRUE)
## tf_idf_stem_gidra <- get_word_count(asian_text %>%
##                                       filter(source == "Gidra"), stem = TRUE)
## tf_idf_stem_bridge <- get_word_count(asian_text %>%
##                                        filter(source == "Bridge"), stem = TRUE)
## 
## # Save the objects
## save(df, tf_idf_stem_nclr,
##      asian_text, tf_idf_stem_gidra, tf_idf_stem_bridge,
##      file = here("processed_data/processed_text.Rdata"))


## ------------------------------------
#load(file = here("processed_data/processed_text.Rdata"))

tf_idf_stem <- bind_rows(
  mutate(tf_idf_stem_nclr, group = "NCLR"), 
  mutate(tf_idf_stem_gidra, group = "Gidra"),
  mutate(tf_idf_stem_bridge, group = "Bridge"))


## ------------------------------------
ggplot2::theme_set(ggthemes::theme_clean())

plot_track_keyword(tf_idf_stem, "visibl") /
plot_track_keyword(tf_idf_stem, "recogn")

ggsave(here("outputs", "desc.png"), width = 10, height = 10)


## ----eval = FALSE--------------------
## knitr::purl(input = here("code", "01_EDA.Rmd"),
##             output = here("code", "01_EDA.r"))

