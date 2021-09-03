## -----------------------------------
# Import pkgs
pacman::p_load(
  tesseract,
  pdftools,
  magick, 
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
  ggpubr, 
  glue,
  reactable,
  zoo,
  stopwords
)

theme_set(theme_clean())

source(here("functions", "utils.r"))


## -----------------------------------
# File name list
filename <- list.files(here("raw_data", "nclr"))

asian_filename <- list.files(here("raw_data", "clean_magazines_txts"))


## -----------------------------------
# Excluding codebook
filename <- filename[!str_detect(filename, "codebook")]

gidra <- asian_filename[str_detect(asian_filename, "Gidra")]

bridge <- asian_filename[str_detect(asian_filename, "Bridge")]


## -----------------------------------
# Mapping 
text_list <- map(here("raw_data", "nclr", filename), pdf2text)

test <- pdf2text(here("raw_data", "nclr", filename[1]))

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
  mutate(date = str_replace_all(date, "_", "-"))

# Ignore page numbers and concatenate texts by date 
df <- df %>% 
  group_by(date) %>%
  summarise(value = paste(value, collapse = ","))

# Save the df
saveRDS(df, file = here("processed_data/nclr.Rdata"))


## -----------------------------------
gidra_text <- map_dfr(here("raw_data", "clean_magazines_txts", gidra), parse_text)

bridge_text <- map_dfr(here("raw_data", "clean_magazines_txts", bridge), parse_text)

asian_text <- bind_rows(gidra_text, bridge_text)

# Save the asian_text
saveRDS(asian_text, file = here("processed_data/asian.Rdata"))


## -----------------------------------
# Load the df 
df <- readRDS(here("processed_data/nclr.Rdata")) %>%
  filter(date != "codebook")

asian_text <- readRDS(here("processed_data/asian.Rdata"))

new_date_asian <- glue("{asian_text$year}-{asian_text$month}")

df$date <- zoo::as.yearmon(df$date)
asian_text$date <- zoo::as.yearmon(new_date_asian)


## -----------------------------------
# Call stop words dictionary 
data("stop_words")

df <- clean_text(df)
asian_text <- clean_text(asian_text)


## -----------------------------------
tf_idf_nclr <- get_word_count(df, stem = FALSE)
tf_idf_gidra <- get_word_count(asian_text %>%
                                 filter(source == "Gidra"), stem = FALSE)
tf_idf_bridge <- get_word_count(asian_text %>%
                                 filter(source != "Gidra"), stem = FALSE)

# Save the objects
save(df, tf_idf_nclr,
     asian_text, tf_idf_gidra, tf_idf_bridge, 
     file = here("processed_data/processed_text.Rdata"))


## -----------------------------------
#load(file = here("processed_data/processed_text.Rdata"))

tf_idf <- bind_rows(
  mutate(tf_idf_nclr, group = "NCLR"), 
  mutate(tf_idf_gidra, group = "Gidra"),
  mutate(tf_idf_bridge, group = "Bridge"))


## -----------------------------------
theme_set(theme_clean())

ggarrange(plot_track_keyword(tf_idf, "discrimination|prejudice|oppression|racism|poverty"), plot_track_keyword(tf_idf, "black|blacks"), ncol = 1, nrow = 2)

ggsave(here("outputs", "desc_comp.png"),width = 13)


## -----------------------------------
knitr::purl(input = here("code", "01_EDA.Rmd"),
            output = here("code", "01_EDA.r"))

