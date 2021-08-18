# preprocessing 

get_word_count <- function(data, stem = TRUE) {
  
  tidy_df <- data %>%
    # Tokenize 
    unnest_tokens("word", value) %>%
    # Remove stop words 
    anti_join(get_stopwords(), by = "word") 
  
  if (stem == TRUE) {
    
    # Stemming 
    tidy_df <- tidy_df %>% mutate(stem = wordStem(word))
    
    df_words <- tidy_df %>%
      count(date, stem, sort = TRUE) 
    
    total_words <- df_words %>% 
      group_by(date) %>% 
      summarize(total = sum(n))
    
    joined_words <- left_join(df_words, total_words)
    
    tf_idf <- joined_words %>% 
      # TF-IDF
      bind_tf_idf(stem, date, n) 
    
  } else {
    
    df_words <- tidy_df %>% count(date, word, sort = TRUE) 
  
    total_words <- df_words %>% 
      group_by(date) %>% 
      summarize(total = sum(n))
    
    joined_words <- left_join(df_words, total_words)
    
    tf_idf <- joined_words %>% 
      # TF-IDF
      bind_tf_idf(word, date, n)   
    
  }
  
  return(tf_idf)
  
}

# The following code draws on this reference: https://smltar.com/embeddings.html

slide_windows <- function(tbl, window_size) {
  skipgrams <- slider::slide(
    tbl, 
    ~.x, 
    .after = window_size - 1, 
    .step = 1, 
    .complete = TRUE
  )
  
  safe_mutate <- safely(mutate)
  
  out <- map2(skipgrams,
              1:length(skipgrams),
              ~ safe_mutate(.x, window_id = .y))
  
  out %>%
    transpose() %>%
    pluck("result") %>%
    compact() %>%
    bind_rows()
}

# The following code draws on this reference: https://smltar.com/embeddings.html

nearest_neighbors <- function(df, token) {
  df %>%
    widely(
      ~ {
        y <- .[rep(token, nrow(.)), ]
        res <- rowSums(. * y) / 
          (sqrt(rowSums(. ^ 2)) * sqrt(sum(.[token, ] ^ 2)))
        
        matrix(res, ncol = 1, dimnames = list(x = names(res)))
      },
      sort = TRUE
    )(item1, dimension, value) %>%
    select(-item2)
}

# input: keyword, output: similarity 

key2sim <- function(vectors, key_word, n = 30) {
  
  out <- vectors %>%
    nearest_neighbors(key_word) %>%
    filter(item1 != key_word) %>%
    top_n(n, abs(value)) %>%
    mutate(value = round(value,2)) %>%
    rename(word = item1,
           similarity = value) %>%
    mutate(keyword = key_word)
  
  return(out)
  
} 

plot_embed <- function(embed) {
  
  out <- embed %>%
    filter(!is.na(keyword)) %>%
    group_by(keyword) %>%
    slice_max(similarity, n = 10) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>%
    ggplot(aes(similarity, word, fill = keyword)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~keyword, ncol = 5, scales = "free")
  
  return(out)
}

clean_text <- function(df) {

  # Remove non word characters and cover pages 
  df$value <- textclean::strip(df$value)

  # Remove cover pages 
  df <- df %>%
    filter(str_length(value) != 0) %>%
    filter(!is.na(date))
  
  return(df)

}

rec_stem <- function(tf_idf_stem, stem) {
  
  keyword <- tf_idf_stem$stem[str_detect(tf_idf_stem$stem, stem)] %>% unique()
  
  return(keyword) 
}

plot_track_keyword <- function(tf_idf_stem, stem) {
  
  keyword <- tf_idf_stem$stem[str_detect(tf_idf_stem$stem, stem)] %>% unique()
  
  if (length(keyword) == 0) stop("the stem is not found in the corpus.") else {
    
    keyword <- paste(keyword, collapse = "|")
    
  }

  base_stem_count <- tf_idf_stem %>%
    group_by(group, date, stem) %>%
    summarise(sum_n = sum(n, na.rm = TRUE),
              sum_tf_idf = sum(tf_idf, na.rm = TRUE)) %>%
    filter(str_detect(stem, keyword)) %>%
    rename(count = sum_n,
           tf_idf = sum_tf_idf) %>%
    pivot_longer(cols = c(count, tf_idf),
                 names_to = "Measurement",
                 values_to = "values")

  plot <- base_stem_count %>%
    ggplot(aes(x = date, y = values, color = group)) +
    geom_point() +
    labs(title = glue("The count of words related to {stem}"),
         x = "Issue date",
         y = "Count",
         color = "Group",
         caption = glue("Sources: National Council of La Raza, 1972-1981 (Hispanic),
                          Gidra, 1969-1974, Bridge, 1970-1982 (Asian)")) +
    facet_wrap(~Measurement, scales = "free_y")

  return(plot)
}

df2pmi <- function(df) {
  
  tokenized_df <- df %>%
    # Tokenize 
    unnest_tokens("word", value)
  
  tidy_df <- tokenized_df %>%
    # Add count 
    add_count(word) %>%
    # Drop the variable 
    select(-n)
  
  nested_df <- tidy_df %>%
    nest(words = c(word))
  
  plan(multisession)  ## for parallel processing
  set.seed(1234)
  
  unnested_df <- nested_df %>%
    mutate(words = future_map(words, slide_windows, 4L)) %>%
    unnest(words) 
  
  tidy_pmi <- unnested_df %>%
    unite(window_id, date, window_id) %>%
    pairwise_pmi(word, window_id)
  
  return(tidy_pmi)
}