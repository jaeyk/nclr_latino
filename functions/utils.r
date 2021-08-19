parse_text <- function(file_path) {
  
  text <- readtext::readtext(file_path)
  
  meta <- text$doc_id %>% str_split("_")
  
  out <- data.frame(source = meta[[1]][1],
                    year = meta[[1]][2],
                    month = meta[[1]][3],
                    value = text$text)
  
  return(out)
}

keyword2plot <- function(word_vector, keywords, n, custom_title = NULL){

  out <- purrr::map_dfr(keywords, 
               possibly(~vec2sim(word_vector, ., n),
                        # This sorts out the keyword not in the word embedding
                        otherwise = 
                          data.frame(word = NA, 
                                     similarity = NA, 
                                     keyword = NA)))

  if (is.null(custom_title)) {
  
    out <- plot_embed(out) 
    
    return(out)
    
    }
  
  else {
      
      out <- plot_embed(out) + labs(title = custom_title)
      
      return(out)
      
    } 
}

df2vec <- function(df) {
  
  # Create iterator over tokens
  tokens <- space_tokenizer(df$value)
  
  # Create vocabulary. Terms will be unigrams (simple words).
  it = itoken(tokens, progressbar = FALSE)
  
  vocab <- create_vocabulary(it)
  
  # Filter words
  vocab <- prune_vocabulary(vocab, term_count_min = 5L)
  
  # Use our filtered vocabulary
  vectorizer <- vocab_vectorizer(vocab)
 
  # Use window of 10 for context words
  tcm <- create_tcm(it, vectorizer, skip_grams_window = 10L)
  
  glove <- GlobalVectors$new(rank = 50, x_max = 10)
  
  wv_main <- glove$fit_transform(tcm, n_iter = 1000, convergence_tol = 0.001, n_threads = 8)
  
  wv_context <- glove$components
  
  word_vectors <- wv_main + t(wv_context)
  
  return(word_vectors)
}

vec2sim <- function(word_vectors, keyword, n = 10) {
  
  pattern <- word_vectors[keyword, , drop = FALSE]
  
  cos_sim <- sim2(x = word_vectors, y = pattern, 
                  method = "cosine", norm = "l2")
  
  out <- head(sort(cos_sim[,1], decreasing = TRUE), n + 1)[-1]
  
  out <- data.frame(out) %>%
    add_rownames("word") %>%
    rename(similarity = out) 
  
  out$keyword <- keyword
  
  out <- out %>% select(word, similarity, keyword)
  
  return(out)
}

# Adapted from here: https://cbail.github.io/textasdata/word2vec/rmarkdown/word2vec.html
skipgrams_generator <- function(text, tokenizer, window_size, negative_samples) {
  
  tokenizer <- text_tokenizer(num_words =  10000) # maximum number of word to keep (based on frequency)
  
  gen <- texts_to_sequences_generator(tokenizer, sample(text))
  
  function() {
    skip <- generator_next(gen) %>%
      skipgrams(
        vocabulary_size = tokenizer$num_words, 
        window_size = window_size, 
        negative_samples = 1
      )
    
    x <- transpose(skip$couples) %>% map(. %>% unlist %>% as.matrix(ncol = 1))
    y <- skip$labels %>% as.matrix(ncol = 1)
    
    list(x, y)
  }
  
}

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
    facet_wrap(~keyword, ncol = 5, scales = "free") +
    labs(x = "Cosine similiarity",
         y = "")
  
  return(out)
}

clean_text <- function(df) {

  # Remove non word characters
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
    ggplot(aes(x = date, y = values, col = group)) +
    geom_point() +
    labs(title = glue("The count of words related to {stem}"),
         col = "Source",
         x = "",
         y = "",
         caption = glue("Sources: National Council of La Raza, 1972-1981 (Hispanic),
                          Gidra, 1969-1974, Bridge, 1970-1982 (Asian)")) +
    facet_wrap(~Measurement, scales = "free_y") +
    scale_color_manual(values = wes_palette(n = 3, name = "GrandBudapest1"))

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