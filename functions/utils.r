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