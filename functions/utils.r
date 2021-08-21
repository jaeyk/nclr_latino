con2nplot <- function(corpus, keyword, local_glove, local_transform) { 
  
  # Latino context 
  contextL <- get_context(x = corpus$value[corpus$source == "NCLR"], target = keyword)
  
  # Asian context 
  contextA <- get_context(x = corpus$value[corpus$source != "NCLR"], target = keyword)
  
  # bind contexts 
  contexts_corpus <- rbind(cbind(contextL, group = "Latino"), cbind(contextA, group = "Asian"))
  
  # embed each instance using a la carte
  contexts_vectors <- embed_target(
    context = contexts_corpus$context, 
    pre_trained = local_glove, 
    transform_matrix = local_transform, 
    transform = TRUE, 
    aggregate = FALSE, 
    verbose = TRUE)

  # get local vocab
  local_vocab <- get_local_vocab(c(contextL$context, contextA$context), pre_trained = local_glove)

  set.seed(1234)
  contrast_target <- contrast_nns(
    context1 = contextL$context, 
    context2 = contextA$context, 
    pre_trained = local_glove, 
    transform_matrix = local_transform, 
    transform = TRUE, 
    bootstrap = TRUE, 
    num_bootstraps = 20, 
    permute = TRUE, 
    num_permutations = 100, 
    candidates = local_vocab, 
    norm = "l2")

  # define top N of interest
  N <- 30

  # first get each party's nearest neighbors (output by the contrast_nns function)
  nnsL <- contrast_target$nns1
  nnsA <- contrast_target$nns2

  # subset to the union of top N nearest neighbors for each party
  top_nns <- union(nnsL$Term[1:N], nnsA$Term[1:N])

  # identify which of these are shared
  shared_nns <- intersect(nnsL$Term[1:N], nnsA$Term[1:N])
  
  # subset nns_ratio (output by contrast_nns) to the union of the top nearest
  # neighbors
  nns_ratio <- contrast_target$nns_ratio %>% 
    dplyr::filter(Term %in% top_nns) %>% 
    mutate(group = case_when(
      Term %in% nnsL$Term[1:N] & !(Term %in% nnsA$Term[1:N]) ~ "Latino", 
      !(Term %in% nnsL$Term[1:N]) & Term %in% nnsA$Term[1:N] ~ "Asian", 
      Term %in% shared_nns ~ "Shared"), 
      significant = if_else(Empirical_Pvalue < 0.01, "yes", "no"))

  # order Terms by Estimate
  nns_ratio <- nns_ratio %>% 
    mutate(absdev = abs(1 - Estimate)) %>% 
    arrange(-absdev) %>% 
    mutate(tokenID = 1:nrow(.)) %>% 
    mutate(Term_Sig = if_else(significant == "yes", paste0(Term, "*"), Term))

  # plot
  nns_ratio %>%
    ggplot() + 
    geom_point(aes(x = Estimate, y = tokenID, color = group, shape = group), 
               data = nns_ratio, size = 2) + 
    geom_vline(xintercept = 1, colour = "black", linetype = "dashed", 
               size = 0.5) + 
    geom_text(
      aes(x = Estimate, y = tokenID, label = Term_Sig), 
        data = nns_ratio, 
        hjust = if_else(nns_ratio$Estimate > 1, -0.2, 1.2), 
        vjust = 0.25, 
        size = 5) +
    scale_color_manual(
      values = c("black", "gray30", "gray60")) + 
    xlim(0, 2) + 
    ylim(0, 60) + 
    ylab("") + 
    xlab("cosine similarity ratio \n (Latino/Asian)") + 
    theme(
      panel.background = element_blank(), 
      plot.title = element_text(size = 18, 
                                hjust = 0.5, color = "blue"), 
      axis.text.x = element_text(size = 16), 
      axis.text.y = element_text(size = 16), 
      axis.title.y = element_text(size = 16, margin = margin(t = 0, r = 15, b = 0, 
                                                             l = 15)), 
      axis.title.x = element_text(size = 16, margin = margin(t = 15, r = 0, b = 15, l = 0)), 
      legend.text = element_text(size = 16), 
      legend.title = element_blank(), 
      legend.key = element_blank(), 
      legend.position = "top", 
      legend.spacing.x = unit(0.25, "cm"), 
      plot.margin = unit(c(1, 1, 0, 0), "cm")) +
    ggplot2::annotate(
      geom = "text", 
      x = c(0.5, 1.5), 
      y = c(55, 55), 
      label = c("more Asian", 
                "more Latino")
    )
}

key2convec <- function(corpus, keyword) {

  contexts <- get_context(x = corpus$value, target = keyword, window = 6, valuetype = "fixed", case_insensitive = TRUE, hard_cut = FALSE, verbose = FALSE)

  contexts_vectors <- embed_target(context = contexts$context, pre_trained = local_glove, transform_matrix = local_transform, transform = TRUE, aggregate = TRUE, verbose = TRUE)

  return(contexts_vectors)
}

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

df2cm <- function(corpus, count_min = 5, window_size = 5) {
  
  ############################### Create VOCAB ###############################
  
  # Create iterator over tokens
  tokens <- space_tokenizer(corpus$value)
  
  # Create vocabulary. Terms will be unigrams (simple words).
  it <- itoken(tokens, progressbar = TRUE)
  
  vocab <- create_vocabulary(it)
  
  # Filter words
  vocab_pruned <- prune_vocabulary(vocab, term_count_min = count_min)
  
  # use quanteda's fcm to create an fcm matrix
  fcm_cr <- tokens(corpus$value) %>% 
    quanteda::fcm(context = "window", count = "frequency", 
                                             window = window_size, weights = rep(1, window_size), tri = FALSE)
  
  # subset fcm to the vocabulary included in the embeddings
  fcm_cr <- fcm_select(fcm_cr, pattern = vocab_pruned$term, selection = "keep")
  
  return(fcm_cr)
}

df2ltm <-  function(corpus, fcm_cr, local_glove, count_min = 5, window_size = 5) {
  
  ############################### Create VOCAB ###############################
  
  # Create iterator over tokens
  tokens <- space_tokenizer(corpus$value)
  
  # Create vocabulary. Terms will be unigrams (simple words).
  it <- itoken(tokens, progressbar = TRUE)
  
  vocab <- create_vocabulary(it)
  
  # Filter words
  vocab_pruned <- prune_vocabulary(vocab, term_count_min = count_min)
  
  local_transform <- compute_transform(context_fcm = fcm_cr, pre_trained = local_glove, 
                                       vocab = vocab_pruned, weighting = 1000)
}

df2vec <- function(corpus, count_min = 5, window_size = 5, dims = 50) {
  
  ############################### Create VOCAB ###############################
  
  # Create iterator over tokens
  tokens <- space_tokenizer(corpus$value)
  
  # Create vocabulary. Terms will be unigrams (simple words).
  it <- itoken(tokens, progressbar = TRUE)
  
  vocab <- create_vocabulary(it)
  
  # Filter words
  vocab_pruned <- prune_vocabulary(vocab, term_count_min = count_min)
  
  ############################### Create Term Co-occurence Matrix ###############################
  
  # Use our filtered vocabulary
  vectorizer <- vocab_vectorizer(vocab_pruned)
 
  # Use window of 10 for context words
  tcm <- create_tcm(it, vectorizer, skip_grams_window = window_size, skip_grams_window_context = "symmetric", weights = rep(1, window_size))
  
  ############################### Set Model Parameters ###############################
  
  glove <- GlobalVectors$new(rank = dims, x_max = 10)
  
  ############################### Fit Model ###############################
  
  wv_main <- glove$fit_transform(tcm, n_iter = 1000, convergence_tol = 0.001, n_threads = RcppParallel::defaultNumThreads())
  
  ############################### Get Output ###############################
  
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
  
  out <- out %>% select(keyword, word, similarity) 
  
  out <- out %>%
    rename(word1 = keyword, 
           word2 = word, 
           n = similarity)
  
  return(out)
}

vec2sim3 <- function(word_vectors, exp, group1, group2, n = 10) {
  
  pattern <- word_vectors[exp, , drop = FALSE] - word_vectors[group1, , drop = FALSE] + word_vectors[group2, , drop = FALSE]
  
  cos_sim <- sim2(x = word_vectors, y = pattern, 
                  method = "cosine", norm = "l2")
  
  out <- head(sort(cos_sim[,1], decreasing = TRUE), n + 1)[-1]
  
  out <- data.frame(out) %>%
    add_rownames("word") %>%
    rename(similarity = out) 
  
  out$exp <- exp
  out$group1 <- group1
  out$group2 <- group2
  
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
  
  # Remove stopwords and excessive whitespace
  df <- df %>% mutate(value = value %>%
                       # Remove stopwords 
                       tm::removeWords(words = stop_words$word) %>%
                       # Remove excessive white space
                       str_squish()) 
  
  return(df)

}

rec_stem <- function(tf_idf_stem, stem) {
  
  keyword <- tf_idf_stem$stem[str_detect(tf_idf_stem$stem, stem)] %>% unique()
  
  return(keyword) 
}

plot_track_keyword <- function(tf_idf, keyword) {

  base_count <- tf_idf %>%
    group_by(group, date, word) %>%
    summarise(sum_n = sum(n, na.rm = TRUE),
              sum_tf_idf = sum(tf_idf, na.rm = TRUE)) %>%
    filter(str_detect(word, keyword)) %>%
    rename(count = sum_n,
           tf_idf = sum_tf_idf) 
  
  base_count %>%
    ggplot(aes(x = date, y = tf_idf, col = group)) +
    geom_point() +
    geom_line(alpha = 0.5) +
    labs(title = glue("The count of words related to {keyword}"),
         col = "Source",
         x = "",
         y = "") +
    theme(legend.position = "bottom")
  #       caption = glue("Sources: National Council of La Raza, 1972-1981 (Hispanic),
   #                       Gidra, 1969-1974, Bridge, 1970-1982 (Asian)"))
    

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