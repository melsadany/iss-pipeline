calc_divergence <- function(words, word_embeddings, start_word = NULL) {
  require(TSP)
  if (!is.null(start_word)) {
    all.words <- unique(c(start_word,words))
    dist.0 <- do.call(rbind,combn(all.words,m = 2,simplify = F)) %>% 
      as.data.frame() %>% rename(w1=1,w2=2) %>% rowwise() %>%
      mutate(distance = dist(rbind(word_embeddings[w1,] %>% as.numeric(),
                                   word_embeddings[w2,] %>% as.numeric())) %>%
               as.numeric())
    dist.m <- rbind(dist.0, dist.0 %>% select(w1=w2,w2=w1,distance)) %>%
      pivot_wider(names_from = w2,values_from = distance, id_cols = w1) %>%
      column_to_rownames("w1") %>% as.matrix()
    dist.m <- dist.m[all.words, all.words]
    tsp.dist <- TSP(dist.m) # travelling salesman problem
    tsp.sol <- as.integer(solve_TSP(tsp.dist, method = "nn", control = list("start" = 1))) 
  } else {
    dist.0 <- do.call(rbind,combn(words,m = 2,simplify = F)) %>% 
      as.data.frame() %>% rename(w1=1,w2=2) %>% rowwise() %>%
      mutate(distance = dist(rbind(word_embeddings[w1,] %>% as.numeric(),
                                   word_embeddings[w2,] %>% as.numeric())) %>%
               as.numeric())
    dist.m <- rbind(dist.0, dist.0 %>% select(w1=w2,w2=w1,distance)) %>%
      pivot_wider(names_from = w2,values_from = distance, id_cols = w1) %>%
      column_to_rownames("w1") %>% as.matrix()
    dist.m <- dist.m[words, words]
    tsp.dist <- TSP(dist.m) # travelling salesman problem
    tsp.sol <- as.integer(solve_TSP(tsp.dist, method = "nn")) 
  }
  
  optimal.distance = do.call(sum, lapply(c(1:(length(tsp.sol)-1)), function(xi) {
    dist.m[tsp.sol[xi],tsp.sol[xi+1]]
  }))
  actual.distance = do.call(sum, lapply(c(1:(nrow(dist.m)-1)), function(xi) {
    dist.m[xi,xi+1]
  }))
  return(data.frame(actual_distance = actual.distance,
                    optimal_distance = optimal.distance))
}