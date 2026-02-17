calc_euclidean_path <- function(words, word_embeddings, nCores = 1) {
  # make sure that the embeddings matrix has words as rownames
  consec.pairs <- data.frame(w1 = words[-length(words)], w2 = words[-1])
  registerDoMC(cores = nCores)
  res <- foreach(ii=1:nrow(consec.pairs), .combine = rbind) %dopar% {
    w1 <- consec.pairs$w1[ii]
    w2 <- consec.pairs$w2[ii]
    data.frame(w1 = w1,w2=w2,
               dist=dist(rbind(as.numeric(word_embeddings[w1,]),
                               as.numeric(word_embeddings[w2,]))) %>%
                 as.numeric)
  }
  return(sum(res$dist,na.rm=T))
}