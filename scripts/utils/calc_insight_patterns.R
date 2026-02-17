analyze_insight_patterns <- function(response_times, words) {
  # Look for patterns suggesting "aha moments" or insight-based problem solving
  
  # Calculate inter-response intervals (IRIs)
  iris <- diff(response_times)
  
  ## get the 75% of thinking time
  long.time <- quantile(iris, 0.75, na.rm = TRUE)%>%as.numeric()
  short.time <- quantile(iris, 0.25, na.rm = TRUE)%>%as.numeric()
  
  dt <- data.frame(inter=iris) %>% 
    mutate(type = case_when(inter>=long.time ~ "long",
                            inter<=short.time ~ "short",
                            T~""),
           before = lag(type),
           insight = type=="short"&before=="long")
  
  return(list(
    insight_pattern_density = sum(dt$insight,na.rm=T) / nrow(dt),
    insight_pattern_count = sum(dt$insight,na.rm=T),
    
    # Pattern consistency (indicating systematic vs random approach)
    pattern_regularity = calculate_regularity_index(iris)
  ))
}

calculate_regularity_index <- function(iris) {
  # Fourier transform to detect rhythmic patterns
  if (length(iris) < 4) return(NA)
  spec <- stats::spectrum(iris, plot = FALSE)
  # Higher power at dominant frequency = more regular pattern
  return(max(spec$spec))
}