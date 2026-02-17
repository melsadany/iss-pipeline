identify_cognitive_styles <- function(features,iq) {
  # Cluster participants by their language production patterns
  style_clusters <- kmeans(scale(features), centers = 4)
  # Characterize each cognitive style
  styles <- list()
  for (i in 1:4) {
    style_features <- colMeans(features[style_clusters$cluster == i, ])
    styles[[paste0("style_", i)]] <- list(
      # description = describe_style(style_features),  # e.g., "Systematic explorer"
      prevalence = sum(style_clusters$cluster == i) / nrow(features),
      cognitive_profile = iq[style_clusters$cluster == i, ]
    )
  }
  return(styles)
}
