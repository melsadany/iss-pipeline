################################################################################
# Run this ONCE on reference embeddings to create archetype model
################################################################################

library(archetypes)
library(tidyverse)

# helper
archetypes_summ <- function(obj, k, points_labels) {
  ss <- simplexplot(obj)
  arc <- rbind(cbind(ss$proj_z, 
                     text = paste0("A", c(1:k)))) %>% 
    as.data.frame()
  a.df <- cbind(ss$proj_h,
                text = points_labels) %>%
    rbind(cbind(ss$proj_z, 
                text = paste0("A", c(1:k)))) %>%
    as.data.frame() %>%
    mutate(x=as.numeric(x),
           y=as.numeric(y),
           lab = ifelse(text %in% paste0("A", c(1:k)), T, F),
           size2 = ifelse(grepl("A", text), 1, 0.7)) %>%
    full_join(arc %>% complete(nesting(x,y), text) %>% 
                select(text, xend=x, yend=y) %>%
                left_join(arc, .,by = 'text') %>%
                dplyr::filter(!(x==xend & y==yend)) %>%
                mutate_at(.vars = c(1,2,4,5), 
                          .funs = function(x) as.numeric(x)) %>% 
                select(-text) %>% 
                mutate(lab = F)) %>%
    mutate(text = ifelse(is.na(xend), text, ""))
  
  alpha.mat <- coef(obj)
  arch.mat <- parameters(obj)
  b.df <- as.matrix(alpha.mat) %*% as.matrix(arch.mat)
  
  ret <- list(simplex_plot = a.df,
              arch_coef = alpha.mat,
              arch.weights = arch.mat,
              reduced_data = b.df)
  return(ret)
}

################################################################################
# Semantic
################################################################################
# Load all semantic embeddings
sem_embeddings <- read_rds("reference_data/embeddings/semantic_common_50k.rds")

# Fit archetype model (k=10 archetypes)
cat("Fitting archetype model (this may take a while)...\n")
sem_arch_model <- stepArchetypes(
  sem_embeddings %>% select(-text) %>% as.matrix(), 
  k = 10, 
  nrep = 4, 
  verbose = TRUE
)

# Get best model
sem_arch <- bestModel(sem_arch_model)

# Get archetype coefficients for all words
sem_arch_coef <- sem_arch$alphas  # alpha matrix
sem_arch_weights <- as.data.frame(sem_arch_coef)
colnames(sem_arch_weights) <- paste0("A", 1:ncol(sem_arch_weights))
sem_arch_weights$text <- sem_embeddings$text

# Assign each word to dominant archetype
sem_text_arch_categorized <- sem_arch_weights %>%
  pivot_longer(cols = starts_with("A"), names_to = "archetype", values_to = "weight") %>%
  group_by(text) %>%
  slice_max(order_by = weight, n = 1) %>%
  select(text, archetype = archetype, weight)


# Create simplex plot data (for archetypal area calculation)
sem_arch_summ <- archetypes_summ(obj = sem_arch, k = 10, points_labels = sem_embeddings$text)
sem_arch_simplex <- sem_arch_summ$simplex_plot %>% dplyr::filter(text %in% sem_embeddings$text) %>%
  select(word = text, x, y)

sem_arch.ls <- list(model = sem_arch, assignments = sem_text_arch_categorized, 
                    simplex = sem_arch_simplex, weights = sem_arch_weights)
sem_arch.ls %>% write_rds("reference_data/archetypes/semantic_common_50k.rds", compress = "gz")

cat("✓ Semantic archetype model and assignments saved\n")


################################################################################
# Phonetic
################################################################################
# Load all phonetic embeddings
phon_embeddings <- read_rds("reference_data/embeddings/phonetic_common_50k.rds")

# identify high variance dimensions
low_var <- which(apply(phon_embeddings, 2, sd) %>% as.numeric() <0.20)
phon_embeddings_filt <- phon_embeddings %>% select(-low_var)
high_var <- colnames(phon_embeddings_filt)[-1]

# Fit archetype model (k=7 archetypes)
cat("Fitting archetype model (this may take a while)...\n")
phon_arch_model <- stepArchetypes(
  phon_embeddings_filt %>% select(-text) %>% as.matrix(), 
  k = 7, 
  nrep = 4, 
  verbose = TRUE
)

# Get best model
phon_arch <- bestModel(phon_arch_model)

# Get archetype coefficients for all words
phon_arch_coef <- phon_arch$alphas  # alpha matrix
phon_arch_weights <- as.data.frame(phon_arch_coef)
colnames(phon_arch_weights) <- paste0("A", 1:ncol(phon_arch_weights))
phon_arch_weights$text <- phon_embeddings_filt$text

# Assign each word to dominant archetype
phon_text_arch_categorized <- phon_arch_weights %>%
  pivot_longer(cols = starts_with("A"), names_to = "archetype", values_to = "weight") %>%
  group_by(text) %>%
  slice_max(order_by = weight, n = 1) %>%
  select(text, archetype = archetype, weight)


# Create simplex plot data (for archetypal area calculation)
phon_arch_summ <- archetypes_summ(obj = phon_arch, k = 7, points_labels = phon_embeddings_filt$text)
phon_arch_simplex <- phon_arch_summ$simplex_plot %>% dplyr::filter(text %in% phon_embeddings$text) %>%
  select(word = text, x, y)

phon_arch.ls <- list(model = phon_arch, assignments = phon_text_arch_categorized, 
                     simplex = phon_arch_simplex, high_var_dims = high_var, weights = phon_arch_weights)
phon_arch.ls %>% write_rds("reference_data/archetypes/phonetic_common_50k.rds", compress = "gz")

cat("✓ Phonetics archetype model and assignments saved\n")
