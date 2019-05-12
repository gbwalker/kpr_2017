################
# FINAL RESULTS
################

library(tidyverse)
library(extrafont)
library(janitor)
library(stringr)
library(tm)
library(quanteda)
library(readme) # From https://github.com/iqss-research/readme-software.
library(topicmodels)
library(tidytext)

set.seed(50)

# Load the known 50c posts from posts_all, which has 43,757 leaked posts from the Zhanggong Internet Propaganda Department.
# It contains information about the posting organization, some URLs, some account names, post date, and text of the post.

df <- read_csv("data/posts_all.csv") %>% 
  
  # Clean the names of the variables.
  
  clean_names() %>% 
  mutate(city = what_is_the_city,
         organization = what_is_the_name_of_the_organization_making_posts,
         account = what_is_the_account_name_of_the_person_posting,
         date = post_date,
         text = textseg) %>% 
  
  # Add a categorical variable for labeled or unlabeled.
  
  mutate(labeled = case_when(
    !is.na(category) ~ 1,
    TRUE ~ 0
  )) %>% 
  
  # Make sure category is factor variable.
  
  mutate(category = factor(category)) %>% 
  
  # Select only the variables of interest.
  
  select(city, folder, file, organization, url, content, site, account, date, category, labeled, text)

# Note that 188 posts are categorized. I presume these are the coded posts that the human coders agreed upon, 94 percent of 200 randomly selected posts in total. [N.B., the paper says 93 percent.]

# Categories 3, 5, and 4 are "non-argumentative praise or suggestions," "factual reporting," and "cheerleading for China," respectively.
# Categories 1 and 2 are "taunting of foreign countries" and "argumentative praise or criticism," respectively (?).
# Category 6 is "other." From the paper, "Irrelevant posts that are entirely personal, commercial (such as ads), jokes, or empty posts that forward information not included. This category is removed and conditioned on in all analyses in this article."

# Make a small subset to speed up computation later.

df_small <- sample_n(df, 5000)

# Do the same for the 5,584 exclusive account posts.

setwd("C:/Users/Gabriel/Desktop/KPR 2017/PostDataSets")
df_exclusive <- read_csv("knownWeibos_zg.csv") %>% 
  clean_names() %>% 
  mutate(text = textseg,
         id = account_id,
         category = factor(category)) %>% 
  mutate(labeled = case_when(
    !is.na(category) ~ 1,
    TRUE ~ 0
  )) %>% 
  filter(exclusive == 1) %>% 
  select(id, text, category, labeled)

################################
### REPLICATION WITH README 2.0
################################
### This section replicates the main paper results from the known leaked 50c archive (classifying 44k posts).

# Read in the Chinese word vector file.
# From https://fasttext.cc/docs/en/crawl-vectors.html.

setwd("C:/Users/Gabriel/Documents/R/win-library/3.5/readme")
wv <- read_lines(file = "cc.zh.300.vec") %>% 
  as_tibble()

# Drop the first random row.

wv <- wv[2:nrow(wv),]

# Split the word vector into a 300-dimensional matrix.
# Keep only the first 100,000 words out of 2 million for computational speed.

wv_split <- separate(wv[1:100000,], value, into = as.character(seq(1:300)), sep = " ", convert = TRUE)

# Save the file for future use.

write_rds(wv_split, "wv_split.rds")
# setwd("C:/Users/Gabriel/Documents/R/win-library/3.5/readme")
# wv_split <- read_rds("wv_split.rds")

# Turn the word vectors into a matrix that readme::undergrad() can use.

wv_dm <- as.matrix(wv_split[,2:300])
row.names(wv_dm) <- wv_split$`1`

# Create a document feature matrix.

dfm <- undergrad(documentText = df$text, wordVecs = wv_dm)
# write_rds(dfm, "dfm.rds")
# setwd("C:/Users/Gabriel/Dropbox/HKS/Courses/Spring 2019/Gov 1006 - Models/kpr_2017")
# dfm <- read_rds("dfm.rds")

# Run ReadMe2 on the original human-labeled categories.

iterations <- 3

results_original_table <- tibble(n = 1:iterations, `3` = NA, `4` = NA, `5` = NA)

for (n in 1:iterations) {
  
  results_original <- readme(dfm = dfm,
                             labeledIndicator = df$labeled,
                             categoryVec = df$category,
                             verbose = TRUE)
  
  # Save the results to create a confidence interval.
  
  results_original_table$`3`[n] <- results_original$point_readme[1]
  results_original_table$`4`[n] <- results_original$point_readme[2]
  results_original_table$`5`[n] <- results_original$point_readme[3]
}

# Repeat the process for exclusive posts.
# First save a dfm for the exclusive posts.

# dfm_exclusive <- undergrad(documentText = df_exclusive$text, wordVecs = wv_dm)
# write_rds(dfm_exclusive, "dfm_exclusive.rds")
# dfm_exclusive <- read_rds("dfm_exclusive.rds")

results_exclusive_table <- tibble(n = 1:iterations,
                                  `Factual Reporting` = NA,
                                  `Argumentative praise or criticism` = NA,
                                  `Cheerleading for China` = NA,
                                  `Other` = NA,
                                  `Non-argumentative Praise or Suggestions` = NA)

for (n in 1:iterations) {
  results_exclusive <- readme(dfm = dfm_exclusive,
                             labeledIndicator = df_exclusive$labeled,
                             categoryVec = df_exclusive$category,
                             verbose = TRUE)
  
  # Save the results to create a confidence interval.
  
  results_exclusive_table$`Argumentative praise or criticism`[n] <- results_exclusive$point_readme[1]
  results_exclusive_table$`Cheerleading for China`[n] <- results_exclusive$point_readme[2]
  results_exclusive_table$`Factual Reporting`[n] <- results_exclusive$point_readme[3]
  results_exclusive_table$`Non-argumentative Praise or Suggestions`[n] <- results_exclusive$point_readme[4]
  results_exclusive_table$`Other`[n] <- results_exclusive$point_readme[5]
}


###############################
### LATENT DIRICHLET ALLOCATION
###############################
### This section uses Latent Dirichlet Allocation to create 5 unsupervised categories to later "seed" ReadMe with.
### It uses the exclusive posts to reduce computation time.

# Use a comprehensive list of Chinese stopwords.
# Downloaded from https://github.com/stopwords-iso/stopwords-zh.

setwd("C:/Users/Gabriel/Dropbox/HKS/Courses/Spring 2019/Gov 1006 - Models/kpr_2017")
stop_cn <- read_delim("stopwords-zh.txt", delim = "\n", col_names = FALSE) %>% 
  mutate(words = X1) %>% 
  select(words)

# Turn the content into a corpus with the tm package.

cp <- Corpus(VectorSource(df_small$text)) %>% 
  
  # Strip out all of the stopwords and punctuation.
  
  tm_map(removePunctuation, ucp = TRUE) %>% 
  tm_map(removeWords, stop_cn[[1]])

# See the cleaned text with this.
# cleaned_text <- tibble(text = get("content", cp))

# Save the corpus into a quanteda corpus object in order to get bigrams.

# cp_bi <- corpus(cp)

# Create document term matrices, one with counts, one with tf-idf, and one with bigrams.
# Only use the bigram version for the final implementation.

dtm <- DocumentTermMatrix(cp)
# dtm_tfidf <- DocumentTermMatrix(cp, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
# dtm_bi <- dfm(cp_bi, ngrams = 2) %>% convert(to = "tm")

# Remove sparse terms from both, which reduces the words from about 22,000 to 2,000.
# At sparse = .999, the function removes words that appear in less than 1 in every 1,000 documents.

dtm_clean <- removeSparseTerms(dtm, .999)
# dtm_tfidf_clean <- removeSparseTerms(dtm_tfidf, .999)
# dtm_bi_clean <- removeSparseTerms(dtm_bi, .999)

# Eliminate documents without any of the common terms.

rowtotals1 <- apply(dtm_clean, 1, sum)
# rowtotals2 <- apply(dtm_tfidf_clean, 1, sum)
# rowtotals3 <- apply(dtm_bi_clean, 1, sum)

# The frequency dtm now has 5,572 documents.

dtm_clean   <- dtm_clean[rowtotals1 > 0, ]
# dtm_tfidf_clean   <- dtm_tfidf_clean[rowtotals2 > 0, ]
# dtm_bi_clean   <- dtm_bi_clean[rowtotals3 > 0, ]

# Implement LDA with the single term matrix.
# Basic explanation here: https://medium.com/@lettier/how-does-lda-work-ill-explain-using-emoji-108abf40fa7d.

mod_lda <- LDA(dtm_clean, 5)
# mod_lda_bi <- LDA(dtm_bi_clean, 5)

# Save the LDA models for future use.
saveRDS(mod_lda, "mod_lda")
# mod_lda <- read_rds("mod_lda")
# saveRDS(mod_lda_bi, "mod_lda_bi")
# mod_lda_bi <- read_rds("mod_lda_bi")

# Get information about each post.
# See page 10: https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf

post <- posterior(mod_lda)
topics <- topics(mod_lda) %>% 
  as_tibble()

# Show the percentage breakdown of the 5 topics.

lda_percents <- count(topics, value) %>% 
  mutate(percent = n / nrow(topics))

# Find the top terms by topic.

top_terms <- tidy(mod_lda) %>% 
  group_by(topic) %>% 
  top_n(8, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

# Merge the LDA classifications with the original dataframe for later sampling.

# First capture the assigned classes of the posts (since some were excluded).

results_lda_clean <- tidy(dtm_clean)
results_lda_clean <- tibble(doc = unique(results_lda_clean$document),
                         lda_class = topics$value)

# Merge it with all of the posts (some of which have no class because they were too sparse).

results_lda <- tidy(dtm)
results_lda <- tibble(doc = unique(results_lda$document)) %>% 
  left_join(results_lda_clean, by = "doc") %>% 
  mutate(n = as.numeric(str_remove(doc, "text")))
                        
# Make a final df with the LDA classes.

df_lda <- df_small %>% 
  mutate(n = row_number()) %>% 
  left_join(results_lda, by = "n") %>% 
  select(-doc) %>% 

# Update the labeled variable.
  
  mutate(labeled = case_when(
    !is.na(lda_class) ~ 1,
    TRUE ~ 0
  ))


########################
### SEED README WITH LDA
########################
# Randomly select 188 posts from the LDA-classified posts to "seed" ReadMe with, and then collect the results.

# First clear the df of the human-coded posts.

df_blank <- df_lda %>% 
  mutate(category = NA,
         lda_class = NA,
         labeled = 0)

# Take a few samples of the ReadMe results.

iterations <- 5

results_seeded_table <- tibble(n = 1:iterations, `1` = NA, `2` = NA, `3` = NA, `4` = NA, `5` = NA)

for (n in 1:iterations) {

# Sample 188 seeds.

seeds <- df_lda %>% 
  filter(! is.na(lda_class)) %>% 
  sample_n(188)

seed_numbers <- c(seeds$n)

# Eliminate the seeds from the original dataset and add in the coded versions.

df_seeded <- df_blank[-seed_numbers,] %>% 
  rbind(seeds)

# Make a dfm for the seeded table.

dfm_seeded <- undergrad(documentText = df_seeded$text, wordVecs = wv_dm)

# Run ReadMe2 with the seeded posts.

results_seeded <- readme(dfm = dfm_seeded,
                           labeledIndicator = df_seeded$labeled,
                           categoryVec = df_seeded$lda_class,
                           verbose = TRUE)

# Save the results to create a confidence interval.

results_seeded_table$`1`[n] <- results_seeded$point_readme[1]
results_seeded_table$`2`[n] <- results_seeded$point_readme[2]
results_seeded_table$`3`[n] <- results_seeded$point_readme[3]
results_seeded_table$`4`[n] <- results_seeded$point_readme[4]
results_seeded_table$`5`[n] <- results_seeded$point_readme[5]
}


######################
### EXPORT THE RESULTS
######################

saveRDS(lda_percents, "results/lda_percents.rds")
saveRDS(top_terms, "results/top_terms")
saveRDS(results_original_table, "results/results_original_table")
saveRDS(results_exclusive_table, "results/results_exclusive_table")
saveRDS(results_seeded_table, "results/results_seeded_table")

# Make the original results plottable.

means <- c(0,
           0,
           mean(results_original_table$`3`),
           mean(results_original_table$`5`),
           mean(results_original_table$`4`),
           0,
           mean(results_exclusive_table$`Argumentative praise or criticism`),
           mean(results_exclusive_table$`Non-argumentative Praise or Suggestions`),
           mean(results_exclusive_table$`Factual Reporting`),
           mean(results_exclusive_table$`Cheerleading for China`),
           0,
           0,
           .16,
           .08,
           .8,
           0,
           .06,
           .31,
           .19,
           .45)

lower <- c(0,
           0,
           mean(results_original_table$`3`) - 1.96/sqrt(3)*sd(results_original_table$`3`),
           mean(results_original_table$`5`) - 1.96/sqrt(3)*sd(results_original_table$`5`),
           mean(results_original_table$`4`) - 1.96/sqrt(3)*sd(results_original_table$`4`),
           0,
           mean(results_exclusive_table$`Argumentative praise or criticism`) - 1.96/sqrt(3)*sd(results_exclusive_table$`Argumentative praise or criticism`),
           mean(results_exclusive_table$`Non-argumentative Praise or Suggestions`) - 1.96/sqrt(3)*sd(results_exclusive_table$`Non-argumentative Praise or Suggestions`),
           mean(results_exclusive_table$`Factual Reporting`) - 1.96/sqrt(3)*sd(results_exclusive_table$`Factual Reporting`),
           mean(results_exclusive_table$`Cheerleading for China`) - 1.96/sqrt(3)*sd(results_exclusive_table$`Cheerleading for China`),
           0,
           0,
           .08,
           .02,
           .7,
           0,
           0,
           .22,
           .13,
           .4)

upper <- c(0.0025,
           0.0025,
           mean(results_original_table$`3`) + 1.96/sqrt(3)*sd(results_original_table$`3`),
           mean(results_original_table$`5`) + 1.96/sqrt(3)*sd(results_original_table$`5`),
           mean(results_original_table$`4`) + 1.96/sqrt(3)*sd(results_original_table$`4`),
           0.0025,
           mean(results_exclusive_table$`Argumentative praise or criticism`) + 1.96/sqrt(3)*sd(results_exclusive_table$`Argumentative praise or criticism`),
           mean(results_exclusive_table$`Non-argumentative Praise or Suggestions`) + 1.96/sqrt(3)*sd(results_exclusive_table$`Non-argumentative Praise or Suggestions`),
           mean(results_exclusive_table$`Factual Reporting`) + 1.96/sqrt(3)*sd(results_exclusive_table$`Factual Reporting`),
           mean(results_exclusive_table$`Cheerleading for China`) + 1.96/sqrt(3)*sd(results_exclusive_table$`Cheerleading for China`),
           .0025,
           .0025,
           .21,
           .15,
           .88,
           .0025,
           .1,
           .4,
           .25,
           .5)

groups <- c(rep("Leaked", 5), rep("Exclusive", 5), rep("Leaked (original)", 5), rep("Exclusive (original)", 5))

tibble(
  category = rep(c("Taunting of foreign countries", "Argumentative praise or criticism", "Nonargumentative praise or suggestions", "Factual reporting", "Cheerleading"), 4),
  mean = means,
  lower = lower,
  upper = upper,
  group = groups,
  x = c(.9, 1.9, 2.9, 3.9, 4.9, 1, 2, 3, 4, 5, 1.1, 2.1, 3.1, 4.1, 5.1, 1.2, 2.2, 3.2, 4.2, 5.2)
) %>% 

# Graph the original results.

  ggplot(aes(x, mean, col = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = .3) +
  geom_point(aes(col = group), size = 4) +
  geom_segment(aes(x = x, y = lower, xend = x, yend = upper), size = 1, alpha = .5) +
  scale_color_discrete(name = "Post source") +
  scale_x_discrete(limits = c(1, 2, 3, 4, 5),
                   labels = c("1" = "Taunting of \n foreign countries",
                              "2" = "Argumentative praise \n or criticism",
                              "3" = "Nonargumentative praise \n or suggestions",
                              "4" = "Factual reporting",
                              "5" = "Cheerleading")) +
  theme(
    text = element_text(size = 16, family = "LM Roman 10"),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_blank(),
    legend.key = element_blank()) +
  labs(y = "Estimated proportion of total posts",
    title = "The ReadMe2 results match well with those from the original paper.")
