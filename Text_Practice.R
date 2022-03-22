# "Text Practice"
# "Sara Dovalo del Río and Javier Muñoz Flores"
# Date: "23/03/2022"

# Import libraries
if (!require("readtext")) install.packages("readtext")
if (!require("quanteda")) install.packages("quanteda")
if (!require("stringi")) install.packages("stringi")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("quanteda.textstats")) install.packages("quanteda.textstats")
if (!require("quanteda.textplots")) install.packages("quanteda.textplots")
library(readtext)
library(quanteda)
library(stringi)
library(ggplot2)
library(quanteda.textstats)
library(quanteda.textplots)

#Load the data
data_text <- texts(readtext("https://www.gutenberg.org/cache/epub/67584/pg67584.txt"))
names(data_text) <- "Archibald Gracie"

# Specify start and end of the text
start_text <- stri_locate_first_fixed(data_text, "CHAPTER I\n\nTHE LAST DAY ABOARD SHIP")[1]
end_text <- stri_locate_last_fixed(data_text, "deeds.")[1]
# Check the end of the text
kwic(tokens(data_text), "deeds")

# Create the object which contains only the book
novel <- stri_sub(data_text, start_text, end_text)

# Convert to lower case
novel_lower <- char_tolower(novel)

# Split into words
titanic_word <- tokens(novel_lower, remove_punct = TRUE) %>% as.character()

# Print the first five words
titanic_word[1:5]

## TASK 1

# List of words
Words <- c("love","feeling", "smile", "desire", "wonderful","happy")
list_pwords <- list()

# List of the ocurrence of words
for (i in 1:length(Words)){
  list_pwords = c(list_pwords, length(titanic_word[which(titanic_word == Words[i])]))
}

## TASK 2

# Frequency of words
Frequency <- list_pwords %>% unlist()
# Create a matrix to an easier visualize with kable
matrix_count <- data.frame(Words, Frequency)
matrix_count

# To dfm
titanic_dfm <- dfm(novel_lower, remove_punct = TRUE)
# Frequency plot
theme_set(theme_minimal())
textstat_frequency(titanic_dfm, n = 100) %>% 
  ggplot(aes(x = rank, y = frequency)) +
  geom_point() +
  labs(x = "Frequency rank", y = "Term frequency")
textstat_frequency(titanic_dfm, n = 10)

## TASK 3

# All word frequencies sorted
sorted_titanic_freqs_t <- topfeatures(titanic_dfm, n = nfeat(titanic_dfm))
# Only the words of interest
sorted_titanic_freqs_t[c("he", "she", "him", "her")]

# Comparison
sorted_titanic_freqs_t["he"] / sorted_titanic_freqs_t["she"]
sorted_titanic_freqs_t["she"] / sorted_titanic_freqs_t["her"]

# Weigthing dfdfm
titanic_dfm_pct <- dfm_weight(titanic_dfm, scheme = "prop") * 100
dfm_select(titanic_dfm_pct, pattern = c("he", "she", "him", "her", Words))
# Plot relative frquencies
textstat_frequency(titanic_dfm[, c("he", "she", "him", "her", Words)]) %>% 
  ggplot(aes(x = reorder(feature, -rank), y = frequency)) +
  geom_bar(stat = "identity") + coord_flip() + 
  labs(x = "", y = "Term Frequency as a Percentage")

## TASK 4

# Lexical plot
textplot_xray(
  kwic(novel, pattern = "boat"),
  kwic(novel, pattern = "ice")) + 
  ggtitle("Lexical dispersion")

## TASK 5

# Identify the location of the chapter breaks
chapters_corp <- 
  corpus(data_text) %>%
  corpus_segment(pattern = "CHAPTER\\s[A-Z]*\\n", valuetype = "regex")
summary(chapters_corp, 7)

# Tidy up \n
docvars(chapters_corp, "pattern") <- stringi::stri_trim_right(docvars(chapters_corp, "pattern"))
summary(chapters_corp, n = 7)

# Rename chapters
docnames(chapters_corp) <- docvars(chapters_corp, "pattern")

## TASK 6

# Correlation analysis
chap_dfm <- dfm(chapters_corp)
dfm_weight(chap_dfm, scheme = "prop") %>% 
  textstat_simil(selection = c("gracie", "happy"), method = "correlation", margin = "features") %>%
  as.matrix() %>%
  head(2)
cor_data_df <- dfm_weight(chap_dfm, scheme = "prop") %>% 
  dfm_keep(pattern = c("gracie", "happy")) %>% 
  convert(to = "data.frame")

# sample 1000 replicates and create data frame
n <- 1000
samples <- data.frame(
  cor_sample = replicate(n, cor(sample(cor_data_df$gracie), cor_data_df$happy)),
  id_sample = 1:n
)

# plot distribution of resampled correlations
ggplot(data = samples, aes(x = cor_sample, y = ..density..)) +
  geom_histogram(colour = "black", binwidth = 0.01) +
  geom_density(colour = "red") +
  labs(x = "Correlation Coefficient", y = NULL,
       title = "Histogram of Random Correlation Coefficients with Normal Curve")

## TASK 7

# Calculate mean
mean_words <- (ntoken(chapters_corp) / ntype(chapters_corp))
sort(mean_words, decreasing = TRUE) 

# Plot without scaling
(ntoken(chapters_corp) / ntype(chapters_corp)) %>%
  plot(type = "h", ylab = "Mean word frequency")
# Scaled plot
(ntoken(chapters_corp) / ntype(chapters_corp)) %>%
  scale() %>%
  plot(type = "h", ylab = "Scaled mean word frequency")

# Calculate TTR
dfm(chapters_corp) %>% 
  textstat_lexdiv(measure = "TTR") 

## TASK 8

# Calculate proportion
hapax_measure <- rowSums(chap_dfm == 1) / ntoken(chap_dfm)
head(hapax_measure)

# Plot ratios
barplot(hapax_measure, beside = TRUE, col = "grey", names.arg = seq_len(ndoc(chap_dfm)))


