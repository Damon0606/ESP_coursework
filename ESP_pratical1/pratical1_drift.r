# Practical 1: A Markov Ulysses
# Finished by: Group 15: Huantong Hou (s2481591), Yuqi Shi, Zukai Li (s2505721)
# Contribution:
#### Huantong Hou (s2481591):
#### Yuqi Shi:
#### Zukai Li (s2505721):


# ———————————————————————————————————————————————————————————————————————————————
# Step 3
# ———————————————————————————————————————————————————————————————————————————————
setwd("/Users/houhuantong/Edinburgh/S1/Extended Statistical Programming/Groupwork") ## comment out of submitted
W <- scan("4300-0.txt", what = "character", skip = 73, nlines = 32858 - 73)
W <- gsub("_(", "", W, fixed = TRUE) ## remove "_("
W <- gsub(")_", "", W, fixed = TRUE) ## remove ")_"


# ———————————————————————————————————————————————————————————————————————————————
# Step 4
# ———————————————————————————————————————————————————————————————————————————————
remove_punctuation <- function(words) {
  words <- gsub("[-—_*()\\.{3}]", "", words) ## remove "[()*-—_]"
  words <- words[words != ""] ## remove null
  return(words)
}
W <- remove_punctuation(W) ## remove useless punctuation
### Unable to deal with situation that words containing two punctuation, e.g. ""
split_punct <- function(words, punctuation) {
  words_index <- grep("[,.;!:?]", words) ## indices of words containing "[,.;!:?]"
  punc_index <- words_index + 1:length(words_index) ## add new entries for words containing punctuation
  words_modified <- rep("", length(words) + length(punc_index)) ## vector to store split words and punctuation
  ## insert punctuation and containing punctuation (e.g."look" ".")
  words_modified[-punc_index] <- sapply(words, function(x) ifelse(x %in% words[words_index], substr(x, 1, nchar(x) - 1), x))
  ## insert the rest words
  words_modified[punc_index] <- sapply(words[words_index], function(x) substr(x, nchar(x), nchar(x)))
  return(words_modified)
}


# ———————————————————————————————————————————————————————————————————————————————
# Step 5
# ———————————————————————————————————————————————————————————————————————————————
W_clean <- split_punct(W, punctuation) ## split words and punctuation


# ———————————————————————————————————————————————————————————————————————————————
# Step 6
# ———————————————————————————————————————————————————————————————————————————————
Unique <- unique(tolower(W_clean)) ## 6(a)vector of unique words
Index <- match(tolower(W_clean), Unique) ## 6(b)indices indicating positions of words in the text corresponding to unique vector
Frequency <- as.data.frame(table(Index)) ## 6(c)occurrence frequency of unique words in the text
sorted_Frequency <- Frequency[order(-Frequency$Freq), ] ## sort occurrence frequency in descending order
threshold <- sorted_Frequency$Freq[1000] ## 6(d)threshold number of occurrences
boundary <- max(which(sorted_Frequency$Freq == threshold))
Frequency_1000 <- sorted_Frequency[1:boundary, ]
b <- Unique[Frequency_1000$Index] ## most commonly occurring words


# ———————————————————————————————————————————————————————————————————————————————
# Step 7
# ———————————————————————————————————————————————————————————————————————————————
first_col <- second_col <- third_col <- c() ## define vectors for three column matrix
Index_a_common <- match(tolower(W_clean), b) ## 7(a)indices indicating positions of words in the text corresponds to b
for (i in 1:length(Frequency_1000$Index)) { ## 7(b)
  a_b_position <- which(Index_a_common[] == Frequency_1000$Index[i]) ##
  first_col <- as.numeric(append(matrix(rep(Frequency_1000$Index[i], length(a_b_position)), ncol = 1), first_col)) ## index of common words
  second_col <- as.numeric(append(matrix(Index_a_common[a_b_position + 1], ncol = 1), second_col)) ## index for the following word
  third_col <- as.numeric(append(matrix(Index_a_common[a_b_position + 2], ncol = 1), third_col)) ## index for the next following word
}
## 7(c)
Tri <- cbind(first_col, second_col, third_col) ## define Triplets
Tri_rowsum <- rowSums(Tri, na.rm = FALSE)
Tri_final <- Tri[which(!is.na(Tri_rowsum)), ] ## drop triplets containing NA
## 7(d)same for Pairs
P <- cbind(first_col, second_col)
P_rowsum <- rowSums(P, na.rm = FALSE)
P_final <- P[which(!is.na(P_rowsum)), ]

## remove specific illogical situations in Pairs & Triplets
the_position <- which(b[] == "the") ## index of 'the'
position <- which(P_final[, 1] == the_position & P_final[, 2] == the_position)
P_final <- P_final[-position, ] ## remove 'the the'

sample_words <- function(all_words, n, type) {
  Frenquency_table <- as.data.frame(table(all_words[, type]))
  Frenquency_table$word_freq <- Frenquency_table[, 2] / sum(Frenquency_table[, 2])
  index_TEMP <- as.integer(as.character(Frenquency_table[, 1]))
  if (length(index_TEMP) == 1) {
    index_temp <- index_TEMP
  } else {
    index_temp <- sample(index_TEMP, size = n, prob = as.double(Frenquency_table$word_freq))
  }
  return(index_temp)
}

sample_50_words <- c() ## vector for 50-word sections
sample_50_words[1] <- sample(unique(P_final[, 1]), size = 1) ## 8(a)randomly select from P_final
sample_50_words[2] <- sample_words(matrix(P_final[P_final[, 1] == sample_50_words[1], ], ncol = 2), 1)

# Simulate the rest of the 48 words
for (i in 3:50) {
  ## 8(b) extract sub-matrix[2] from Triplets
  all_third_words <- matrix(Tri_final[(Tri_final[, 1] == sample_50_words[i - 2]) &
                                        (Tri_final[, 2] == sample_50_words[i - 1]), ], ncol = 3)
  all_second_words <- matrix(P_final[P_final[, 1] == sample_50_words[i - 1], ], ncol = 2)
  ## 8(c)
  if (length(all_third_words) != 0) { ## if sub-matrix has rows
    sample_50_words <- append(sample_50_words, sample_words(all_third_words, 1, 3))
    cat(i, "3", index_temp3, "\n")
  } else if (length(all_second_words) != 0) { ## if sub-matrix has no rows
    sample_50_words <- append(sample_50_words, sample_words(all_second_words, 1, 2)) ## simulate from Pairs
    cat(i, "2", index_temp2, "\n")
  } else { ## if neither Triplets nor Pairs have rows
    sample_50_words <- append(sample_50_words, sample(unique(P_final[, 1]), 1)) ## simulate based on probability
    cat(i, "1", index_temp1, "\n")
  }
}

section8 <- paste(b[sample_50_words], collapse = " ") ## combined into a sentence
section8 <- gsub("\\s+(?=[[:punct:]])", "", section8, perl = TRUE)
cat(section8)


# ———————————————————————————————————————————————————————————————————————————————
# Step 9
# ———————————————————————————————————————————————————————————————————————————————
# Frequency_1000$b_freq <- Frequency_1000$Freq / sum(Frequency_1000$Freq)
words_sections9 <- sample(Frequency_1000[, 1], size = 50, prob = Frequency_1000[, 2]) ## simulate indices based on common frequencies
words_sections9 <- Unique[words_sections9] ## words based on simulation indices
section9 <- paste(words_sections9, collapse = " ") ## combined into a sentence
section9 <- gsub("\\s+(?=[[:punct:]])", "", section9, perl = TRUE)
cat(section9)


# ———————————————————————————————————————————————————————————————————————————————
# Step 10
# ———————————————————————————————————————————————————————————————————————————————
CapitalWords <- c()
pattern <- "\\b^[A-Z]\\w*\\b"
Capital_Unique <- unique(W_clean)
# 遍历每个单词
for (w in Capital_Unique) {
  matches <- str_extract_all(w, pattern)[[1]]# 检查每个单词是否匹配模式
  CapitalWords <- append(CapitalWords, matches)
}
Capi_Index <- match(CapitalWords, W_clean) ## 6(b)indices indicating positions of words in the text corresponding to unique vector
Capi_Frequency <- as.data.frame(table(Capi_Index)) ## 6(c)occurrence frequency of unique words in the text
Capi_sorted_Frequency <- Capi_Frequency[order(-Capi_Frequency$Freq), ] ## sort occurrence frequency in descending order
Capi_threshold <- Capi_sorted_Frequency$Freq[10] ## 6(d)threshold number of occurrences
Capi_boundary <- max(which(Capi_sorted_Frequency$Freq == Capi_threshold))
Capi_Frequency_10 <- Capi_sorted_Frequency[1:Capi_boundary, ]
Capi_b <- W_clean[as.integer(as.character(Capi_Frequency_10[, 1]))] ## most commonly occurring words

LowerSection <- b[sample_50_words]
for (i in (1:length(LowerSection))){
  wd <- LowerSection[i]
  if (wd %in% tolower(Capi_b)){
    LowerSection[i] <- str_to_title(wd)
  }
}
Lowersection8 <- paste(LowerSection, collapse = " ") ## combined into a sentence
Lowersection8 <- gsub("\\s+(?=[[:punct:]])", "", Lowersection8, perl = TRUE)
cat(Lowersection8)

