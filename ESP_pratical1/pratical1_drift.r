# 第一版完整初稿从这里开始：
# ———————————————————————————————————————————————————————————————————————————————
# Step 3
# ———————————————————————————————————————————————————————————————————————————————
setwd("/Users/shiyuqi/Downloads") ## comment out of submitted
W <- scan("4300-0.txt", what = "character", skip = 73, nlines = 32858 - 73)
W <- gsub("_(", "", W, fixed = TRUE) ## remove "_("
W <- gsub(")_", "", W, fixed = TRUE) ## remove ")_"
# View(W)


# ———————————————————————————————————————————————————————————————————————————————
# Step 4
# ———————————————————————————————————————————————————————————————————————————————
remove_punctuation <- function(words) {
  words <- gsub("[-—_*()\\.{3}]", "", words) # 删除文本数据中的[()*-—_]
  words <- words[words != ""] # 删除空值
  return(words)
}

### 不能处理一个词中同时出现两个符号的情况
split_punct <- function(words, punctuation) {
  words_index <- grep("[,.;!:?]", words) # 处理单词结尾是符号[,.;!:?]的情况
  punc_index <- words_index + 1:length(words_index)
  words_modified <- rep("", length(words) + length(punc_index))
  words_modified[-punc_index] <- sapply(words, function(x) ifelse(x %in% words[words_index], substr(x, 1, nchar(x) - 1), x))
  words_modified[punc_index] <- sapply(words[words_index], function(x) substr(x, nchar(x), nchar(x)))
  return(words_modified)
}

# ———————————————————————————————————————————————————————————————————————————————
# Step 5
# ———————————————————————————————————————————————————————————————————————————————
W <- remove_punctuation(W)
W_clean <- split_punct(W, punctuation)
W_clean

# ———————————————————————————————————————————————————————————————————————————————
# Step 6
# ———————————————————————————————————————————————————————————————————————————————
Unique <- unique(tolower(W_clean)) ## 去重
Index <- match(tolower(W_clean), Unique) # W_clean中单词在unique的位置
Frequency <- as.data.frame(table(Index)) # 统计在W_clean中unique各单词各出现了多少次
sorted_Frequency <- Frequency[order(-Frequency$Freq), ] # 把所有词的频次从大到小排序

threshold <- sorted_Frequency$Freq[1000]
threshold
boundary <- max(which(sorted_Frequency$Freq == threshold))
boundary
Frequency_1000 <- sorted_Frequency[1:boundary, ]
b <- Unique[Frequency_1000$Index]


# ———————————————————————————————————————————————————————————————————————————————
# Step 7
# ———————————————————————————————————————————————————————————————————————————————
first_col <- c()
second_col <- c()
third_col <- c()
Index_a_common <- match(tolower(W_clean), b)
for (i in 1:length(Frequency_1000$Index)) {
  a_b_position <- which(Index_a_common[] == Frequency_1000$Index[i])
  first_col <- as.numeric(append(matrix(rep(Frequency_1000$Index[i], length(a_b_position)), ncol = 1), first_col))
  second_col <- as.numeric(append(matrix(Index_a_common[a_b_position + 1], ncol = 1), second_col))
  third_col <- as.numeric(append(matrix(Index_a_common[a_b_position + 2], ncol = 1), third_col))
}

Tri <- cbind(first_col, second_col, third_col) # 创建三元组矩阵
Tri_rowsum <- rowSums(Tri, na.rm = FALSE)
Tri_all_common_words <- which(!is.na(Tri_rowsum))
Tri_final <- Tri[Tri_all_common_words, ]

P <- cbind(first_col, second_col) # 创建二元组矩阵
P_rowsum <- rowSums(P, na.rm = FALSE)
P_all_common_words <- which(!is.na(P_rowsum))
P_final <- P[P_all_common_words, ]

##delete 'the the'
the_position <- which(b[]=='the')
position <- which(P_final[, 1] == the_position & P_final[, 2] == the_position)
P_final<-P_final[-position,] 

# ———————————————————————————————————————————————————————————————————————————————
# Step 8
# ———————————————————————————————————————————————————————————————————————————————
sample_words <- function(all_words, n) {
  Frenquency_table <- as.data.frame(table(all_words))
  Frenquency_table$word_freq <- Frenquency_table[, 2] / sum(Frenquency_table[, 2])
  word_index <- sample(seq_along(Frenquency_table[, 1]), 
                       size = n, replace = TRUE, prob = Frenquency_table$word_freq)
  return(word_index)
}
sample_50_words<-c()
##随机抽取第一个词，从P_final的第一列中抽取
##随机抽取第一个词，从P_final的第一列中抽取
sample_50_words[1]<-sample(unique(P_final[,1]),size = 1)
##第二词以第一个词为基准，找出频率最高的第二个词
all_second_words_1<-P_final[P_final[, 1] == sample_50_words[1], ]
sample_50_words[2] <- sample_words(all_second_words_1, 1)

# Simulate the rest of the 48 words
for (i in 3:50) {
  all_third_words <- Tri_final[(Tri_final[, 1] == sample_50_words[i - 2]) & (Tri_final[, 2] == sample_50_words[i - 1])]
  all_second_words <- P_final[P_final[, 1] == sample_50_words[i - 1]]
  if (length(all_third_words) != 0) {
    sample_50_words<-append(sample_50_words,sample_words(all_third_words, 1))
  } else if (length(all_second_words) != 0) {
    sample_50_words<-append(sample_50_words,sample_words(all_second_words, 1))
  } else {
    sample_50_words<- append(sample_50_words,sample(unique(P_final[,1]),size = 1))
    print(i)
  }
}
section8 <- paste(b[sample_50_words], collapse = " ")
section8 <- gsub("\\s+(?=[[:punct:]])", "", section8, perl = TRUE)
cat(section8)
### 问题：会出现两个连续的符号

# ———————————————————————————————————————————————————————————————————————————————
# Step 9
# ———————————————————————————————————————————————————————————————————————————————
# Frequency_1000$b_freq <- Frequency_1000$Freq / sum(Frequency_1000$Freq)
words_sections9 <- sample_words(Frequency_1000[, 1], 50)
words_sections9<-Unique[words_sections9]
section9 <- paste(words_sections9, collapse = " ")
section9 <- gsub("\\s+(?=[[:punct:]])", "", section9, perl = TRUE)
cat(section9)







# Practical 1: A Markov Ulysses
# Finished by: Group 15: Huantong Hou(s2481591), Yuqi Shi, Zukai Li
# Contribution:
#### Huantong Hou(s2481591):
#### Yuqi Shi:
#### Zukai Li:


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
Tri_all_common_words <- which(!is.na(Tri_rowsum)) ## identify rows containing NA
Tri_final <- Tri[Tri_all_common_words, ] ## drop triplets containing NA
## 7(d)same for Pairs
P <- cbind(first_col, second_col) 
P_rowsum <- rowSums(P, na.rm = FALSE)
P_all_common_words <- which(!is.na(P_rowsum)) 
P_final <- P[P_all_common_words, ]

## remove specific illogical situations in Pairs & Triplets
the_position <- which(b[]=='the') ## index of 'the'
position <- which(P_final[, 1] == the_position & P_final[, 2] == the_position)
P_final<-P_final[-position,] ## remove 'the the'


# ———————————————————————————————————————————————————————————————————————————————
# Step 8
# ———————————————————————————————————————————————————————————————————————————————
sample_words <- function(all_words, n) { 
  Frenquency_table <- as.data.frame(table(all_words))
  Frenquency_table$word_freq <- Frenquency_table[, 2] / sum(Frenquency_table[, 2]) ## probability of each word
  word_index <- sample(seq_along(Frenquency_table[, 1]), ## select index with a given probability
                       size = n, replace = TRUE, prob = Frenquency_table$word_freq)
  return(word_index)
}

sample_50_words<-c() ## vector for 50-word sections
sample_50_words[1]<-sample(unique(P_final[,1]),size = 1) ## 8(a)randomly select from P_final
sample_50_words[2] <- sample_words(all_second_words_1, 1) ## generate second word from Pairs
# Simulate the rest of the 48 words
for (i in 3:50) { 
  ## 8(b)extract sub-matrix from Triplets
  all_third_words <- Tri_final[(Tri_final[, 1] == sample_50_words[i - 2]) 
                               & (Tri_final[, 2] == sample_50_words[i - 1])]
  all_second_words <- P_final[P_final[, 1] == sample_50_words[i - 1]]
  ## 8(c)
  if (length(all_third_words) != 0) { ## if sub-matrix has rows
    sample_50_words<-append(sample_50_words,sample_words(all_third_words, 1)) ## simulate from Triplets
  } else if (length(all_second_words) != 0) { ## if sub-matrix has no rows
    sample_50_words<-append(sample_50_words,sample_words(all_second_words, 1)) ## simulate from Pairs
  } else { ## if neither Triplets nor Pairs have rows
    sample_50_words<- append(sample_50_words,sample(unique(P_final[,1]),size = 1)) ## simulate based on probability
    print(i)
  }
}
section8 <- paste(b[sample_50_words], collapse = " ") ## combined into a sentence
section8 <- gsub("\\s+(?=[[:punct:]])", "", section8, perl = TRUE)
cat(section8)
### 问题：会出现两个连续的符号


# ———————————————————————————————————————————————————————————————————————————————
# Step 9
# ———————————————————————————————————————————————————————————————————————————————
# Frequency_1000$b_freq <- Frequency_1000$Freq / sum(Frequency_1000$Freq)
words_sections9 <- sample_words(Frequency_1000[, 1], 50) ## simulate indices based on common frequencies
words_sections9<-Unique[words_sections9] ## words based on simulation indices
section9 <- paste(words_sections9, collapse = " ")## combined into a sentence
section9 <- gsub("\\s+(?=[[:punct:]])", "", section9, perl = TRUE)
cat(section9)
