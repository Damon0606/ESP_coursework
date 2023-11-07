setwd("/workspaces/ESP_coursework/ESP_pratical1/Original_Data") ## comment out of submitted

W <- scan("4300-0.txt", what = "character", skip = 73, nlines = 32858 - 73)
W <- gsub("_(", "", W, fixed = TRUE) ## remove "_("
W <- gsub(")_", "", W, fixed = TRUE) ## remove ")_"
# View(W)


# Step 4
### 不能处理一个词中同时出现两个符号的情况
split_punct <- function(words, punctuation) {
  words_index <- grep("[,.;!:?]", words) # 处理单词结尾是符号[,.;!:?]的情况
  punc_index <- words_index + 1:length(words_index)
  words_modified <- rep("", length(words) + length(punc_index))
  words_modified[-punc_index] <- sapply(words, function(x) ifelse(x %in% words[words_index], substr(x, 1, nchar(x) - 1), x))
  words_modified[punc_index] <- sapply(words[words_index], function(x) substr(x, nchar(x), nchar(x)))
  
  words_index_hyphen <- grep("[-—_]", words) # 处理单词开头是符号[-—_]的情况
  punc_index_hyphen <- words_index_hyphen + 1:length(words_index_hyphen) # 单词应该在的位置
  words_modified_hyphen <- rep("", length(words) + length(punc_index_hyphen))
  words_modified[-punc_index_hyphen] <- sapply(words, function(x) ifelse(x %in% words[words_index_hyphen], substr(x, 1, 1), x)) # 放符号
  words_modified[punc_index_hyphen] <- sapply(words[words_index_hyphen], function(x) substr(x, 2, nchar(x))) # 放单词
  return(words_modified)
}


remove_punctuation <- function(words) {
  # hyphen_index <- grepl("\\b-+\\w|\\b—+\\w|\\b_+\\w", words) # 处理单词开头包含[-—_]的情况
  # words[hyphen_index] <- sapply(words[hyphen_index], function(x) substr(x, 2, nchar(x))) # 删除单词开头的[-—_]
  words <- gsub("[*)]", "", words) # 删除文本数据中的[(*)]
  words <- words[words != ""]
  return(words)
}




# Step 5
W_splited <- split_punct(W, punctuation)
W_splited

remove_punctuation <- function(words) {
  # hyphen_index <- grepl("\\b-+\\w|\\b—+\\w|\\b_+\\w", words) # 处理单词开头包含[-—_]的情况
  # words[hyphen_index] <- sapply(words[hyphen_index], function(x) substr(x, 2, nchar(x))) # 删除单词开头的[-—_]
  words <- gsub("[*)]", "", words) # 删除文本数据中的[(*)]
  words <- words[words != ""]
  return(words)
}

W_clean <- remove_punctuation(W_splited)
W_clean


# Step 6
# (a)
# 去重
Unique <- unique(tolower(W_clean))

# (b)
# 单词对应的index
Index <- match(tolower(W_clean), Unique)

# (c)
# index对应的频次
Frequency <- tabulate(Index)

# (d)
# 重新排序
SortedFreq <- Frequency[order(Frequency, decreasing = TRUE)]

# 提取常见词
target_values <- SortedFreq[1:1000] # 常见词出现频次值
target_positions <- which(Frequency %in% target_values) # 找回常见词对应频次在index对应的频次表中的位置
Frequency[target_positions] # 提取常见词对应的index

# (e)
b <- Unique[Index[Frequency[target_positions]]] # 提取常见词index对应的单词

# Check example: was
# was_index <- which(Unique == "was")
# was_frequency <- Frequency[24]
# was_rank <- which(SortedFreq == was_frequency)

# Step 6-2
Unique <- unique(tolower(W_clean)) ##去重
Index <- match(tolower(W_clean), Unique) #a中单词在unique的位置
Frequency <- as.data.frame(table(Index))#统计在a中unique各单词各出现了多少次
sorted_Frequency <- Frequency[order(-Frequency$Freq), ]
Frequency_1000 <- sorted_Frequency[1:1000, ]
b <- Unique[Frequency_1000$Index]

# Step 7
first_col<-c()
second_col<-c()
third_col<-c()
Index_a_common <- match(tolower(W_clean),b)
for (i in 1:length(Frequency_1000$Index)) {
  a_b_position<- which(Index_a_common[] == Frequency_1000$Index[i])
  first_col <- as.numeric(append(matrix(rep(Frequency_1000$Index[i],length(a_b_position)),ncol = 1),first_col))
  second_col<-as.numeric(append(matrix(Index_a_common[a_b_position+1],ncol = 1),second_col))
  third_col<-as.numeric(append(matrix(Index_a_common[a_b_position+2],ncol = 1),third_col))
}
Tri<- cbind(first_col,second_col,third_col)# 创建三元组矩阵
P<-cbind(first_col,second_col)# 创建二元组矩阵
Tri_rowsum <- rowSums(Tri,na.rm = FALSE)
Tri_all_common_words <- which(!is.na(Tri_rowsum))
P_rowsum <- rowSums(P,na.rm = FALSE)
P_all_common_words <- which(!is.na(P_rowsum))
Tri_final <- Tri[Tri_all_common_words,]
P_final <- P[P_all_common_words,]









# 第一版完整初稿从这里开始：
# ———————————————————————————————————————————————————————————————————————————————
# Step 3
# ———————————————————————————————————————————————————————————————————————————————
setwd("/Users/l/Desktop/Edinburgh/Extended_Statistical_Programming/ESP_coursework/ESP_pratical1/Original_Data") ## comment out of submitted
W <- scan("4300-0.txt", what = "character", skip = 73, nlines = 32858 - 73)
W <- gsub("_(", "", W, fixed = TRUE) ## remove "_("
W <- gsub(")_", "", W, fixed = TRUE) ## remove ")_"
# View(W)


# ———————————————————————————————————————————————————————————————————————————————
# Step 4
# ———————————————————————————————————————————————————————————————————————————————
remove_punctuation <- function(words) {
  # hyphen_index <- grepl("\\b-+\\w|\\b—+\\w|\\b_+\\w", words) # 处理单词开头包含[-—_]的情况
  # words[hyphen_index] <- sapply(words[hyphen_index], function(x) substr(x, 2, nchar(x))) # 删除单词开头的[-—_]
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
  # words_index_hyphen <- grep("[-—_]", words) # 处理单词开头是符号[-—_]的情况
  # punc_index_hyphen <- words_index_hyphen + 1:length(words_index_hyphen) # 单词应该在的位置
  # words_modified_hyphen <- rep("", length(words) + length(punc_index_hyphen))
  # words_modified[-punc_index_hyphen] <- sapply(words, function(x) ifelse(x %in% words[words_index_hyphen], substr(x, 1, 1), x)) # 放符号
  # words_modified[punc_index_hyphen] <- sapply(words[words_index_hyphen], function(x) substr(x, 2, nchar(x))) # 放单词
  return(words_modified)
}

# # Test
# text <- c("An", "omnishambles,", "in)", "a.", "_headless", "chicken's", "-factory", "b...")
# text <- remove_punctuation(text)
# text
# text <- split_punct(text, punctuation)
# text

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

# Frequency_1000 <- sorted_Frequency[1:1000, ]

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


# ———————————————————————————————————————————————————————————————————————————————
# Step 8
# ———————————————————————————————————————————————————————————————————————————————
#### 10.4 version
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
#####################

sample_words <- function(all_words, n, Dictionary) {
  Frenquency_table <- as.data.frame(table(all_words))
  Frenquency_table$word_freq <- Frenquency_table[, 2] / sum(Frenquency_table[, 2])
  word_index <- sample(seq_along(Frenquency_table[, 1]), size = n, replace = TRUE, prob = Frenquency_table$word_freq)
  word <- Dictionary[word_index]
  return(c(word_index, word))
}

word_sections <- c()
word_sections_index <- c()

# Simulate the first word of the 50 word sections
first_word_index <- sample_words(Frequency_1000[, 1], 1, Unique)[1]
first_word <- sample_words(Frequency_1000[, 1], 1, Unique)[2]
word_sections_index[1] <- first_word_index
word_sections[1] <- first_word
# Simulate the second word
all_second_words <- P_final[P_final[, 1] == first_word_index]
if (length(all_second_words) != 0) {
  second_word_index <- sample_words(all_second_words, 1, Unique)[1]
  second_word <- sample_words(all_second_words, 1, Unique)[2]
} else {
  second_word_index <- sample_words(Frequency_1000[, 1], 1, Unique)[1]
  second_word <- sample_words(Frequency_1000[, 1], 1, Unique)[2]
}
word_sections_index[2] <- second_word_index
word_sections[2] <- second_word
# Simulate the rest of the 48 words
for (i in 3:50) {
  all_third_words <- Tri_final[(Tri_final[, 1] == word_sections_index[i - 2]) & (Tri_final[, 2] == word_sections_index[i - 1])]
  all_second_words <- P_final[P_final[, 1] == word_sections_index[i - 1]]
  if (length(all_third_words) != 0) {
    current_word_index <- sample_words(all_third_words, 1, Unique)[1]
    current_word <- sample_words(all_third_words, 1, Unique)[2]
  } else if (length(all_second_words) != 0) {
    current_word_index <- sample_words(all_second_words, 1, Unique)[1]
    current_word <- sample_words(all_second_words, 1, Unique)[2]
  } else {
    current_word_index <- sample_words(Frequency_1000[, 1], 1, Unique)[1]
    current_word <- sample_words(Frequency_1000[, 1], 1, Unique)[2]
  }
  word_sections_index[i] <- current_word_index
  word_sections[i] <- current_word
}

section8 <- paste(word_sections, collapse = " ")
section8 <- gsub("\\s+(?=[[:punct:]])", "", section8, perl = TRUE)
cat(section8)
### 问题：会出现两个连续的符号

# ———————————————————————————————————————————————————————————————————————————————
# Step 9
# ———————————————————————————————————————————————————————————————————————————————
# Frequency_1000$b_freq <- Frequency_1000$Freq / sum(Frequency_1000$Freq)
words_sections9 <- sample_words(Frequency_1000[, 1], 50, Unique)[51:100]
section9 <- paste(words_sections9, collapse = " ")
section9 <- gsub("\\s+(?=[[:punct:]])", "", section9, perl = TRUE)
cat(section9)