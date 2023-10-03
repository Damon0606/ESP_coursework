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

