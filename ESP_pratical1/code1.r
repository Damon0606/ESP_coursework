setwd("/workspaces/ESP_coursework/ESP_pratical1/Original_Data") ## comment out of submitted

W <- scan("4300-0.txt", what = "character", skip = 73, nlines = 32858 - 73)
W <- gsub("_(", "", W, fixed = TRUE) ## remove "_("
# View(W)


# Step 4
# punctuation <- c(",", ".", ";", "!", ":", "?")
split_punct <- function(words, punctuation) {
  words_index <- grep("[,.;!:?]", words) # 处理单词结尾是符号[,.;!:?]的情况
  punc_index <- words_index + 1:length(words_index)
  words_modified <- rep("", length(words) + length(punc_index))
  words_modified[-punc_index] <- sapply(words, function(x) ifelse(x %in% words[words_index], substr(x, 1, nchar(x) - 1), x))
  words_modified[punc_index] <- sapply(words[words_index], function(x) substr(x, nchar(x), nchar(x)))
  return(words_modified)
}


# Step 5
W_splited <- split_punct(W, punctuation)
W_splited

remove_punctuation <- function(words) {
  hyphen_index <- grepl("\\b-+\\w|\\b—+\\w|\\b_+\\w", words) # 处理单词开头包含[-—_]的情况
  words[hyphen_index] <- sapply(words[hyphen_index], function(x) substr(x, 2, nchar(x))) # 删除单词开头的[-—_]
  words <- gsub("[,.;!:?_]", "", words) # 删除所有分隔出来的[,.;!:?_]
  words <- words[words != ""]
  return(words)
}

W_clean <- remove_punctuation(W_splited)
W_clean