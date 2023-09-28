setwd("/workspaces/ESP_coursework/ESP_pratical1/Original_Data") ## comment out of submitted
a <- scan("4300-0.txt", what = "character", skip = 73, nlines = 32858 - 73)
a <- gsub("_(", "", a, fixed = TRUE) ## remove "_("

punctuation <- c(",", ".", ";", "!", ":", "?")
split_punct <- function(words, punctuation) {
  pattern <- paste0(punctuation, "$") # 创建匹配以标点符号结尾的模式
  sentences <- grep(pattern, words, value = TRUE) # 找到与模式匹配的单词
  
  words_index <- which(sentences) ## 返回有标点符号的单词位置
  
  words <- paste0()words[-words_index] ##在没有标点符号的单词后加空格
  
  number_total <- rep('', length(sentences) + length(words))## 建立新空向量来存放单词+符号
  punctuations_index <- words_index + 1:length(words_index)## 新向量中标点符号应该放在哪里

  number_total[punctuations_index]<-substr(words[words_index],-1,-1)## 截取所有标点符号并放入
  ## 放没有标点符号的词
  number_total[-punctuations_index]<-substr(words,1,-2)
  
  
  
  
  # # 在匹配的单词中用空格替换标点符号，以实现分割
  # split_sentences <- gsub(pattern, " ", sentences)
  # 
  # # 使用分割后的句子替换原始向量中的匹配单词
  # result <- sub(pattern, split_sentences, words)

  return(result)
}
