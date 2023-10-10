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
Tri_final <- Tri[which(!is.na(Tri_rowsum)), ] ## drop triplets containing NA
## 7(d)same for Pairs
P <- cbind(first_col, second_col)
P_rowsum <- rowSums(P, na.rm = FALSE)
P_final <- P[which(!is.na(P_rowsum)), ]

## remove specific illogical situations in Pairs & Triplets
the_position <- which(b[] == "the") ## index of 'the'
position <- which(P_final[, 1] == the_position & P_final[, 2] == the_position)
P_final <- P_final[-position, ] ## remove 'the the'


# ———————————————————————————————————————————————————————————————————————————————
# Step 8
# ———————————————————————————————————————————————————————————————————————————————
# sample_words <- function(all_words) {
#   Frenquency_table <- as.data.frame(table(all_words))
#   Frenquency_table$word_freq <- Frenquency_table[, 2] / sum(Frenquency_table[, 2]) ## probability of each word
#   word_index <- sample(Frenquency_table[, 1], ## select index with a given probability
#     size = 1, replace = TRUE, prob = Frenquency_table$word_freq
#   )
#   return(word_index)
# }
# 
# sample_50_words <- c() ## vector for 50-word sections
# sample_50_words[1] <- sample(unique(P_final[, 1]), size = 1) ## 8(a)randomly select from P_final
# all_second_words_1 <- matrix(P_final[P_final[, 1] == sample_50_words[1], ], ncol = 2) ## generate second word from Pairs
# sec_freq <- as.data.frame(table(all_second_words_1))
# sec_freq$word_freq <- sec_freq[, 2] / sum(sec_freq[, 2])
# sample_50_words[2] <- sample(sec_freq[, 1], size = 1, prob = sec_freq$word_freq)
# 
# 
# # Simulate the rest of the 48 words
# for (i in 3:50) {
#   ## 8(b) extract sub-matrix[2] from Triplets
#   all_third_words <- matrix(Tri_final[(Tri_final[, 1] == sample_50_words[i - 2]) &
#     (Tri_final[, 2] == sample_50_words[i - 1]), ], ncol = 3)
#   all_second_words <- matrix(P_final[P_final[, 1] == sample_50_words[i - 1], ], ncol = 2)
#   ## 8(c)
#   if (length(all_third_words) != 0) { ## if sub-matrix has rows
#     third_freq <- as.data.frame(table(all_third_words[, 3]))
#     third_freq$word_freq <- third_freq[, 2] / sum(third_freq[, 2])
#     index_TEMP3 <- as.character(third_freq[, 1])
#     index_TEMP3 <- as.integer(index_TEMP3)
#     if (length(index_TEMP3) == 1) {
#       index_temp3 <- index_TEMP3
#     } else {
#       index_temp3 <- sample(index_TEMP3, size = 1, prob = third_freq[, 3])
#     }
#     sample_50_words <- append(sample_50_words, index_temp3) ## simulate from Triplets
#     cat(i, "3", index_temp3, "\n")
#   } else if (length(all_second_words) != 0) { ## if sub-matrix has no rows
#     second_freq <- as.data.frame(table(all_second_words[, 2]))
#     second_freq$word_freq <- second_freq[, 2] / sum(second_freq[, 2])
#     index_TEMP2 <- as.character(second_freq[, 1])
#     index_TEMP2 <- as.integer(index_TEMP2)
#     if (length(index_TEMP2) == 1) {
#       index_temp2 <- index_TEMP2
#     } else {
#       index_temp2 <- sample(index_TEMP2, size = 1, prob = second_freq[, 3])
#     }
#     index_temp2 <- sample(index_TEMP2, size = 1, prob = second_freq[, 3])
#     sample_50_words <- append(sample_50_words, index_temp2) ## simulate from Pairs
#     cat(i, "2", index_temp2, "\n")
#   } else { ## if neither Triplets nor Pairs have rows
#     index_temp1 <- sample(unique(P_final[, 1]), 1)
#     sample_50_words <- append(sample_50_words, index_temp1) ## simulate based on probability
#     cat(i, "1", index_temp1, "\n")
#   }
# }
# 
# section8 <- paste(b[sample_50_words], collapse = " ") ## combined into a sentence
# section8 <- gsub("\\s+(?=[[:punct:]])", "", section8, perl = TRUE)
# cat(section8)



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













# DRAFT
# ———————————————————————————————————————————————————————————————————————————————
# ———————————————————————————————————————————————————————————————————————————————
# ———————————————————————————————————————————————————————————————————————————————
# punctuation <- c(",", ".", ";", "!", ":", "?")
# punctuation <- grep("\\w+|[[:punct:]]")


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


text <- "—There was a fellow sailed with me in the _Rover_, the old seadog,
himself a rover, proceeded, went ashore and took up a soft job as
gentleman’s valet at six quid a month. Them are his trousers I’ve on me
and he gave me an oilskin and that jackknife. I’m game for that job,
shaving and brushup. I hate roaming about. There’s my son now, Danny,
run off to sea and his mother got him took in a draper’s in Cork where
he could be drawing easy money.

—What age is he? queried one hearer who, by the way, seen from the
side, bore a distant resemblance to Henry Campbell, the townclerk, away
from the carking cares of office, unwashed of course and in a seedy
getup and a strong suspicion of nosepaint about the nasal appendage.

—Why, the sailor answered with a slow puzzled utterance, my son, Danny?
He’d be about eighteen now, way I figure it.

The Skibbereen father hereupon tore open his grey or unclean anyhow
shirt with his two hands and scratched away at his chest on which was
to be seen an image tattooed in blue Chinese ink intended to represent
an anchor.

—There was lice in that bunk in Bridgwater, he remarked, sure as nuts.
I must get a wash tomorrow or next day. It’s them black lads I objects
to. I hate those buggers. Suck your blood dry, they does.

Seeing they were all looking at his chest he accommodatingly dragged
his shirt more open so that on top of the timehonoured symbol of the
mariner’s hope and rest they had a full view of the figure 16 and a
young man’s sideface looking frowningly rather.

—Tattoo, the exhibitor explained. That was done when we were lying
becalmed off Odessa in the Black Sea under Captain Dalton. Fellow, the
name of Antonio, done that. There he is himself, a Greek.

—Did it hurt much doing it? one asked the sailor.

That worthy, however, was busily engaged in collecting round the.
Someway in his. Squeezing or.

—See here, he said, showing Antonio. There he is cursing the mate. And
there he is now, he added, the same fellow, pulling the skin with his
fingers, some special knack evidently, and he laughing at a yarn.

And in point of fact the young man named Antonio’s livid face did
actually look like forced smiling and the curious effect excited the
unreserved admiration of everybody including Skin-the-Goat, who this
time stretched over.

—Ay, ay, sighed the sailor, looking down on his manly chest. He’s gone
too. Ate by sharks after. Ay, ay.

He let go of the skin so that the profile resumed the normal expression
of before.

—Neat bit of work, one longshoreman said.

—And what’s the number for? loafer number two queried.

—Eaten alive? a third asked the sailor."

words <- scan(text = text, what = "character", quiet = TRUE)
text_clean <- remove_punctuation(words)
text_clean <- split_punct(text_clean, punctuation)
text_clean




text <- c("An", "omnishambles,", "in)", "a.", "_headless", "chicken's", "-factory", "of", "of")
text <- split_punct(text, punctuation)
text <- remove_punctuation(text)
text


pattern <- ("of\\sof")
text_paste <- paste(text, collapse = " ")
matches <- grep(pattern, text_paste, value = FALSE)
matches


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
commonly_words_index <- Frequency[target_positions] # 提取常见词对应的index

# (e)
b <- Unique[Index[Frequency[target_positions]]] # 提取常见词index对应的单词

# Check example: was
# was_index <- which(Unique == "was")
# was_frequency <- Frequency[24]
# was_rank <- which(SortedFreq == was_frequency)





# Test_list <- c(2, 3, 2, 6, 8, 4, 4, 4, 5)
# order <- sort(Test_list, decreasing = TRUE)
# threshold <- order[5]
# index <- which(Test_list == threshold)
# boundary <- max(index)
# boundary


# 假设data是一个数据框，包含多个列，每一行都是数字型数据
data <- data.frame(
  Col1 = c(1, 2, 3, 1, 2),
  Col2 = c(4, 5, 6, 5, 4),
  Col3 = c(7, 8, 7, 8, 8)
)

# 将每一行的数字按指定顺序组合成字符串
row_combinations <- apply(data, 1, function(row) paste(sort(row), collapse = ""))

# 使用table()函数统计频次
frequency_table <- as.data.frame(table(row_combinations))

# 输出频次表
View(frequency_table)














# ———————————————————————————————————————————————————————————————————————————————
# Step 8
# ———————————————————————————————————————————————————————————————————————————————
# sample_words <- function(all_words) {
#   Frenquency_table <- as.data.frame(table(all_words))
#   Frenquency_table$word_freq <- Frenquency_table[, 2] / sum(Frenquency_table[, 2]) ## probability of each word
#   word_index <- sample(Frenquency_table[, 1], ## select index with a given probability
#                        size = 1, replace = TRUE, prob = Frenquency_table$word_freq
#   )
#   return(word_index)
# }

# Modified Version 1
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

# # Modified Version 2
# sample_words <- function(all_words, n, type) {
#   Frenquency_table <- as.data.frame(table(all_words[, type]))
#   Frenquency_table$word_freq <- Frenquency_table[, 2] / sum(Frenquency_table[, 2])
#   index_temp <- sample(as.integer(as.character(Frenquency_table[, 1])), size = n, prob = Frenquency_table$word_freq)
#   return(index_temp)
# }



sample_50_words <- c() ## vector for 50-word sections
sample_50_words[1] <- sample(unique(P_final[, 1]), size = 1) ## 8(a)randomly select from P_final

# all_second_words_1 <- matrix(P_final[P_final[, 1] == sample_50_words[1], ], ncol = 2) ## generate second word from Pairs
# sec_freq <- as.data.frame(table(all_second_words_1))
# sec_freq$word_freq <- sec_freq[, 2] / sum(sec_freq[, 2])
# sample_50_words[2] <- sample(sec_freq[, 1], size = 1, prob = sec_freq$word_freq)
sample_50_words[2] <- sample_words(matrix(P_final[P_final[, 1] == sample_50_words[1], ], ncol = 2), 1)




# Simulate the rest of the 48 words
for (i in 3:50) {
  ## 8(b) extract sub-matrix[2] from Triplets
  all_third_words <- matrix(Tri_final[(Tri_final[, 1] == sample_50_words[i - 2]) &
    (Tri_final[, 2] == sample_50_words[i - 1]), ], ncol = 3)
  all_second_words <- matrix(P_final[P_final[, 1] == sample_50_words[i - 1], ], ncol = 2)
  ## 8(c)
  if (length(all_third_words) != 0) { ## if sub-matrix has rows
    # third_freq <- as.data.frame(table(all_third_words[, 3]))
    # third_freq$word_freq <- third_freq[, 2] / sum(third_freq[, 2])
    # index_TEMP3 <- as.character(third_freq[, 1])
    # index_TEMP3 <- as.integer(index_TEMP3)
    # if (length(index_TEMP3) == 1) {
    #   index_temp3 <- index_TEMP3
    # } else {
    #   index_temp3 <- sample(index_TEMP3, size = 1, prob = third_freq[, 3])
    # }
    # index_temp3 <- sample_words(all_third_words, 1, 3)
    # sample_50_words <- append(sample_50_words, index_temp3) ## simulate from Triplets
    sample_50_words <- append(sample_50_words, sample_words(all_third_words, 1, 3))
    cat(i, "3", index_temp3, "\n")
  } else if (length(all_second_words) != 0) { ## if sub-matrix has no rows
    # second_freq <- as.data.frame(table(all_second_words[, 2]))
    # second_freq$word_freq <- second_freq[, 2] / sum(second_freq[, 2])
    # index_TEMP2 <- as.character(second_freq[, 1])
    # index_TEMP2 <- as.integer(index_TEMP2)
    # if (length(index_TEMP2) == 1) {
    #   index_temp2 <- index_TEMP2
    # } else {
    #   index_temp2 <- sample(index_TEMP2, size = 1, prob = second_freq[, 3])
    # }
    # index_temp2 <- sample(index_TEMP2, size = 1, prob = second_freq[, 3])
    # index_temp2 <- sample_words(all_second_words, 1, 2)
    # sample_50_words <- append(sample_50_words, index_temp2) ## simulate from Pairs
    sample_50_words <- append(sample_50_words, sample_words(all_second_words, 1, 2)) ## simulate from Pairs
    cat(i, "2", index_temp2, "\n")
  } else { ## if neither Triplets nor Pairs have rows
    # index_temp1 <- sample(unique(P_final[, 1]), 1)
    # sample_50_words <- append(sample_50_words, index_temp1) ## simulate based on probability
    sample_50_words <- append(sample_50_words, sample(unique(P_final[, 1]), 1)) ## simulate based on probability
    cat(i, "1", index_temp1, "\n")
  }
}

section8 <- paste(b[sample_50_words], collapse = " ") ## combined into a sentence
section8 <- gsub("\\s+(?=[[:punct:]])", "", section8, perl = TRUE)
cat(section8)

sample_50_words
