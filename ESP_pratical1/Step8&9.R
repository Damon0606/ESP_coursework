# Huantong Hou (s2481591), Yuqi Shi (s2508879), Zukai Li (s2505721)

# Contribution:
#### Huantong Hou (s2481591): 32%
###### (1) Ditch whole idea for total practical work;
###### (2) Finish codes for steps 4, 6 and 10, and participate in debugging total practical work;
###### (3) Participate in making comments on total practical work.

#### Yuqi Shi (s2508879): 33%
###### (1) Organize the logic for implementing the project；
###### (2) Mainly write the code for steps 6, 7, and 8，and participate in debugging work;
###### (3) Participate in making comments on total practical work.

#### Zukai Li (s2505721): 35%
###### (1) Organizing the overall idea of the project；
###### (2) Mainly write the code for steps 4, 8, and 9，and participate in debugging work;
###### (3) Participate in making comments on total practical work.


# ———————————————————————————————————————————————————————————————————————————————
# Step 3
# Read the file into R and remove "_()_"from file
# ———————————————————————————————————————————————————————————————————————————————
# setwd("/Users/shiyuqi/Downloads") ## comment out of submitted
W <- scan("4300-0.txt", what = "character", skip = 73, nlines = 32858 - 73)
W <- gsub("_(", "", W, fixed = TRUE) ## remove "_("
W <- gsub(")_", "", W, fixed = TRUE) ## remove ")_"


# ———————————————————————————————————————————————————————————————————————————————
# Step 4
# Write a function(split_punct) that can find punctuation marks, remove them from words,
# and place separated words and punctuation into a new vector
# ——————————————————————————————————————————————————————————————————————————————
## Remove useless punctuation
remove_punctuation <- function(words) {
  words <- gsub("[-—_*()\\.{3}]", "", words) ## remove "[()*-—_]"
  words <- words[words != ""] ## remove null
  return(words)
}
W <- remove_punctuation(W)
## Write a function(split_punct)
split_punct <- function(words, punctuation) {
  words_index <- grep("[,.;!:?]", words) ## indices of words containing "[,.;!:?]"
  punc_index <- words_index + 1:length(words_index) ## determine the length of the vector which words and punctuation will be placed
  words_modified <- rep("", length(words) + length(punc_index)) ## create a vector to store split words and punctuation
  words_modified[-punc_index] <- sapply(words, function(x) ifelse(x %in% words[words_index], substr(x, 1, nchar(x) - 1), x))
  ## insert the text portion of words
  words_modified[punc_index] <- sapply(words[words_index], function(x) substr(x, nchar(x), nchar(x))) ## insert words without punctuation
  return(words_modified)
}


# ———————————————————————————————————————————————————————————————————————————————
# Step 5
# Use the split_punct () to separate text and punctuation
# ———————————————————————————————————————————————————————————————————————————————
W_clean <- split_punct(W, punctuation)


# ———————————————————————————————————————————————————————————————————————————————
# Step 6
# Create a vector-b, of the almost 1000 commonly occurring words
# ———————————————————————————————————————————————————————————————————————————————
Unique <- unique(tolower(W_clean)) ## find the unique words of W_clean
Index <- match(tolower(W_clean), Unique) ## find the indicating positions of words in the text corresponding to unique vector
Frequency <- as.data.frame(table(Index)) ## frequency of unique words in the text
sorted_Frequency <- Frequency[order(-Frequency$Freq), ] ## sort frequency in descending order
threshold <- sorted_Frequency$Freq[1000] ## set the threshold number of occurrences
boundary <- max(which(sorted_Frequency$Freq == threshold))
Frequency_1000 <- sorted_Frequency[1:boundary, ]
b <- Unique[Frequency_1000$Index] ## most commonly occurring words


# ———————————————————————————————————————————————————————————————————————————————
# Step 7
# Create the matrices of common word triplets and pairs(Tri_final and P_final)
# ———————————————————————————————————————————————————————————————————————————————
first_col <- second_col <- third_col <- c() ## define empty vectors for three column matrix
Index_a_common <- match(tolower(W_clean), b) ## find the indicating positions of words in the text corresponds to b
for (i in 1:length(Frequency_1000$Index)) {
  a_b_position <- which(Index_a_common[] == Frequency_1000$Index[i]) ## find the positions of common words in W_clean
  first_col <- as.numeric(append(matrix(rep(Frequency_1000$Index[i], length(a_b_position)), ncol = 1), first_col)) ## index of the common words
  second_col <- as.numeric(append(matrix(Index_a_common[a_b_position + 1], ncol = 1), second_col)) ## index for the following word
  third_col <- as.numeric(append(matrix(Index_a_common[a_b_position + 2], ncol = 1), third_col)) ## index for the next following word
}
Tri <- cbind(first_col, second_col, third_col) ## define Triplets
Tri_rowsum <- rowSums(Tri, na.rm = FALSE) ## Use rowSum() to find rows that contain 'NA'
Tri_final <- Tri[which(!is.na(Tri_rowsum)), ] ## drop triplets containing NA
## 7(d)same for Pairs
P <- cbind(first_col, second_col)
P_rowsum <- rowSums(P, na.rm = FALSE)
P_final <- P[which(!is.na(P_rowsum)), ]

## remove specific illogical situations in Pairs & Triplets
the_position <- which(b[] == "the") ## find the index of 'the'
position <- which(P_final[, 1] == the_position & P_final[, 2] == the_position) ## find the positions of 'the''the' in pairs
P_final <- P_final[-position, ] ## remove 'the the' from pairs


# ———————————————————————————————————————————————————————————————————————————————
# Step 8
# Simulate 50-word sections using word occurrence probability models
# ———————————————————————————————————————————————————————————————————————————————
## A function randomly extracts word indices from the corresponding array according to different situations
sample_words <- function(all_words, n, type) {
  Frenquency_table <- as.data.frame(table(all_words[, type])) ## calculate frequency
  Frenquency_table$word_freq <- Frenquency_table[, 2] / sum(Frenquency_table[, 2]) ## calculate sample probability according to the frequency
  index_TEMP <- as.integer(as.character(Frenquency_table[, 1])) ## change the data type to make sure the sample function works
  if (length(index_TEMP) == 1) { ## when the word to be generated is unique
    index_temp <- index_TEMP ## extract directly
  } else {
    index_temp <- sample(index_TEMP, size = n, prob = as.double(Frenquency_table$word_freq)) ## extract the next generated word based on probability
  }
  return(index_temp)
}

sample_50_words <- c() ## generate an empty vector for 50-word sections
sample_50_words[1] <- sample(unique(P_final[, 1]), size = 1) ## simulate the first word randomly from P_final
sample_50_words[2] <- sample_words(matrix(P_final[P_final[, 1] == sample_50_words[1], ], ncol = 2), 1) ## simulate the second word randomly

# Simulate the rest of the 48 words
for (i in 3:50) {
  all_third_words <- matrix(Tri_final[(Tri_final[, 1] == sample_50_words[i - 2]) &
                                        (Tri_final[, 2] == sample_50_words[i - 1]), ], ncol = 3)
  all_second_words <- matrix(P_final[P_final[, 1] == sample_50_words[i - 1], ], ncol = 2)
  if (length(all_third_words) != 0) { ## if sub-matrix from Tri_final has rows that satisfy the condition
    sample_50_words <- append(sample_50_words, sample_words(all_third_words, 1, 3)) ## simulate from Tri_final
  } else if (length(all_second_words) != 0) { ## if sub-matrix from P_final has rows that satisfy the condition
    sample_50_words <- append(sample_50_words, sample_words(all_second_words, 1, 2)) ## simulate from Pairs
  } else { ## if neither Triplets nor Pairs have rows that satisfy the condition
    sample_50_words <- append(sample_50_words, sample(unique(P_final[, 1]), 1)) ## simulate according to the common word frequencies
  }
}

section8 <- paste(b[sample_50_words], collapse = " ") ## combined into a sentence
section8 <- gsub("\\s+(?=[[:punct:]])", "", section8, perl = TRUE) ## handling syntactic formatting
cat(section8)


# ———————————————————————————————————————————————————————————————————————————————
# Step 9
# Simulate 50-word sections of text only based on the common word frequencies
# ———————————————————————————————————————————————————————————————————————————————
words_sections9 <- sample(Frequency_1000[, 1], size = 50, prob = Frequency_1000[, 2]) ## simulate indices based on common frequencies
words_sections9 <- Unique[words_sections9] ## find the corresponding words based on simulation indices
section9 <- paste(words_sections9, collapse = " ") ## combined all simulated words into a sentence
section9 <- gsub("\\s+(?=[[:punct:]])", "", section9, perl = TRUE) ## handling syntactic formatting
cat(section9)


# ———————————————————————————————————————————————————————————————————————————————
# Step 10
# Find words that most often start with capital letter, also start with a
# capital letter in simulation (section in step 8)
# ———————————————————————————————————————————————————————————————————————————————
CapitalWords <- c() ## vector for words most often start with capital letter
Capital_Unique <- unique(W_clean) ## Find unique words in text
pattern <- "\\b^[A-Z]\\w*\\b" ## pattern to search words starting with capital letter
for (w in Capital_Unique) {
  matches <- str_extract_all(w, pattern)[[1]] ## Find all words starting with capital letter
  CapitalWords <- append(CapitalWords, matches)
}
## indices indicating positions of Capital in the text corresponding to unique vector
Capi_Index <- match(CapitalWords, W_clean)
## occurrence frequency of unique capital words in the text
Capi_Frequency <- as.data.frame(table(Capi_Index))
## sort frequency in descending order
Capi_sorted_Frequency <- Capi_Frequency[order(-Capi_Frequency$Freq), ]
Capi_threshold <- Capi_sorted_Frequency$Freq[10] ## threshold number of occurrences
## last index of commonly capital words
Capi_boundary <- max(which(Capi_sorted_Frequency$Freq == Capi_threshold))
Capi_Frequency_10 <- Capi_sorted_Frequency[1:Capi_boundary, ]
## most commonly occurring capital words
Capi_b <- W_clean[as.integer(as.character(Capi_Frequency_10[, 1]))]

LowerSection <- b[sample_50_words]
for (i in (1:length(LowerSection))) {
  wd <- LowerSection[i]
  if (wd %in% tolower(Capi_b)) { ## find words in section are commonly capital words
    LowerSection[i] <- str_to_title(wd) ## capitalize first word
  }
}
Lowersection8 <- paste(LowerSection, collapse = " ") ## combined into a sentence
Lowersection8 <- gsub("\\s+(?=[[:punct:]])", "", Lowersection8, perl = TRUE)
cat(Lowersection8)
