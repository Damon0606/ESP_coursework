# 4.1
a <- 2
a
b <- 1 / a
b


foo <- function(x, y, k = 0.5) {
  x * cos(y - k)
}

foo(3, 2)
foo(3, 2, 0)

a <- 1 + 2 + 3
+4 + 5
a

a <- 1 + 2 + 3 + 4 + 5
a

a <- 2
b <- 1
d <- log(b)

q("no")


# 4.2
x <- c(10, 2, 7, 89, 43, 1)
ii <- which(x %/% 10 > 0)
# /: standard division; %/%: whole division (rounded); %%: takes the remainder.
xs <- rep(0, length(ii) + length(x))
iis <- ii + 1:length(ii) - 1
xs[iis] <- x[ii] %/% 10
xs[-iis] <- x %% 10

# 4.3
x <- c("10", "2", "7", "89", "43", "1")
ii <- which(nchar(x) > 1) # nchar: count the characters in each element of x.
xs <- rep("", length(ii) + length(x))
iis <- ii + 1:length(ii)
xs <- substr(x[ii], 2, 2)
# substr: extracts the characters between 2 and 2 for every element in vector x[ii] (so just one digit)
xs[-iis] <- substr(x, 1, 1)
# substr: extracts the characters between 1 and 1 for every element in vector x (so just one digit)

# 4.3.1
poem <- paste(
  "Inside me is a skeleton, of this I have no doubt,",
  "now it's got my flesh on, but it's waiting to get out."
)
# paste: join the two given strings into one string.
pow <- strsplit(poem, " ")[[1]]
# strsplit: splits a sentence into a vector of its individual words at the breaks given by spaces, " ".
# return a list
n.words <- length(pow)
freq <- tabulate(nchar(pow))
# nchar: counts the letters in each word;
# tabulate: tallies them up.
ie <- grep("e", pow, fixed = TRUE)
n.e <- length(ie)
ia <- grep("a", pow, fixed = TRUE)
iea <- ia[ia %in% ie]
# %in%: to determine if a element is in a vector
# TRUE if each element of ia that occurs in ie
# So the result of ia %in% shows that the element '9' in ia is also in ie
pow[iea] <- paste(pow[iea], "*", sep = "")
paste(pow, collapse = " ")
# collapse: choose a character/string to separate each two words, here we choose space.

# Exercises
# 1.
? gsub
# sub and gsub perform replacement of the first and all matches respectively.
pow_gsub <- gsub("[,.]", "", pow)

# 2.
poem_new <- paste(
  "Inside me is a skeleton,", "\n", "of this I have no doubt,", "\n",
  "now it's got my flesh on,", "\n", "but it's waiting to get out."
)
cat(poem_new)

# 3.
set.seed(0)
y <- rt(100, df = 4)
hist(y)
mean_y <- mean(y)
sd_y <- sd(y)
outliers <- y < (mean_y - 2 * sd_y) | y > (mean_y + 2 * sd_y)
y_filtered <- y[!outliers]
mean_filtered <- mean(y_filtered)
mean_y
sd_y
mean_filtered

# 4.
flitered_y <- function(y, k = 2) {
  mean_y <- mean(y)
  sd_y <- sd(y)
  outliers <- y < (mean_y - k * sd_y) | y > (mean_y + k * sd_y)
  y_filtered <- y[!outliers]
  mean_filtered <- mean(y_filtered)
}

flitered_y(y,k=3)
