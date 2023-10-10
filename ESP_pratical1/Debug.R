3 3 16 
4 2 2 
5 3 101 
6 3 47 
7 3 55 
8 2 10 
9 3 59 
10 2 3 
11 3 2 
12 2 795 
13 3 101 
14 3 290 
15 3 12 
16 3 863 
17 1 769 
18 2 27 
19 3 31 
20 3 418 
21 3 235 
22 3 53 
23 3 8 
24 3 341 
25 3 1 
26 1 356 
27 2 35 
28 3 3 
29 1 670 
30 2 6 
31 3 19 
32 3 7 
33 2 2 
34 3 478 
35 3 3 
36 1 955 
37 2 722 
38 3 12 
39 1 278 
40 2 12 
41 3 788 
42 1 626 
43 2 1 
44 3 4 
45 2 554 
46 2 1 
47 3 729 
48 2 8 
49 3 92 
50 3 71 


[1] 570 230  16   2 101  47  55  10  59   3   2 795 101 290  12 863 769  27  31 418 235  53   8 341   1 356
[27]  35   3 670   6  19   7   2 478   3 955 722  12 278  12 788 626   1   4 554   1 729   8  92  71

Tri_final[(Tri_final[, 1] == ) & (Tri_final[, 2] == 1),]
P_final[(P_final[, 1] == 4) & (P_final[, 2] == 21),]


matrix(Tri_final[(Tri_final[, 1] == sample_50_words[i - 2]) &
                   (Tri_final[, 2] == sample_50_words[i - 1]),], ncol = 3)


all_third_words <- matrix(Tri_final[(Tri_final[, 1] == 2) &
                                      (Tri_final[, 2] == 101),], ncol = 3)

c <- as.data.frame(table(all_third_words[,3]))
c$word_freq <- c[, 2] / sum(c[, 2])
c

all_second_words <- matrix(P_final[P_final[, 1] == 16,], ncol = 2)
c <- as.data.frame(table(all_second_words[,2]))
c$word_freq <- c[, 2] / sum(c[, 2])
c

sample(c[, 1], size = 1, prob = c$word_freq)
