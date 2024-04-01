#setwd("/Users/liuchenli/Desktop/NCCU/1112_Statistical_Computing_and_Simulation/ChatGPT_variation")
library(tidyverse)
library(reshape2)

df = read.csv("response_df.csv")
colnames(df)

# boxplot & t-test
par(mfrow=c(1,3), family = "BiauKaiTC")
boxplot(df$length, main = "字數")
boxplot(df$num_word, main = "詞數")
boxplot(df$num_sentence, main = "句數")

t.test(df$length, mu=269, alternative="greater")
t.test(df$num_word, mu=107, alternative="greater")
t.test(df$num_sentence, mu=4, alternative="greater")

# boxplot & ANOVA
df %>%
  mutate(type_ch = ifelse(type=="point","無序清單",ifelse(type=="num","有序清單","文字描述")))%>%
  ggplot(aes(y=length))+
  geom_boxplot()+
  facet_wrap(~type_ch, ncol=3)+
  theme(text = element_text(family = "BiauKaiTC"))+
  labs(y="字數")

fit = aov(df$length~df$type)
summary(fit)

df %>%
  mutate(type_ch = ifelse(type=="point","無序清單",ifelse(type=="num","有序清單","文字描述")))%>%
  ggplot(aes(y=num_word))+
  geom_boxplot()+
  facet_wrap(~type_ch, ncol=3)+
  theme(text = element_text(family = "BiauKaiTC"))+
  labs(y="詞數")

fit = aov(df$num_word~df$type)
summary(fit)

df %>%
  mutate(type_ch = ifelse(type=="point","無序清單",ifelse(type=="num","有序清單","文字描述")))%>%
  ggplot(aes(y=num_sentence))+
  geom_boxplot()+
  facet_wrap(~type_ch, ncol=3)+
  theme(text = element_text(family = "BiauKaiTC"))+
  labs(y="句數")

fit = aov(df$num_sentence~df$type)
summary(fit)

# tf
tf = read.csv("response_df_tf_key.csv")
colnames(tf)

summary(tf$contain)
table(tf$contain)
prop.table(table(tf$contain))

rbind(table(tf$contain),
      prop.table(table(tf$contain)),
      cumsum(prop.table(table(tf$contain)))) %>%
  as.data.frame() %>%
  write.csv("tf_contain.csv",row.names = F)

par(mfrow=c(1,1))
temp = tf %>%
  select(9:18) %>%
  apply(2,sum) %>%
  as.data.frame()
colnames(temp) = "total"
temp$word = row.names(temp)
temp$prob = round(temp$total/200,2)

temp %>%
  ggplot(aes(x=reorder(word,-total), y=total))+
  geom_col()+
  geom_text(aes(y=total+7, label=paste(total)))+
  geom_text(aes(y=total-7, label=paste(prob)), colour="white")+
  theme(text = element_text(family = "BiauKaiTC"))+
  labs(x="")


# tfidf
tfidf = read.csv("response_df_tfidf_key.csv")
colnames(tfidf)

summary(tfidf$contain)
table(tfidf$contain)
prop.table(table(tfidf$contain))

rbind(table(tfidf$contain),
      prop.table(table(tfidf$contain)),
      cumsum(prop.table(table(tfidf$contain)))) %>%
  as.data.frame() %>%
  write.csv("tfidf_contain.csv",row.names = F)

par(mfrow=c(1,1))
temp = tfidf %>%
  select(9:18) %>%
  apply(2,sum) %>%
  as.data.frame()
colnames(temp) = "total"
temp$word = row.names(temp)
temp$prob = round(temp$total/200,2)

temp %>%
  ggplot(aes(x=reorder(word,-total), y=total))+
  geom_col()+
  geom_text(aes(y=total+7, label=paste(total)))+
  geom_text(aes(y=total-7, label=paste(prob)), colour="white")+
  theme(text = element_text(family = "BiauKaiTC"))+
  labs(x="")

# cosine similarity
mat = read.csv("cosine_similarity.csv")[,-1] %>% as.matrix()
colnames(mat) = as.character(1:200)
rownames(mat) = as.character(1:200)
melt_mat = melt(mat) # library(reshape2)

## heat map
melt_mat %>%
  mutate(similarity = value) %>%
  ggplot(aes(x = Var1, y = Var2, fill = similarity))+
  geom_tile()+
  scale_fill_gradient(high = "black", low = "white")+
  theme(text = element_text(family = "BiauKaiTC"))+
  labs(x = "", y = "", title = "original")

## histogram
diag_mat = mat[upper.tri(mat)]
diag_mat_df = as.data.frame(diag_mat)
colnames(diag_mat_df) = "similarity"

diag_mat_df %>%
  ggplot(aes(x=similarity))+
  geom_histogram(breaks = seq(0, 1, 0.1), fill = "gray")+
  stat_bin(breaks = seq(0, 1, 0.1), geom='text', color='black',
           aes(label=..count..), position=position_stack(vjust = 1.1))+
  theme(text = element_text(family = "BiauKaiTC"))+
  scale_x_continuous(breaks = seq(0, 1, 0.1))

rbind(round(hist(diag_mat, breaks = seq(0,1,0.1),plot = F)$counts, 0),
      round(hist(diag_mat, breaks = seq(0,1,0.1),plot = F)$counts/19900, 4),
      round(cumsum(hist(diag_mat, breaks = seq(0,1,0.1),plot = F)$counts/19900), 4))

## sample
set.seed(615)
check = which(melt_mat$value<0.6)# %>% sample(1)
melt_mat %>% 
  group_by(Var1) %>%
  summarise(ave = mean(value)) %>%
  arrange(ave)
melt_mat %>% 
  arrange(value)

## svd
mat = read.csv("cosine_similarity_svd_10.csv")[,-1] %>% as.matrix()
colnames(mat) = as.character(1:200)
rownames(mat) = as.character(1:200)
melt_mat = melt(mat) # library(reshape2)

melt_mat %>%
  mutate(similarity = value) %>%
  ggplot(aes(x = Var1, y = Var2, fill = similarity))+
  geom_tile()+
  scale_fill_gradient(high = "black", low = "white")+
  theme(text = element_text(family = "BiauKaiTC"))+
  labs(x = "", y = "", title = "10 SVD")



mat = read.csv("cosine_similarity_svd_50.csv")[,-1] %>% as.matrix()
colnames(mat) = as.character(1:200)
rownames(mat) = as.character(1:200)
melt_mat = melt(mat) # library(reshape2)

melt_mat %>%
  mutate(similarity = value) %>%
  ggplot(aes(x = Var1, y = Var2, fill = similarity))+
  geom_tile()+
  scale_fill_gradient(high = "black", low = "white")+
  theme(text = element_text(family = "BiauKaiTC"))+
  labs(x = "", y = "", title = "50 SVD")

mat = read.csv("cosine_similarity_svd_100.csv")[,-1] %>% as.matrix()
colnames(mat) = as.character(1:200)
rownames(mat) = as.character(1:200)
melt_mat = melt(mat) # library(reshape2)

melt_mat %>%
  mutate(similarity = value) %>%
  ggplot(aes(x = Var1, y = Var2, fill = similarity))+
  geom_tile()+
  scale_fill_gradient(high = "black", low = "white")+
  theme(text = element_text(family = "BiauKaiTC"))+
  labs(x = "", y = "", title = "100 SVD")

mat = read.csv("cosine_similarity_svd_200.csv")[,-1] %>% as.matrix()
colnames(mat) = as.character(1:200)
rownames(mat) = as.character(1:200)
melt_mat = melt(mat) # library(reshape2)

melt_mat %>%
  mutate(similarity = value) %>%
  ggplot(aes(x = Var1, y = Var2, fill = similarity))+
  geom_tile()+
  scale_fill_gradient(high = "black", low = "white")+
  theme(text = element_text(family = "BiauKaiTC"))+
  labs(x = "", y = "", title = "200 SVD")




