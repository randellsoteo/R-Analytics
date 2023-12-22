
install.packages("rvest")
library(rvest)
install.packages("polite")
library(polite)
install.packages("dplyr")
library(dplyr)
install.packages("httr")
library(httr)
install.packages("kableExtra")
library(kableExtra)
polite::use_manners(save_as = 'polite_scrape.R')




urlLinks <-'https://www.imdb.com/chart/top/?ref_=nv_mv_250&sort=rank%2Casc'
session <- bow(urlLinks,
               user_agent = "Educational")
session

rank_title <- character(0)
links <- character(0)

title_list <- scrape(session) %>%
  html_nodes('h3.ipc-title__text') %>% 
  html_text

class(title_list)
title_list_sub <- as.data.frame(title_list[2:251])
head(title_list_sub)
tail(title_list_sub)
colnames(title_list_sub) <- "ranks"
colnames(title_list_sub) <- "ranks"
split_df <- strsplit(as.character(title_list_sub$ranks),".",fixed = TRUE)
split_df <- data.frame(do.call(rbind,split_df))
split_df <- split_df[-c(3:4)]
colnames(split_df) <- c("ranks","title") 
str(split_df) 
class(split_df)
head(split_df)

rank_title <- data.frame(
  rank_title = split_df)

write.csv(rank_title,file = "title.csv")




link_list <- scrape(session) %>%
  html_nodes('a.ipc-title-link-wrapper') %>% 
  html_attr('href') 

head(link_list)

link_list[245:257]

link <- as.vector(link_list[1:250])
names(link) <- "links"

head(link)
tail(link)

for (i in 1:250) {
  link[i] <- paste0("https://imdb.com", link[i], sep = "")
}

links <- as.data.frame(link)

rank_title <- data.frame(
  rank_title = split_df, link)
scrape_df <- data.frame(rank_title,links)
names(scrape_df) <- c("Rank","Title","Link")

head(scrape_df)




imdb_top_50 <- data.frame()

current_row <- 1

for (row in 1:2) {
  url <- links$link[current_row]
  
  session2 <- bow(url,
                  user_agent = "Educational")
  
  webpage <- scrape(session2)
  
  rating <- html_text(html_nodes(webpage, ".sc-bde20123-1.cMEQkK"))
  rating <- rating[-2]
  
  votecount <- html_text(html_nodes(webpage,
                                    'div.sc-bde20123-3.gPVQxL'))
  votecount <- votecount[-2]
  
  movie_desc <- html_text(html_nodes(webpage, 
                                     '.sc-466bb6c-1.dWufeH'))
  movie_desc <- movie_desc[-2]
  
  meta_score <- html_text(html_nodes(
    webpage,
    '.sc-b0901df4-0.bcQdDJ.metacritic-score-box'))
  meta_score <- meta_score[-2]
  
  cat("Rating for", url, "is:", rating, "vote count is", votecount, 'and metascore is',meta_score, "\n")
  
  imdb_top_50[current_row,1] <- rating
  imdb_top_50[current_row,2] <- votecount
  imdb_top_50[current_row,3] <- movie_desc
  imdb_top_50[current_row,4] <- meta_score
  
  current_row <- current_row + 1
  
  Sys.sleep(3)
}

names(imdb_top_50) <- c("Rating","VoteCount","Description","MetaScore")

write.csv(imdb_top_50,file = "title.csv")
imdb_top_250 <- data.frame(
  scrape_df,imdb_top_50)

write.csv(imdb_top_250,file = "title.csv")

library(kableExtra)

df_d <- imdb_top_250[c(1:2),]

knitr::kable(df_d, caption = "IMDB Top 250 Movies") %>%
  kable_classic(full_width = T, html_font = "Arial Narrow") %>%
  kable_styling(font_size = 9)

