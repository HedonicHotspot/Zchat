#!/usr/bin/env Rscript --vanilla
library(readr)
library(dplyr)
library(tidyr)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(tm)
library(tidyverse)
library(dplyr)
library(corpus)
library(lubridate)

v <- list() # create list for variables

# When does class start/end
v$starts <- hms("14:00:00")
v$ends <- hms("14:50:00")


# v$chatFile <- list.files(pattern = "meeting_saved_chat") |> sort()
v$chatFile <- "meeting_saved_chat.txt"
v$chatDate <- file.info(v$chatFile )[1,"mtime"]
v$filedate <- substr(v$chatDate, 1,10)
# file.rename(v$chatFile, paste0("chat", v$filedate, ".txt"))
# v$chatFile <- paste0("chat", v$filedate, ".txt")

# filedate <- "12.08"  #change the date each day
# chatfile <- paste("chat", filedate, ".txt", sep = "")




#attendancefile <- paste("zoom_attendance", filedate, ".csv", sep = "")
v$wordCloudFile <- paste0("WordCloud_", v$filedate,".jpeg")

v$chat <- read_delim(v$chatFile, 
                     delim = "\t", escape_double = FALSE, 
                     col_names = F) |>
  as.data.frame()


# For total wordcloud #########
# chatfiles <- list.files(path =  "~/Box Sync/HSU/Fall 2022 Crit. Think/Critical Thinking/Participation", pattern = "chat", full.names = TRUE)
# for (file in chatfiles) {
#   chat <- rbind(chat, read_delim(file, 
#                   delim = "\t", escape_double = FALSE, 
#                    col_names = F))
# }

#####################

v$time_name <- v$chat[grep("^1", v$chat$X1),]
v$convo <- v$chat[grep("^1", v$chat$X1) + 1, ] |> as.data.frame()

# v$attendance <- read_csv(attendancefile)
# v$attendance <- separate(attendance, col = 3, sep = " ", into = c("join_date", "join_time"))

#time_name <- chat[ seq(from = 1,to = nrow(chat), by = 2) , ]
#convo <- chat[ seq(from = 2,to = nrow(chat), by = 2) , ]

v$chat <- v$time_name |> as.data.frame()

#chat <- as.data.frame(chat)
#convo[nrow(convo)+1,] <- "bye"


v$chat <- separate(v$chat, 1, c("time", "from", "first", "last", "to", "everyone"), sep = " ")
v$chat <- cbind(v$chat, v$convo)





# Format/Quantify Time Data

# attendance$join_time <- hms(attendance$join_time)
# attendance$join_time <- hour(attendance$join_time)*60 + minute(attendance$join_time)  - 17*60     # convert hours to 
v$participation <- v$chat
v$participation$name <- paste(v$participation$last, v$participation$first)
v$participation <- v$participation[, c(1,3,4,7,8)]
colnames(v$participation) <- c("time", "first", "last", "convo", "name")
v$participation$convo <- gsub("\t", "", v$participation$convo)

v$participation$time <- hms(v$participation$time)        # format to 'hours:minutes:seconds'
# v$participation$time <- hour(v$participation$time)*60 + minute(v$participation$time)    # convert hours to minutes, and add minutes
v$participation$third <- 1:nrow(v$participation)
v$participation$third[v$participation$time <= 
                        v$starts + minutes(2)] <- "Punctual"  #Initial attendance

v$participation$third[v$participation$time > v$starts + minutes(2) & 
                        v$participation$time < v$starts + minutes(25)] <- "Part_1" # Participated in first half
v$participation$third[v$participation$time > v$starts + minutes(25) & 
                        v$participation$time < v$starts + minutes(48)] <- "Part_2" # Participated in second half
v$participation$third[v$participation$time > v$starts + minutes(48)] <- "Farewell"


# Who participated?

# attendance <- as.data.frame(attendance)
# grade_attend <- attendance[order(attendance$`User Name`),c(1,4,5,6)] #For punctuality validation


#For grading
v$grade_part <- as.data.frame.matrix(table(v$participation[,c(5,6)]))
v$grade_part <- cbind(as.data.frame(rownames(v$grade_part)), 
                      v$grade_part[,c(4,2,3,1)])

v$participation <- v$participation[order(v$participation$last),]

write_csv2(v$grade_part,paste0("PartGrade_", v$filedate, ".csv"))
write_csv2(v$participation, paste0("Evidence_", v$filedate, ".csv"))



# Word Cloud

## Make Corpus Vector and Clean
v$words <- v$participation
v$remove_these <- c("yes", "no", "bye", "also", "like", "gets", "hey", "ive", "goes", "dont", "make", "just", "thats", "got", "can", "lot", "get", "yeah", "will")
v$docs <- v$participation[,c(1,4)]
colnames(v$docs) <- c('time', 'text')
v$docs <- VCorpus(VectorSource(v$docs$text)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(removeWords, v$remove_these) %>%
  tm_map(stripWhitespace)
v$docs <- tm_map(v$docs, content_transformer(tolower))
v$docs <- tm_map(v$docs, removeWords, stopwords("english"))

## Document Term Matrix
v$dtm <- TermDocumentMatrix(v$docs) 
v$matrix <- as.matrix(v$dtm) 
v$words <- sort(rowSums(v$matrix),decreasing=TRUE) 
v$df <- data.frame(word = names(v$words),freq=v$words)

# Make wordcloud
set.seed(1234) # for reproducibility 

jpeg(v$wordCloudFile, width = 1080, height = 1080)
wordcloud(words = v$df$word, freq = v$df$freq, min.freq = 1,           
          max.words=200, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))
dev.off()


