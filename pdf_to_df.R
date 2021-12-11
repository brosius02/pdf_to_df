 
#Author John Brosius
#10/07/2020
#DF to DF presentation
#https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf
library(tidyverse)
library(pdftools)
library(dplyr)
library(plyr)


rm(list = ls())

setwd("D:/GitHub/QC-Coders Presentation")
# setwd("P:/r applications/training")
citylist <- read.csv('citylist.csv', stringsAsFactors = FALSE)
get_pdf <- pdf_text("DataTablesExample.pdf")%>%
  readr::read_lines()


vector_pdf <- get_pdf[-c(1, 2)]
vector_pdf[1]



df_pdf <- plyr::ldply(vector_pdf)

df_pdf$Age <- str_extract(df_pdf$V1, '[0-9]+\\s')
df_pdf$StartDate <- str_extract(df_pdf$V1, '[0-9][0-9][0-9][0-9]\\/[0-9][0-9]\\/[0-9][0-9]')


df_pdf$V1 <- str_squish(df_pdf$V1)

df_pdf$FirstName <- str_extract(df_pdf$V1, '[[:upper:]][[:lower:]]+\\s')
df_pdf$LastName <- str_extract(df_pdf$V1, '\\s[[:upper:]][[:lower:]]+\\s')



df_pdf <-  separate(df_pdf, V1, 
                    c("col1", 'Salary'),
                    sep = "\\s[0-9][0-9][0-9][0-9]\\/[0-9][0-9]\\/[0-9][0-9]\\s")

df_pdf <-  separate(df_pdf, col1, 
                    c("col1"),
                    sep = "\\s[0-9][0-9]")

df_pdf$city8 <- str_extract(df_pdf$col1, '\\sNew\\sYork')
df_pdf$city9 <- str_extract(df_pdf$col1, '\\sSan\\sFrancisco')
df_pdf <-  separate(df_pdf, col1, 
                       c("crap", 
                         "crap1", "crap2", "crap3", "crap4", "crap5", "crap6", "crap7"),
                       sep = "\\s|New York|San Francisco")


df_pdf <- data.frame(unite(df_pdf, Salary_date, c(crap4, crap5, crap6, crap7), sep= " ", na.rm = TRUE))

df_pdf <-  separate(df_pdf, Salary_date, 
                    c("job", "job1", "city", "city1"),
                    sep = "\\s")

df_pdf <-  separate(df_pdf, job1, 
                    c("job1"),
                    sep = "\\(")
df_pdf <- data.frame(unite(df_pdf, job_city, c(crap3, job, job1, city, city1), sep= " ", na.rm = TRUE))

df_pdf <-  separate(df_pdf, job_city, 
                    c("job", "job1", "job2", "city", "city1"),
                    sep = "\\s|New York|San Francisco")

df_pdf <- data.frame(unite(df_pdf, city, c(job2, city, city1), sep= " ", na.rm = TRUE))


for (i in 1: nrow(df_pdf)) {
  if(is.na(df_pdf$job1[i]) | df_pdf$job1[i] == 'Developer' | df_pdf$job1[i] == 'Designer' |
     df_pdf$job1[i] == 'Officer' | df_pdf$job1[i] == 'Author' ){
    df_pdf$crap2[i] = paste(df_pdf$crap2[i], df_pdf$job[i], df_pdf$job1[i])
    df_pdf$job1[i] = ""
    df_pdf$crap2[i] = ""
  }
}

df_pdf <- df_pdf[-c(1,2)]
df_pdf <- data.frame(unite(df_pdf, city, c(job1, city, city8, city9), sep= " ", na.rm = TRUE))
df_pdf <- data.frame(unite(df_pdf, job, c(crap2, job), sep= " ", na.rm = TRUE))
#remove information

df_pdf <-  separate(df_pdf, job, 
                    c("job","job1", "job2"),
                    sep = "\\s")
for (i in 1: nrow(df_pdf)) {
  if(is.na(df_pdf$job1[i]) | df_pdf$job1[i] == 'Tokyo' |df_pdf$job1[i] == 'London'| df_pdf$job1[i] == 'Edinburgh'){
    df_pdf$city[i] = paste(df_pdf$job1[i])
    df_pdf$job1[i] = ""
  }
}

df_pdf <- data.frame(unite(df_pdf, job, c(job, job1, job2), sep= " ", na.rm = TRUE))
