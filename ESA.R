# This is a script written by Matthew Morriss on September 1, 2019, to 
# Plot the listing and delisting of different species from the ESA
#################################### SECTION TITLE ####################################
library(tidyverse)
library(stringr)
library(textclean)
library(lubridate)
library(rvest)
library(ggalt)
setwd('/Users/matthew/Documents/GitHub/endangered_species')

#################################### SECTION TITLE ####################################
url <- "https://ballotpedia.org/Delisted_species#cite_note-delisted-2"
website <- read_html(url)
tbls_ls <- website %>%
  html_nodes("table") %>%
  .[1:3] %>%
  html_table(fill = TRUE)

esaData <- tbls_ls[[3]]
rm(url,tbls_ls,website)

colnames(esaData) <- as.character(unlist(esaData[1,]))
esaData <- esaData[-1,]
esaData <- esaData[-nrow(esaData),]

esaData$`Date species first listed` <- mdy(esaData$`Date species first listed`)
esaData$`Date delisted` <-  mdy(esaData$`Date delisted`)

esaData %>%
  mutate(name =  gsub("\\(.*","",`Species name (scientific name)`)) -> esaData

a <- data.frame(do.call('rbind',(strsplit(esaData$name,','))))
esaData$name <- paste(a$X2,a$X1,sep = ' ')

esaData %>%
  filter(.,`Reason for delisting` == 'Extinct' | 
           `Reason for delisting` == 'Recovered') -> trueESA
rm(a)

trueESA %>%
  mutate(dura = trueESA$`Date delisted`-trueESA$`Date species first listed`) ->trueESA

#################################### Plot a dumbell plot ####################################
trueESA <- arrange(trueESA,desc(`Date species first listed`))
# trueESA$`Date species first listed` <- factor(trueESA$`Date species first listed`)
trueESA %>%
  ggplot(group = `Reason for delisting`)+
  geom_dumbbell(aes(y = reorder(name, `Date species first listed`),
                    x = trueESA$`Date species first listed`,
                    xend = trueESA$`Date delisted`
                    ))+
  
  geom_point(aes(y = reorder(name, `Date species first listed`),
               x = trueESA$`Date delisted`,
               color = `Reason for delisting`))+
  theme_light()+
  labs(x = 'Date',y = 'Species',color = 'Reason for delisting',
       title = 'Endangered Species that have been delisted',
       caption = 'by Matthew Morriss, 2019')+
  theme(axis.text.y = element_text(angle = 25, hjust = 1))


#################################### Order by duration ####################################

trueESA <- arrange(trueESA,desc(`Date species first listed`))
# trueESA$`Date species first listed` <- factor(trueESA$`Date species first listed`)
trueESA %>%
  ggplot(group = `Reason for delisting`)+
  geom_dumbbell(aes(y = reorder(name, dura),
                    x = trueESA$`Date species first listed`,
                    xend = trueESA$`Date delisted`
  ))+
  
  geom_point(aes(y = reorder(name, dura),
                 x = trueESA$`Date delisted`,
                 color = `Reason for delisting`))+
  theme_light()+
  labs(x = 'Date',y = 'Species',color = 'Reason for delisting',
       title = 'Endangered Species that have been delisted',
       caption = 'by Matthew Morriss, 2019')

#################################### SECTION TITLE ####################################

trueESA %>%
  ggplot(group = `Reason for delisting`)+
  geom_density(aes(x = dura, fill = `Reason for delisting`))

#################################### DB plot order by delist ####################################
trueESA <- arrange(trueESA,desc(`Date species first listed`))
# trueESA$`Date species first listed` <- factor(trueESA$`Date species first listed`)
trueESA %>%
  ggplot(group = `Reason for delisting`)+
  geom_dumbbell(aes(y = reorder(name, `Date delisted`),
                    x = trueESA$`Date species first listed`,
                    xend = trueESA$`Date delisted`
  ))+
  
  geom_point(aes(y = reorder(name, `Date species first listed`),
                 x = trueESA$`Date delisted`,
                 color = `Reason for delisting`))+
  theme_light()+
  labs(x = 'Date',y = 'Species',color = 'Reason for delisting',
       title = 'Endangered Species that have been delisted',
       caption = 'by Matthew Morriss, 2019')+
  theme(axis.text.y = element_text(angle = 25, hjust = 1))

#################################### SECTION TITLE ####################################

trueESA %>%
  ggplot(aes(x = `Date species first listed`,
             y = dura,
             color = `Reason for delisting`))+
  geom_smooth()+
  geom_point()