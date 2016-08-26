##ID##
1798712427007172
################

##Key##
b129a2fb761fa8d23566b16e907dbb50
################################

library(Rfacebook)
library(ggplot2)
library(dplyr)
library(reshape)
library(gridExtra)
require("RColorBrewer")
col1<-brewer.pal(9, "Oranges")

#Generation du token (pour 2 heures au maximum)
fb_oauth <- 'EAACEdEose0cBAJSVE2c7y78zp0SAa5VrjNJ4iIT0gAaFlbEYyYUc7i9NOpmmK2qN9NbobZCS1ZB4qOMJrWwafwBHyZCGy6z6bw0HkGhALwGf3rpPojZAr0oAdcGKqlUnQoVREZB3SxLxdR02ily9gd4TR1RRK4V2IBwPkT0VtGwZDZD'

##Mes Infos perso###
me <- getUsers("me", token = fb_oauth, private_info = T)
me$name
me$hometown
############

##Les informations de la page des candidats (Nombre des personnes qui aiment la page)
cand10 <- searchPages('Ali Bongo Ondimba', token=fb_oauth, n = 20) #ID:152476488106875
cand10 <- cand10[cand10$id == '152476488106875', ]

cand20 <- searchPages('Jean Ping', token=fb_oauth, n = 20) #ID:208839279314475
cand20 <- cand20[cand20$id == '208839279314475', ]

cand30 <- searchPages('Raymond ndong Sima', token=fb_oauth, n = 20) #ID: 731196176963824
cand30 <- cand30[cand30$id == '731196176963824', ]

data0 <- rbind(cand10, cand20, cand30)
names(data0)[13] <- 'Noms'

###Figure 1####
fig <- ggplot(data0, aes(x=Noms, y=likes, fill = Noms)) + geom_bar(stat="identity") +
  xlab('') + 
  ylab('') +
  geom_text(aes(label = likes), vjust=-0.2) + guides(fill=FALSE)+
  scale_y_continuous(breaks=c(0,50000, 100000, 300000, 400000))+
  scale_fill_manual(values=col1[c(3,5,8)])+
  theme_bw()+
  ggtitle('Nombre de personnes abonnées \n à la page  officielle facebook')
 
##Les informations de la page des candidats du '2016/08/13' jusqu'a temps present####
cand11 <- getPage(page = '152476488106875', token=fb_oauth, since ='2016/08/13', 
                 until =Sys.Date(), n = 500, reactions = T)

cand21 <- getPage(page = '208839279314475', token=fb_oauth, since ='2016/08/13', 
                 until = Sys.Date(), n = 500, reactions = T)

cand31 <- getPage(page = '731196176963824', token=fb_oauth, since ='2016/08/13', 
                  until =Sys.Date(), n = 500, reactions = T)

data1 <- rbind(cand11, cand21, cand31)

format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}

data1$Date0 <- format.facebook.date(data1$created_time)
data1$Date  <- format(data1$Date0, '%d-%m-%Y')

data2 <- data1 %>%
  group_by(from_name, Date) %>%
    summarise(N = n(),
              Likes = mean(likes_count),
              Commentaires = mean(comments_count),
              Partages = mean(shares_count))

data3 <- melt(as.data.frame(data2), id = c('from_name', 'Date', 'N'))
names(data3)[1] <- 'Noms'

data21 <- data1 %>%
  group_by(from_name) %>%
  summarise(N = n(),
            Likes = round(mean(likes_count), 0),
            Commentaires = round(mean(comments_count), 0),
            Partages = round(mean(shares_count), 0))

data31 <- melt(as.data.frame(data21), id = c('from_name', 'N'))
names(data31)[1] <- 'Noms'

data32 <- data31 %>%
  arrange(Noms) %>%
  group_by(Noms) %>%
    mutate(value.a = cumsum(value)) 

###Figure 2####
fig11 <- ggplot(data32, aes(x=Noms, y=value, fill=variable)) +
  geom_bar(stat="identity") + xlab('') +ylab('')+
  geom_text(aes(y=value.a, label=value), vjust=1.2, colour="black")+
  ggtitle("Nombre moyen d'interactions par \n statut facebook publié")+
  #scale_y_log10(breaks = c(0, 10, 100, 1000, 1500, 2000))+
  scale_fill_manual(values=col1[c(3,5,8)])+
  theme_bw()+ theme(legend.position=c(0.85,0.8)) + labs(fill = '')
  
###Fusion des figures 1 et 2 
fig.1 <- grid.arrange(fig, fig11, ncol=2)


####Figure 3
fig21 <- data3 %>%
  filter(variable == 'Likes') %>%
  ggplot(aes(x = Date, y = value, group = Noms)) + 
  geom_line(aes(color = Noms)) + 
  xlab('')+
  ylab("") +
  scale_y_continuous(breaks=c(0, 500, 1000, 2000, 2500, 3000, 3500))+
  ggtitle('Nombre moyen de <<Likes>> journalier par status facebook publié
          depuis le début de la campagne présidentielle')+
  theme_bw() + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5))+
  theme(legend.position="none")

###Figure 4
fig22 <- data3 %>%
  filter(variable == 'Commentaires') %>%
  ggplot(aes(x = Date, y = value, group = Noms)) + 
  geom_line(aes(color = Noms)) + 
  xlab('')+
  ylab("") +
  scale_y_continuous(breaks=c(0, 100, 150, 250, 350, 400))+
  ggtitle('Nombre moyen de <<Commentaires>> journalier par statut facebook publié 
          depuis le début de la campagne présidentielle')+
  theme_bw() + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5))+
  theme(legend.position="none")

###Figure 5
fig23 <- data3 %>%
  filter(variable == 'Partages') %>%
  ggplot(aes(x = Date, y = value, group = Noms)) + 
  geom_line(aes(color = Noms)) + 
  xlab('')+
  ylab("") +
  scale_y_continuous(breaks=c(0, 100, 200, 300, 350, 400, 450))+
  ggtitle('Nombre moyen de <<Partages>> journalier par status facebook publié 
          depuis le début de la campagne présidentielle')+
  theme_bw() + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) 

###Fusion figures 1, 2 et 3  
fig2.2 <- grid.arrange(fig21, fig22, fig23, ncol=2)



