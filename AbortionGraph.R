library(rvest)
library(dplyr)
library(ggplot2)
library(readr)
library(forcats)
url <- "https://www.mdch.state.mi.us/osr/abortion/Tab_US.asp"
aborts <- url %>% html() %>%  html_nodes(xpath='/html/body/article/div[1]/center/table') %>%
  html_table(fill=T)
data=  aborts[[1]] %>% select(Year, 2)  %>% setNames(c("Year", "Abortions")) %>% slice(-1) %>%
  mutate(Abortions =parse_number(Abortions), across(where(is.character), as.numeric)) %>%
  filter(complete.cases(.)) %>% mutate(
    Interpolated = Year %in% c(2001:2003, 2006, 2009, 2012, 2015)
  )




pres=data.frame(President = c("Carter", "Reagan", "Bush", "Clinton", "Bush", "Obama", "Trump"),
                Start_Date=c(1980, 1981, 1989, 1993, 2001, 2009, 2017),
                End_Date=c(1980, 1988, 1992, 2000, 2008, 2016, 2020),
                Party=c("D", "R", "R", "D", "R", "D", "R"))

ggplot(data)+
  geom_point(aes(x=Year+.5, y=Abortions/1000000, shape=Interpolated))+
  geom_rect(data= pres, aes(ymin= .80, ymax=1.70, xmin=Start_Date, xmax=End_Date+1, fill=fct_rev(Party)), alpha=.2)+
  theme_minimal()+
  ylab("Estimated Abortions per Year (Millions)")+
  xlab(NULL)+
  scale_x_continuous(breaks= seq(1980, 2020, by=5))+
  scale_fill_discrete(name="Party in White House")+
  scale_shape_discrete(name=NULL, labels="Interpolated", breaks=1)+
  theme(legend.position = "bottom",
          panel.grid.minor = element_blank()
         )+
  scale_y_continuous(breaks = seq(.8, 1.7, length.out=10), 
                     labels=c(.8,.9, 1, 1.1, 1.2, 1.3,1.4, 1.5, 1.6, 1.7))+
  guides(shape=guide_legend(order=2),
         fill=guide_legend(order=1))+
  ggtitle("US Abortions over Time")+
    labs(caption="Source: Guttmacher, https://www.mdch.state.mi.us/osr/abortion/Tab_US.asp")



