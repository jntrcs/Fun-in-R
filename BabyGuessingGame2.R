

library(googlesheets4)
library(tidyverse)

raw = read_sheet("https://docs.google.com/spreadsheets/d/1temfXOV3zKE3uV5klJp3lNhAv_bDeKPyO_aFodOsBbc/edit?resourcekey#gid=1789446562")

guesses=raw %>% group_by(name = `Your Name`) %>%
  filter(Timestamp == min(Timestamp)) %>%
  ungroup %>%
  mutate(gender=Gender,
         due_date = as.Date(`Birthday (Due on Nov. 1!)`), 
         due_date = if_else(due_date<Sys.Date()-10000, ymd("2023-11-02"), due_date),
         weight = Pounds+Ounces/16) #correct for an input error

scores = expand_grid(weight= seq(6.5, 9.5, by=1/16),
                     birthday = seq.Date(ymd("2023-10-07"), to=ymd("2023-11-10"),
                                         by=1)) %>%
  rowwise() %>%
  reframe(Guesser = guesses$name,
            score = if_else(guesses$gender=="Boy", 10, 0)+
              pmax(0, 10-8*abs(guesses$weight-weight))+
              pmax(0, 10-abs(as.numeric(guesses$due_date-birthday))),
            weight=weight,
            birthday=birthday) %>%
  group_by(weight, birthday) %>%
  summarize(winner=Guesser[score==max(score)][1],
            winning_points=max(score),
            ties = sum(score==max(score))>1) %>%
  ungroup %>% 
  mutate(weight_upper=weight+1/16, next_day=birthday+1,
         winner=if_else(ties, "Tie", winner))

ggplot(guesses, aes(x=due_date, y=weight,
                                             label=name))+
  geom_rect(data=scores, aes(xmin=birthday, ymin=weight, fill=winner, 
                             xmax=next_day, ymax=weight_upper), 
            inherit.aes = F,
            alpha=.5)+
  geom_point(size=3, aes(color=gender, shape=gender))+
  
  ggrepel::geom_text_repel(show.legend = F, aes(color=gender))+
  geom_vline(xintercept = ymd("2023-11-01"), color="green")+
  annotate("text", x=ymd("2023-11-01"), y=9, label="Due")+
  theme_bw()+
  scale_x_date("Birthday ", date_labels = "%m/%d",
               date_breaks = "2 day", date_minor_breaks = "1 day", expand = expansion())+
  scale_y_continuous("Weight (lbs)", breaks=function(x)(seq(floor(x[1]), ceiling(x[2]), by=1)),
                     minor_breaks = function(x)(seq(floor(x[1]), ceiling(x[2]), by=1/4)),
                     expand=expansion(add=c(0, .15)))+
  guides(color=guide_legend(title=NULL), fill=guide_legend("Winner:"))+
  theme(legend.position = "bottom")+
  scale_color_manual(values=c("#1eb6e1", "#f30cb4"))+
  guides(shape=guide_legend(NULL))+  
  ggtitle("Guessing Game Win Chart", subtitle="It's a boy!")


scores = expand_grid(weight= seq(6.5, 9.5, by=1/16),
                     birthday = seq.Date(ymd("2023-10-07"), to=ymd("2023-11-10"),
                                         by=1)) %>%
  rowwise() %>%
  reframe(Guesser = guesses$name,
          score = if_else(guesses$gender=="Girl", 10, 0)+
            pmax(0, 10-8*abs(guesses$weight-weight))+
            pmax(0, 10-abs(as.numeric(guesses$due_date-birthday))),
          weight=weight,
          birthday=birthday) %>%
  group_by(weight, birthday) %>%
  summarize(winner=Guesser[score==max(score)][1],
            winning_points=max(score),
            ties = sum(score==max(score))>1) %>%
  ungroup %>% 
  mutate(weight_upper=weight+1/16, next_day=birthday+1,
         winner=if_else(ties, "Tie", winner))

ggplot(guesses, aes(x=due_date, y=weight,
                                             label=name))+
  geom_rect(data=scores, aes(xmin=birthday, ymin=weight, fill=winner, 
                             xmax=next_day, ymax=weight_upper), 
            inherit.aes = F,
            alpha=.5)+
  geom_point(size=3, aes(color=gender, shape=gender))+
  
  ggrepel::geom_text_repel(show.legend = F, aes(color=gender))+
  geom_vline(xintercept = ymd("2023-11-01"), color="green")+
  annotate("text", x=ymd("2023-11-01"), y=9, label="Due")+
  theme_bw()+
  scale_x_date("Birthday ", date_labels = "%m/%d",
               date_breaks = "2 day", date_minor_breaks = "1 day", expand = expansion())+
  scale_y_continuous("Weight (lbs)", breaks=function(x)(seq(floor(x[1]), ceiling(x[2]), by=1)),
                     minor_breaks = function(x)(seq(floor(x[1]), ceiling(x[2]), by=1/4)),
                     expand=expansion(add=c(0, .15)))+
  guides(color=guide_legend(title=NULL), fill=guide_legend("Winner:"))+
  theme(legend.position = "bottom")+
  scale_color_manual(values=c("#1eb6e1", "#f30cb4"))+
  guides(shape=guide_legend(NULL))+  
  ggtitle("Guessing Game Win Chart", subtitle="If it was a girl")

