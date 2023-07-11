library(tidyverse)
possiblities = expand_grid(spin1=seq(.05, 1, by=.05), spin2=seq(.05, 1, by=.05))

possiblities %>% 
  mutate(total_if_stop = spin1,
         total_if_go = spin1+spin2) %>% 
  mutate(scenario = if_else(spin1<=.5, total_if_go, total_if_stop))%>% 
  summarize(too_low = mean(scenario<=.5), winner = mean(scenario>.5 & scenario<=1),
                           bust=mean(scenario>1))%>% 
  pivot_longer(everything())



possiblities = expand_grid(p1_r1=seq(.05, 1, by=.05), p1_r2=seq(.05, 1, by=.05),
                           p2_r1=seq(.05, 1, by=.05), p2_r2=seq(.05, 1, by=.05)) %>%
  mutate(across(everything(), round, 2))

full_set = possiblities %>% 
  mutate(p1_stopped = p1_r1, p1_go = p1_r1+p1_r2,
         p2_given_p1_stopped = if_else(p2_r1>p1_stopped, p2_r1, 
                                       if_else(p2_r1 < p1_stopped, p2_r1+p2_r2,
                                               if_else(p2_r1<.5, p2_r1+p2_r2, p2_r1))), #handle ties
         p2_given_p1_go = if_else((p2_r1>p1_go)|p1_go>1, p2_r1, 
                                  if_else(p2_r1< p1_go, p2_r1+p2_r2,
                                          if_else(p2_r1<.5, p2_r1+p2_r2, p2_r1))), #handle ties
         p2_win_given_p1_go = (p2_given_p1_stopped > p1_go & p2_given_p1_go <= 1) | p1_go>1,
         p2_win_given_p1_stopped = p2_given_p1_stopped>p1_stopped & p2_given_p1_stopped<=1,
         tie_given_p1_go = p2_given_p1_go == p1_go,
         tie_given_p1_stopped = p2_given_p1_stopped == p1_stopped) 

probs_given_stopped = 
  full_set %>% group_by(p1_r1) %>%
  summarize(p2_wins_prob_after_stop = mean(p2_win_given_p1_stopped),
            p2_ties_after_stop = mean(tie_given_p1_stopped)) %>%
  mutate(p1_wins_prob_after_stop = 1-p2_wins_prob_after_stop - p2_ties_after_stop)

probs_given_go = 
  full_set %>% group_by(p1_r1) %>%
  summarize(p2_wins_prob_after_go = mean(p2_win_given_p1_go),
            p2_ties_after_go = mean(tie_given_p1_go)) %>%
  mutate(p1_wins_prob_after_go = 1-p2_wins_prob_after_go - p2_ties_after_go)


decisions = probs_given_stopped %>% left_join(probs_given_go) %>%
  mutate(p1_win_prob_if_stop = p1_wins_prob_after_stop + .5 * p2_ties_after_stop,
         p1_win_prob_if_go = p1_wins_prob_after_go + .5 * p2_ties_after_go,
    p1_decision = if_else(p1_win_prob_if_stop >
                            p1_win_prob_if_go,
         "STOP", "GO"),
         p1_win_prob = if_else(p1_decision=="STOP", p1_wins_prob_after_stop + .5*p2_ties_after_stop,
                               p1_wins_prob_after_go+.5*p2_ties_after_stop))

decisions%>%
  ggplot(aes(x=p1_r1))+geom_point(aes(y=p1_win_prob_if_stop, color="If stop"))+
  geom_point(aes(y=p1_win_prob_if_go, color="If go"))+
  scale_x_continuous("First Spin", breaks=seq(.05, 1, by=.05))+
  scale_y_continuous(NULL, labels=scales::label_percent())+
  guides(color=guide_legend("Probability"))+
  theme_bw()

decisions %>% summarize(mean(p1_win_prob))
