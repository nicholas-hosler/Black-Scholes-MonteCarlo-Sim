# Monte Carlo Simulation Game

library(tidyverse)





scores <- 
  
  

  
  ngames_sim <- crossing(trials = 1:10000, 
         ngames = 12 * 2 ^ (0:7)) %>%         
  unnest(game = map(ngames, seq_len)) %>% 
  mutate(result = sample(c(1,0,.5),
         n(),
         replace = TRUE, 
         prob = c(.2, .15, .65))) %>% 
  group_by(ngames,trials) %>% 
  summarize(score = sum(result)) %>% 
  mutate(win = mean(score > ngames / 2))


  ngames_sim %>% 
    ggplot(aes(ngames,win)) + 
    geom_line() +
    geom_point() +
    scale_x_log10() + 
    scale_y_continuous(labels = scales::percent_format())+
    labs(x = "# of games",
         y = "Probability better player wins")




  scores %>% 
  ggplot(aes(score)) + 
  geom_histogram(binwidth = .25) +
  geom_vline(color = "red", xintercept = 6.5)
  
  scores %>% 
      summarize(mean(score >= 6.5))


