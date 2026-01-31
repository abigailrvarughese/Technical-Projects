library(mosaic)
library(tidyverse)
library(moderndive)
library(ggthemes)

glimpse(ski)

ski %>% 
  ggplot(aes(x=continent))+
  geom_bar(fill="darkslategrey")+
  labs(x="Continent",
       y="Number of Ski Areas",
       title="Ski Area by Continent")+
  theme_stata()

ski %>% 
  ggplot(aes(x=continent, y=price))+
  geom_boxplot()+
  geom_jitter(color="darkslategrey", alpha=.25)+
  labs(x="Continent",
       y="Price (in Euros) of 1-Day Adult Ski Pass",
       title="Ski Pass Prices Across Continents")+
  theme_stata()

ski %>% 
  ggplot(aes(x=longitude, y=latitude, color=continent=="Asia")) +
  geom_point(alpha = 0.6, size = 2.5) +
  scale_color_manual(values = c('darkslategrey', 'pink')) +
  theme_stata() + theme(legend.position = 'none')+
  labs(x="Longitude (degrees)",
       y="Latitude (degrees)",
       title="Ski Locations by Continent")

dicker_sim = do(100000)*nflip(61, prob=.84)
head(dicker_sim)
ggplot(dicker_sim)+
  geom_histogram(aes(x=nflip), fill="darkslategrey", color="black", binwidth=1,)+
  theme_stata()+
  labs(title="Simulated Counts of Cameron Dicker's Made Field Goals in 61 Tries")
sum(dicker_sim >= 57)
2363/100000
confint(dicker_sim, level=.95)

glimpse(ebay)

ebay %>% 
  ggplot(aes(x=rev_ratio))+
  geom_histogram(fill="darkslategrey")+
  facet_wrap(~adwords)+
  theme_stata()+
  labs(x="Ratio of Revenue After to Revenue Before",
       y="Count",
       title="Revenue Ratio by Treatment or Control Group")
glimpse(ebay)
ebay_adwords = ebay %>% 
  group_by(adwords) %>% 
  summarise(avg = mean(rev_ratio))

t.test(rev_ratio~adwords, data=ebay)

ebay_model = lm(rev_ratio~adwords, data=ebay)
coef(ebay_model)
confint(ebay_model, level=.95)

glimpse(films)

ggplot(films)+
  geom_bar(aes(x=rated, fill=test))+
  scale_fill_manual(values = c("darkslategray", "pink"))+
  labs(x="MPA Rating",
       y="Count",
       fill="Bechdel Test Result",
       title="Bechdel Test Results by MPA Rating")+
  theme_stata()
  
prop.test(test~R_rated, data=films)

films_rating = films %>% 
  filter(votes > quantile(votes, .75)) %>% 
  arrange(desc(domestic)) %>% 
  select(title, domestic, director, year, test)
  head(10)

head(films_rating, 10)
glimpse(films_rating)

films_low = films %>% 
  filter(budget < mean(budget)) %>% 
  arrange(desc(domestic)) %>% 
  select(title, domestic, director, year, test, budget)

films_high = films %>% 
  filter(budget > mean(budget)) %>% 
  arrange(desc(domestic)) %>% 
  select(title, domestic, director, year, test, budget)

head(films_low, 10)
head(films_high, 10)

glimpse(ATT)
att_model = lm(skips ~ solder+size+solder:size, data=ATT)
coef(att_model)

get_regression_table(att_model, conf.level = .95, digits=2)
  round(2)

ATT %>% 
  ggplot(aes(x=skips))+
  geom_histogram(fill="darkslategrey", binwidth=2.5)+
  facet_grid(solder~size)+
  theme_stata()+
  labs(title="Distribution of Skips By Solder and Sizes")
