# install.packages("ggwordcloud")
library(ggwordcloud)

# create word cloud with 40 words from each group; scaling is RADIUS
lasso_fit %>% 
  pull_workflow_fit() %>% 
  vi(lambda = best_tune$penalty) %>% 
  group_by(Sign) %>% 
  # i put n = 40 just to have more words show up
  top_n(40, wt = abs(Importance)) %>% 
  ungroup() %>% 
  mutate(Importance = abs(Importance),
         Variable = str_remove(Variable, "tfidf_description_"),
         Variable = fct_reorder(Variable, Importance),
         effect = ifelse(Sign == "NEG", "HIGHER", "LOWER")) %>% 
  ggplot(aes(label = Variable, size = Importance, color = effect)) +
  # radius can be changed under "range = c(0, i)"
  geom_text_wordcloud() + scale_radius(range = c(0, 20), limits = c(0, NA)) +
  scale_color_manual(values = c("blue","red")) +
  theme_minimal()

# create word cloud with 40 words from each group; scaling is LINEAR
# text is similar in size since the coefficients for the words are pretty close to each other
lasso_fit %>% 
  pull_workflow_fit() %>% 
  vi(lambda = best_tune$penalty) %>% 
  group_by(Sign) %>% 
  top_n(40, wt = abs(Importance)) %>% 
  ungroup() %>% 
  mutate(Importance = abs(Importance),
         Variable = str_remove(Variable, "tfidf_description_"),
         Variable = fct_reorder(Variable, Importance),
         effect = ifelse(Sign == "NEG", "HIGHER", "LOWER")) %>% 
  ggplot(aes(label = Variable, size = Importance, color = effect)) +
  geom_text_wordcloud() + scale_size_area(max_size = 15) +
  scale_color_manual(values = c("blue","red")) +
  theme_minimal()
