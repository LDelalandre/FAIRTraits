lafage <- read.csv2("output/Traitdata_test_lafagetemporary.csv")

lafage %>% 
  filter(verbatimTraitName %in% c("LDMC","LNC")) %>% 
  filter(!is.na(verbatimTraitValue)) %>% 
  group_by(Code_Sp, verbatimTraitName) %>%
  mutate(verbatimTraitValue = as.numeric(verbatimTraitValue)) %>% 
  summarize(mean_trait = mean(verbatimTraitValue,na.rm = T))  %>% 
  spread(key = verbatimTraitName,value = mean_trait) %>% 
  ggplot(aes(x= LNC, y = LDMC)) +
  geom_point()

# regarder le compromis avec les abondances utilisées par Maud sur son gradient
# Puis regader les paramètres (pente, R², RMSE)  en corrigeant :
# - pour le fait qu'on n'a pas le même nombre de points entre espèces et communautés
# - pour le fait que les valeurs sont moins étendues sur les deux axes en CWM
# NB : faire ça en SMA !!

# NB la relation n'est pas forcément très belle. Il y a de la plasticité qui joue : traits mesurés dans des conditions environnementales très différentes (natif et fertile)