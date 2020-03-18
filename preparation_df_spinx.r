library(readxl)
library(dplyr)
library(stringr)

#############"""" traitement donnees sphinx ###########"

setwd('C:/Users/francois.lafont@ccomptes.fr/Desktop/2019.07 Algues Vertes')

df_sphinx <- read_excel('./Sphinx/Answers/COUR_DES_COMPTES_Algues_vertes_v3_3-16-2020_12_7.xlsx')
colnames(df_sphinx) <- str_remove_all(colnames(df_sphinx), "^\\d*?\\.\\s")

####################################################################################################
############################ On ne garde que les réponses complètes
####################################################################################################
df_sphinx <- df_sphinx %>% filter(PROGRESSION=='Terminé')

####################################################################################################
########### d'abord on va rendre exploitable la surface en ha en enlevant les caractères #### 
####################################################################################################


df_sphinx <- df_sphinx %>%
  
  ## make the sau data numeric
  mutate(exp_sau_Autre = str_remove_all(exp_sau_Autre,'[a-z]'),
         exp_sau_Autre = str_replace(exp_sau_Autre,',','.'),
         exp_sau_Autre = as.numeric(exp_sau_Autre),
         
         part_sau_dans_bvav_Autre = str_remove_all(part_sau_dans_bvav_Autre,'[a-z]'),
         part_sau_dans_bvav_Autre = str_replace(part_sau_dans_bvav_Autre, ',', '.'),
         part_sau_dans_bvav_Autre = as.numeric(part_sau_dans_bvav_Autre), 
         
         part_sau_prairie_Autre = str_remove_all(part_sau_prairie_Autre,'[a-z]'),
         part_sau_prairie_Autre = str_replace(part_sau_prairie_Autre, ',', '.'),
         part_sau_prairie_Autre = as.numeric(part_sau_prairie_Autre),
         
         part_sau_corn_Autre = str_remove_all(part_sau_corn_Autre,'[a-z]'),
         part_sau_corn_Autre = str_replace(part_sau_corn_Autre, ',', '.'),
         part_sau_corn_Autre = as.numeric(part_sau_corn_Autre)) %>%
  
  ## calculate ratios of specific crops
  mutate(ratio_sau_bv = part_sau_dans_bvav_Autre/exp_sau_Autre,
         ratio_prairie_bv = part_sau_prairie_Autre/exp_sau_Autre,
         ratio_corn_bv = part_sau_corn_Autre/exp_sau_Autre,
         
         type_exp_bv = case_when(
           ratio_sau_bv==1 ~ 'Exploitation entièrement dans BVAV',
           ratio_sau_bv<1 & ratio_sau_bv > 0.5~'Exploitation principalement dans BVAV',
           ratio_sau_bv >0 & ratio_sau_bv <= 0.5 ~ 'Exploitation principalement hors BVAV',
           exp_sau=='Hors sol intégral'~ 'Exploitation hors-sol',
           part_sau_dans_bvav == 'Ne sais pas'~ 'part SAU dans BVAV inconnue'
         ))

####################################################################
######################## separate exploitations ####################
####################### 100% dans BVAV, + 50%, - 50% ###############
####################################################################
exp_full_bv <- df_sphinx %>%
  
  filter(ratio_sau_bv == 1)

exp_dominante_bv <- df_sphinx %>%
  filter(ratio_sau_bv <1 & ratio_sau_bv >=0.5)

exp_peu_bv <- df_sphinx %>%
  filter(ratio_sau_bv >0 & ratio_sau_bv <0.5)

exp_bv_na <- df_sphinx %>%
  filter(is.na(ratio_sau_bv))

################################################################################
################# UNE DF par BV
################################################################################
df_bv <- list()

for (i in unique(df_sphinx$exp_bvav2)){
  df_bv[[i]] <- df_sphinx %>% filter(exp_bvav2==i)
}

###

########################################################
#############  connaissance selon les typologies sau dans bvav####################
########################################################

# ## plot 
# df_sphinx %>%
#   ggplot(aes(x=type_exp_bv, y = ''))
# 
# #plot(df_sphinx$connaissance_service_plav,df_sphinx$type_exp_bv, col = type_exp_bv)
# 
# plot(table(df_sphinx$type_exp_bv,df_sphinx$connaissance_service_plav))
# 
# plot(table(exp_full_bv$connaissance_service_plav))



