library(cluster)
library(fpc)
library(dplyr)

visualize_matrix = function(m1,title){
  m1 <- t(apply(m1, 2, rev))
  colors2spaceThrough <- c('black', 'grey', 'white')
  nsamples =  256
  customColorMap <- colorRampPalette(colors2spaceThrough)(nsamples)
  image(m1, col = customColorMap, axes = FALSE)
  box()
  title(main = title, font.main = 4)
}

dossier = "train_voyelles"
train_labels=readLines("train_labels.txt")

filenames_path = dir(path = dossier, full.names = TRUE)
filenames = dir(path = dossier)

#matrice du dossier train_voyellles
numero_fichier = sample(1:length(filenames),4500)
liste=NULL
for (int in c(numero_fichier)){
  dat=filenames_path[int]
  liste=rbind(liste,dat)
}
datalist = t(as.data.frame(lapply(liste, function(x)read.delim(x, header=F))))

aggregate(V2 ~ V1, data=datalist,c)

for (i in range(length(liste))){
  read.table(liste[i])
}

read.table(liste[2])

do.call(rbind, lapply(liste, read.table, sep="\t"))

liste_df=data.frame(liste)
raw.data <- liste_df$liste %>%
  # 'do' the function for each row in turn
  rowwise() %>%
  do(., read.table(file=.$liste_df$liste))

do(.,read.table(file = liste_df$liste))
data=as_tibble(datalist)
data%>% distinct()

km=kmeans(datalist,5,iter.max = 10)

#if (kms$ifault==4) { km = kmeans(datalist, 5, algorithm="MacQueen"); }

###affichage et table
plotcluster(datalist,km$cluster)
tab=as.data.frame(km$cluster)
#équivalence cluster voyelle
tab$voyelles_cluster[tab$`km$cluster`=="1"] = 'A'
tab$voyelles_cluster[tab$`km$cluster`=="2"] = 'E'
tab$voyelles_cluster[tab$`km$cluster`=="3"] = 'I'
tab$voyelles_cluster[tab$`km$cluster`=="4"] = 'O'
tab$voyelles_cluster[tab$`km$cluster`=="5"] = 'U'
#voyelles prédites pour chaque image
tab$voyelles_pred=train_labels

 
#nb de voyelles par cluster
aggregate(voyelles_cluster~km$cluster, data=tab, FUN = length)

#répartition des voyelles pour le cluster 2
x=as.data.frame(tab %>% filter (km$cluster == '2') %>% pull(voyelles_cluster))
colnames(x)=c("voyelles")
x %>% group_by(`voyelles`) %>%tally()


# matrice de confusion
vecct_bool = (tab$voyelles_cluster == tab$voyelles_pred)
tab$bonne_reponse = vecct_bool
1-sum(tab$bonne_reponse)/nrow(tab) #recall ou pourcentage d'éléments bien classés
mat_conf=table(tab$voyelles_cluster,tab$voyelles_pred)
mat_conf

errors=NULL
for (i in range(length(liste))){
  if (tab$voyelles_cluster != tab$voyelles_pred){
    errors=rbind(errors,i)
  }
}

by(tab, seq_len(nrow(tab)),function(row),)

errors=NULL
for (i in 1:nrow(tab)){
  if (tab$voyelles_cluster != tab$voyelles_pred){
    errors=rbind(errors,i)
  }
}



