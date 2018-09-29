
# L'idée est de convertir les images en fichier CSV pour les manipuler 
library(EBImage)
save <- "~/Desktop/EXO3 test/Mnist.csv"
# Récupérer la taille 

count_digits <- length(list.files())
df <- data.frame()
label <- 0
for(i in 0:(count_digits-1)){
  directory <- paste("~/Desktop/EXO3 test/",i)
  directory <- paste(directory,"/",sep="")
  setwd(directory)
  print(directory)
  images <- list()
  print(images)
  for(i in 1:length(images))
  {
    print(images[i])
    img <- readImage(images[i])
    img_matrix <- img@.Data
    img_vector <- as.vector(t(img_matrix))
    vec <- c(label, img_vector)
    df <- rbind(df,vec)
  }
  label <- label + 1
  if(i==10) break
  if(i==0) i <- i+1
}
# donnér label pour dataset 
names(df) <- c("label", paste("pixel", c(1:img_size)))
# Aprés la construction du fichier CSV , 
# importer les données 
Mnist <- read.csv("~/Desktop/EXO3 test/Mnist.csv" , header = TRUE)


# Faire le split  , 80 % pour l'apprentissage et 20% pour Test 

set.seed(58555)
library(caTools)
split = sample.split(Mnist$label, SplitRatio = 0.8)
training_set = subset(Mnist, split == TRUE)
test_set = subset(Mnist, split == FALSE)
dim(test_set)
dim(training_set)

library(rpart) 
library(rpart.plot)

### On construit le modele avec rpart()

Rpartmodel <- rpart(as.factor(label) ~ . , data=training_set )

plot(Rpartmodel)
text(Rpartmodel)
rpart.plot(Rpartmodel)
summary(Rpartmodel)




