confoodDF <- as.data.frame(confood2)

#group of brand 2 to 5
#sales
colnames <- c("saleb2", "saleb3", "saleb4", "saleb5")
confoodDF[colnames]
confoodDF$salesO <- apply(confoodDF[colnames], 1, sum)
confoodDF$salesOmean <- apply(confoodDF[colnames], 1, mean)
#rprice
colnames <- c("rpriceb2", "rpriceb3", "rpriceb4", "rpriceb5")
confoodDF$rpriceO <- apply(confoodDF[colnames], 1, sum)
confoodDF$rpriceOmean <- apply(confoodDF[colnames], 1, mean)
#aprice
colnames <- c("apriceb2", "apriceb3", "apriceb4", "apriceb5")
confoodDF$apriceO <- apply(confoodDF[colnames], 1, sum)
confoodDF$apriceOmean <- apply(confoodDF[colnames], 1, mean)

#confood 1 vs other
colnames <- c("period", "storenum", "weeknum", "saleb1", "apriceb1", "rpriceb1",
              "promotb1", "salesOmean", "apriceOmean", "rpriceOmean")
confood1 <- confoodDF[colnames]

# régression linéaire
lm = lm(apriceb1 ~ salesOmean+period+storenum+weeknum+apriceOmean+promotb1, data=confood1)
summary(lm)
m<- lm(confood1$apriceb1 ~ confood1$salesOmean)
summary(m)
plot(confood1$salesOmean, confood1$saleb1)
a<- m$coefficients[1]
b<- m$coefficients[2]
abline(a, b)

#Création de données d'apprentissage et de test
ndata<-nrow(confood1)
napp<-round((2/3)*ndata)
ntst<-round(ndata-napp)
set.seed(100)
dataApp<-sample(1:ndata, napp)
data.train<-confood1[dataApp,]
data.test<-confood1[-dataApp,]

#test predire sale1 avec toutes les variables
reg = lm(saleb1 ~ apriceb1+rpriceb1+salesOmean+period+storenum+weeknum+apriceOmean+promotb1, data=data.train)
res<-predict(reg, newdata=data.test)
plot(data.test$saleb1, res)
err_quad <- mean((data.test$saleb1-res)^2)

summary(reg)

#test avec les variables les plus significatives seulement
reg = lm(saleb1 ~ apriceb1+rpriceb1+apriceOmean+promotb1, data=data.train)
res<-predict(reg, newdata=data.test)
plot(data.test$saleb1, res)
err_quad <- mean((data.test$saleb1-res)^2)

#test avec les variables les plus significatives seulement
reg = lm(saleb1 ~ apriceb1+rpriceb1, data=data.train)
res<-predict(reg, newdata=data.test)
plot(data.test$saleb1, res)
err_quad <- mean((data.test$saleb1-res)^2)

#Validation


#change promotion variable to dummy


#corrélation
M <- cor(confood1[which(confood1$storenum==1),])
corrplot(M, method='color')

M <- cor(confood1[which(confood1$storenum==2),])
corrplot(M, method='color')

#acp
library(factoextra)
res.pca <- prcomp(confood1, scale = TRUE)
fviz_eig(res.pca)
# 3 ou 4 variables suffisent a expliquer, méthode du coude
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Colorer par le cos2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)
fviz_pca_var(res.pca,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)
#Les variables corrélées positivement sont du même côté du graphique. 
#Les variables corrélées négativement sont sur des côtés opposés du graphique.

# Valeurs propres
eig.val <- get_eigenvalue(res.pca)
eig.val

library("corrplot")
corrplot(res.var$cos2, is.corr=FALSE)
#on remarque que la combinaison rpriceOmean, apriceOmean et storenum

#autre methode prcomp
prin_comp <- prcomp(data.train, scale. = T)
std_dev <- prin_comp$sdev
pr_var <- std_dev^2
pr_var[1:10]
prop_varex <- pr_var/sum(pr_var)
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
#On fait des boxplots pour voir les variables
box<-boxplot(confood1$saleb1)
box$out
b2<-confood1$saleb1[-which(confood1$saleb1%in%box$out)]
boxplot(b2)
