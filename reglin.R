confoodDF <- as.data.frame(confood2)
cf<-read.table(file = 'C:/Users/alizee/Downloads/finaltab.tsv', sep = '\t', header = TRUE)
cf_agg <- confoodDF %>% group_by(weeknum) 
sum = summarise(cf_agg, saleb1 = sum(saleb1), saleb2 = sum(saleb2), saleb4 = sum(saleb4), saleb3 = sum(saleb3),
                saleb5 = sum(saleb5), apriceb1 = mean(apriceb1), apriceb2 = mean(apriceb2), apriceb3 = mean(apriceb3),
                apriceb4= mean(apriceb4), apriceb5 = sum(apriceb5), rpriceb1 = mean(rpriceb1), rpriceb2 = mean(rpriceb2), 
                rpriceb3 = mean(rpriceb3), rpriceb4= mean(rpriceb4), rprice5 = sum(rpriceb5), promotb1 = mean(promotb1),
                promotb2 = mean(promotb2), promotb3 = mean(promotb3), promotb4 = mean(promotb4), 
                promotb5 = mean(promotb5))

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
ndata<-nrow(sum)
napp<-round((2/3)*ndata)
ntst<-round(ndata-napp)
set.seed(100)
dataApp<-sample(1:ndata, napp)
data.train<-sum[dataApp,]
data.test<-sum[-dataApp,]

#test predire saleb1 avec toutes les variables
reg = lm(saleb1 ~ apriceb1+rpriceb1+salesOmean+period+storenum+weeknum+apriceOmean+promotb1, data=data.train)
reg = lm(saleb1 ~. -saleb1, data=data.train)
res<-predict(reg, newdata=data.test)
plot(data.test$saleb1, res)
err_quad <- mean((data.test$saleb1-res)^2)

summary(reg)

sum <- subset(sum, !(sum$saleb1==58525))
sum <- subset(sum, !(sum$saleb1==34270))

#test avec les variables les plus significatives seulement
reg = lm(saleb1 ~ apriceb1+rpriceb1+promotb1, data=data.train)
res<-predict(reg, newdata=data.test)
plot(data.test$saleb1, res)
err_quad <- mean((data.test$saleb1-res)^2)

#test avec les variables les plus significatives seulement
reg = lm(saleb1 ~ rpriceOmean+storenum+weeknum+rpriceb1+apriceOmean, data=data.train)
res<-predict(reg, newdata=data.test)
plot(data.test$saleb1, res)
err_quad <- mean((data.test$saleb1-res)^2)


#change promotion variable to dummy


#corrélation
M <- cor(confood1[which(confood1$storenum==1),])
corrplot(M, method='color')

M <- cor(confood1[which(confood1$storenum==2),])
corrplot(M, method='color')

#acp
library(factoextra)
res.pca <- prcomp(sum, scale = TRUE)
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
M <- cor(sum)
corrplot(M, method='color')

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
box<-boxplot(sum$saleb1)
box$out
b2<-sum$saleb1[-which(sum$saleb1%in%box$out)]
boxplot(b2)
newConfood <- subset(sum, !(sum$saleb1%in%box$out))
