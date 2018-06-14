# The link for obtaing the dataset is - "https://www.kaggle.com/c/msk-redefining-cancer-treatment/data"

library("readr")
library("stringr")
library("plyr")
library("dplyr")


train_txt_dump <- tibble(text = read_lines('training_text', skip = 1))
# tibble is a trimmed down version of data.frame() function.
# skip = 1 indicates that it takes new row after every line of the original dataset.

typeof(train_txt_dump)
# here train_txt_dump is a "list" type object.
library("tidyr")

train_txt <- separate(train_txt_dump,text, into = c("ID", "txt"), sep = "\\|\\|")
# in "sep" argument it interprate character as a regular expression.

train_txt <- mutate(train_txt,ID = as.integer(ID))
# mutate function is used to transform the data frame.

train_var <- read.csv("training_variantsnew.txt")

library("ggplot2")

# Drawing plot for genes.
# Since genes are of 264 types in the data set, so plotting them is very difficult.
# So we will plot only those genes whose frequency is greater than equal to 40.
top_gene1 <- group_by(train_var,Gene)
# it group the data according to Gene.
top_gene2 <- summarise(top_gene1,length(duplicated(Gene)))
# Here all the duplicated genes are present.It contain only 2 columns i.e gene and 
# its frequency.
top_gene3 <- subset(top_gene2,`length(duplicated(Gene))` >= 40)
# It contain names and frequency of only those genes whose frequency is greater than equal
# to 40.
z <- qplot(top_gene3$Gene,top_gene3$`length(duplicated(Gene))`,xlab = "Gene",ylab = "Frequency")
# plot of frequency of top occuring genes. 

# Now drawing plots for variations.
# Here no. of distinct variations in our data set were 2996. 
# So we will plot only those variations whose frequency is greater than 2. 
top_variation1 <- group_by(train_var,Variation)
# group_by() function is present is dplyr package.
top_variation2 <- summarise(top_variation1,length(duplicated(Variation)))
# summarise() function is present in dplyr package.
top_variation3 <- subset(top_variation2,`length(duplicated(Variation))` > 2)
p <- qplot(top_variation3$Variation,top_variation3$`length(duplicated(Variation))`,xlab = "Variation",ylab = "Frequency")

# Now let us make plot for classes of genetic mutation.
q <- qplot(train_var$Class,xlab = "Class",ylab = "Frequency")

# Now let us make a plot for gene and class.
# Now we will plot for only top occuring genes.
alt <- subset(train_var,Gene %in% str_c(top_gene3$Gene))
# str_c() function is present is stringr package.
# top_gene3 consisted of top occuring genes, so we took only those rows which had those genes.
m <- ggplot(data = alt,aes(Class))
m <- m + geom_bar(fill = "steelblue")
# here we gave geom_bar() and not geom_point() because y axis is not specified.
m <- m + facet_wrap(~Gene)
# here facet_wrap() wraps a 1-D sequence of panels into 2-D according to Gene variable. 
# we could have used facets = ~Gene , it would have shown graphs equal to distinct number of genes but all in one row.
# So we have used facet_wrap() function for better visualisation.

# Other plot of same gene and class can also be, but visualisation is not as good as from above.
n <- ggplot(data = alt,aes(Gene))
n <- n + geom_bar()
n <- n + facet_wrap(~Class)

# Now making plots for gene and top occuring variation
alt2 <- subset(train_var,Variation %in% str_c(top_variation3$Variation))

o <- ggplot(data = alt2,aes(Class))
o <- o + geom_bar()
o <- o + facet_wrap(~Variation)


train_txt$Class <- train_var$Class
# adding class column to our data frame.
train_txt2 <- as.data.frame(train_txt)

library("stringr")

library("tm")

memory.limit(size = 70000)

corp <- VCorpus(VectorSource(train_txt$txt))

typeof(corp)
# from the typeof() function you will come to know that corp is a list.

corp <- tm_map(
  corp, 
  function(x) {
    content(x) <- iconv(enc2utf8(as.character(content(x))), sub = "byte")
    return(x)
  }
)
# above function is there because there were non-breaking characters and other special characters 
# in our text data, so when NLP is applied, it cannot recognise these special characters.So we have converted
# everything to UTF-8 encoding.

corp <- tm_map(corp,content_transformer(tolower))
# above function tm_map() basically applies a function given inside it, to the object given inside it.
# in above case our whole corp object is converted to lower case. So that no two same words are presented
# as different words just because of their case.

corp <- tm_map(corp,removeNumbers)
# above function remove the numbers from the corp object.

corp <- tm_map(corp,removePunctuation)
# above function remove the punctuationslike dots,commas,colon,semi-colon,etc.

library("SnowballC")
# by loading the Snowballc package we get a list of words that are known as stopwords.

corp <- tm_map(corp,removeWords,stopwords())
# above function remove the stop-words from the corp object. Stopwords include for,a,this,and,or,etc. 

corp <- tm_map(corp,stemDocument)
# above function stem the corp object.By stemming we mean replacing a word by its root word.
# for eg- work,worked,will work,working all these represent the same thing. So all these are replaced by
# their root word i.e work.

corp <- tm_map(corp,stripWhitespace)
# above command remove the whitespaces from our corp object.

# Now let us make a word cloud
# For making a word cloud you need to have TermDocumentMatrix
# Difference between DTM and TDM is that the term document matrix has each corpus word represented 
# as a row with documents as columns. Where as the document term matrix is the transposition of the
# TDM so each document is a row and each word is a column.
tdm <- TermDocumentMatrix(corp)
r <- as.matrix(tdm)
# above command turns the tdm to a normal matrix.
v <- sort(rowSums(r),decreasing=TRUE)
# above command sorted the rows according to the sum of values of each column.
# as each word here represents row, so we have got all the words in their decreasing order of frequency.
d <- data.frame(word = names(v),freq=v)
# In above name() function get the names of v. i.e the words.
x <- nrow(d)
# Now let us remove special characters which were converted as UTF-8 characters that are not normal
# english characters from the data set.
for (i in 1:x) {
  if(grepl("^[a-zA-Z0-9 ]*$", d[i,1])==FALSE){ 
    d <- d[-i,]
  }
}

# Now let us see occuring words.
head(d)

# Now let us generate the word cloud.
# First load the package wordcloud.

library("wordcloud")

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,max.words=200, random.order=FALSE)
# min.freq tells that the words with frequency below min.freq will not be plotted, max.words tells the
# maximum number of words to be plotted, random.order = FALSE suggest the words are not plotted in random 
# order, but in decreasing frequency.

# The word cloud shows that mutat,cell,activ,cancer,mutant,tumor are the most important words.

# Now if you want to plot the top 20 frequent words, do,
topd <- d[1:20,]
# topd contain the top 20 most frequently occuring words.
u <- qplot(topd$word,topd$freq,xlab = "Word",ylab = "Frequency")

# Now let us make a proper data-set so that analysis can be done using machine learning algorithms.
dtm <- DocumentTermMatrix(corp)

dtm
# above command will show you the sparcity of the above matrix.
# by sparcity we mean the columns which are 0 in almost all the columns.
dtm <- removeSparseTerms(dtm,0.95)
#above command reduce the sparcity of the matrix i.e we are keeping only the words that are more frequent
# By 0.95 we mean that we will keep 95% of the words that are most frequent in this sparse matrix of features.

orig <- as.data.frame(as.matrix(dtm))
# we are making a data frame from the above matrix.

orig$mutation <- train_txt$Class
# adding the outcome variable to our data frame naming it "mutation".

orig$mutation <- as.factor(orig$mutation)
# changing the data-type of mutation from integer to class.

# We don't want special characters like non space character as our column so let us remove it
x <- colnames(orig)
for (i in 1:length(x)) {
  if(grepl("^[a-zA-Z0-9 ]*$", x[i])==FALSE){ 
    y <- which(colnames(orig) == x[i])
    y <- as.numeric(y)
    
    orig <- orig[,-y]}
}

colnames(orig)[which(names(orig) == "break")] <- "altbreak"
colnames(orig)[which(names(orig) == "function")] <- "altfunction"
colnames(orig)[which(names(orig) == "next")] <- "altnext"
colnames(orig)[which(names(orig) == "repeat")] <- "altrepeat"
z <- which(colnames(orig) == "mutation")
# in above command we are trying to change some column names as these are the parameter names in random forest. 

library("caret")
library("rpart")
library("randomForest")
library("rattle")
library("e1071")
library("pROC")
library("ROCR")
library("xgboost")

set.seed(3230)
# set.seed() is done so that when you run your program again and again, results don't get too much affected.


intrain <- createDataPartition(y = orig$mutation,p = 0.75,list = FALSE)
# The above command shows that we are splitting the data based on variable Result,
# and the splitting will be on 75 to 25.

# Now make two data sets training and testing based on above partition
# where training will contain 75% of data and testing will contain 25% of data

training1 <- orig[intrain,]
testing1 <- orig[-intrain,]

fit <- rpart(mutation~.,data = training1,method = "class")
# above command suggest that we are trying to apply decision tree model,method = "class" is given because
# because our outcome variable is a class variable, so want classification model.

pre <- predict(fit,newdata = testing1,type = "class")
# above command is used to predict the model on testing data set

# Now to see how much we are right use the following command-
tr <- table(pre,testing1$mutation)
cm <- confusionMatrix(pre,testing1$mutation)
m <- as.numeric(testing1$mutation)
n <- as.numeric(pre)
pre21 <- multiclass.roc(m,n)
rs1 <- pre21[['rocs']]
plot.roc(rs1[[1]],col = "red",print.auc = TRUE)
plot.roc(rs1[[2]],col = "red",print.auc = TRUE)
plot.roc(rs1[[3]],col = "red",print.auc = TRUE)
plot.roc(rs1[[4]],col = "red",print.auc = TRUE)
plot.roc(rs1[[5]],col = "red",print.auc = TRUE)
plot.roc(rs1[[6]],col = "red",print.auc = TRUE,main = "ROC curve for decision tree")
plot.roc(rs1[[7]],col = "red",print.auc = TRUE)
plot.roc(rs1[[8]],col = "red",print.auc = TRUE)
plot.roc(rs1[[9]],col = "red",print.auc = TRUE)

# Now let us try to make SVM model
fit3 <- svm(mutation~.,data = training1,type = "C-classification",kernel = "linear")
pre3 <- predict(fit3,newdata = testing1)
tr3 <- table(pre3,testing1$mutation)
cm3 <- confusionMatrix(pre3,testing1$mutation)

m <- as.numeric(testing1$mutation)
n <- as.numeric(pre3)
pre22 <- multiclass.roc(m,n)
rs2 <- pre22[['rocs']]
plot.roc(rs2[[1]],col = "red",print.auc = TRUE)
plot.roc(rs2[[2]],col = "red",print.auc = TRUE)
plot.roc(rs2[[3]],col = "red",print.auc = TRUE)
plot.roc(rs2[[4]],col = "red",print.auc = TRUE)
plot.roc(rs2[[5]],col = "red",print.auc = TRUE)
plot.roc(rs2[[6]],col = "red",print.auc = TRUE,main = "ROC curve for SVM linear")
plot.roc(rs2[[7]],col = "red",print.auc = TRUE)
plot.roc(rs2[[8]],col = "red",print.auc = TRUE)
plot.roc(rs2[[9]],col = "red",print.auc = TRUE)


fit1 <- svm(mutation~.,data = training1,type = "C-classification",kernel = "radial")
pre1 <- predict(fit1,newdata = testing1)
tr1 <- table(pre1,testing1$mutation)
cm1 <- confusionMatrix(pre1,testing1$mutation)

m <- as.numeric(testing1$mutation)
n <- as.numeric(pre1)
pre23 <- multiclass.roc(m,n)
rs3 <- pre23[['rocs']]
plot.roc(rs3[[1]],col = "red",print.auc = TRUE)
plot.roc(rs3[[2]],col = "red",print.auc = TRUE)
plot.roc(rs3[[3]],col = "red",print.auc = TRUE)
plot.roc(rs3[[4]],col = "red",print.auc = TRUE)
plot.roc(rs3[[5]],col = "red",print.auc = TRUE)
plot.roc(rs3[[6]],col = "red",print.auc = TRUE,main = "ROC curve for SVM radial")
plot.roc(rs3[[7]],col = "red",print.auc = TRUE)
plot.roc(rs3[[8]],col = "red",print.auc = TRUE)
plot.roc(rs3[[9]],col = "red",print.auc = TRUE)


fit2 <- randomForest(mutation~.,data = training1,ntree = 1000)
# In above command we are trying to build random forest model, where we are telling to produce 2000 trees.

#to show which words were considered important by random forest use - 
varImpPlot(fit2)

pre2 <- predict(fit2,newdata = testing1)
tr2 <- table(pre2,testing1$mutation)
cm2 <- confusionMatrix(pre2,testing1$mutation)

m <- as.numeric(testing1$mutation)
n <- as.numeric(pre2)
pre24 <- multiclass.roc(m,n)
rs4 <- pre24[['rocs']]
plot.roc(rs4[[1]],col = "red",print.auc = TRUE)
plot.roc(rs4[[2]],col = "red",print.auc = TRUE)
plot.roc(rs4[[3]],col = "red",print.auc = TRUE)
plot.roc(rs4[[4]],col = "red",print.auc = TRUE)
plot.roc(rs4[[5]],col = "red",print.auc = TRUE)
plot.roc(rs4[[6]],col = "red",print.auc = TRUE,main = "ROC curve for Random Forest")
plot.roc(rs4[[7]],col = "red",print.auc = TRUE)
plot.roc(rs4[[8]],col = "red",print.auc = TRUE)
plot.roc(rs4[[9]],col = "red",print.auc = TRUE)

fit4 <- naiveBayes(x = training1[,-z],y = training1$mutation)
pre4 <- predict(fit4,newdata = testing1[,-z])
tr4 <- table(testing1$mutation,pre4)

num_classes <- 9
# XGBoost only accepts factors that are sequentially numbered and the numbering 
# starts from 0. And in our case factors started from value 1. So it would give 
# error if not converted.
y <- as.numeric(training1$mutation) - 1
# above command is used to convert our outcome variable value start from 0 
fitnew <- xgboost(data = as.matrix(training1[,-z]),label = y,nrounds = 10,objective = "multi:softprob",num_class = num_classes)
# xgboost stands for eXtreme Gradient Boosting
# in above function we have converted the data frame to matrix as xgboost works on matrix
# nrounds tell the maximum number of iterations, objective = "multi:softprob" suggest 
# that we have used multipleclasses for prediction and num_class suggest how many classes
# are there in the model

y_pred3 <- predict(fitnew,newdata = as.matrix(testing1[,-z]))

prednew <- matrix(y_pred3, ncol=num_classes, byrow=TRUE)
# prednew contain the predicted values of each class.
pred_labels <- max.col(prednew)
# we are predicting to the class which has maximum probability value
trnew <- table(testing1$mutation,pred_labels)
cmnew <- confusionMatrix(testing1$mutation,pred_labels)
# Now let us make ROC curves for all classes.
pre20 <- multiclass.roc(testing1$mutation,pred_labels)
rs <- pre20[['rocs']]
plot.roc(rs[[1]],col = "red",print.auc = TRUE)
plot.roc(rs[[2]],col = "red",print.auc = TRUE)
plot.roc(rs[[3]],col = "red",print.auc = TRUE)
plot.roc(rs[[4]],col = "red",print.auc = TRUE)
plot.roc(rs[[5]],col = "red",print.auc = TRUE)
plot.roc(rs[[6]],col = "red",print.auc = TRUE,main = "ROC curve for XGBoost")
plot.roc(rs[[7]],col = "red",print.auc = TRUE)
plot.roc(rs[[8]],col = "red",print.auc = TRUE)
plot.roc(rs[[9]],col = "red",print.auc = TRUE)


# Now let us apply ANN.

library("h2o", lib.loc="~/R/win-library/3.4")
# h2o is an open source, it connects instance of the system thus working very fast.
# h2o package connects us to a powerful server thus working very fast.
h2o.init(nthreads = -1)
# By specifying nthreads = -1, we are telling to take all the cores of CPU.
# Before applying ANN it is very important to normalise the data.
# By normalising we make all the columns to have mean = 0 and standard deviation as 1. So below function
# just normalise the training and testing data.
training1[,-z] <- scale(training1[,-z])
testing1[,-z] <- scale(testing1[,-z])
x <- (ncol(training1)) / 2
x <- as.integer(x)

classifier <- h2o.deeplearning(y = "mutation",training_frame = as.h2o(training1),activation = "Rectifier",hidden = c(x,x),epochs = 100,train_samples_per_iteration = -2)
# in above command we have converted the data frame to h2o object as h2o.deeplearning() works on h2o object
# The activation function used in building the model is Rectifier.
# There are 2 hidden layers, each consisting of x neurons. Now number of neurons is determined by taking
# average of input and output variables. 
# epochs mean how many time you want to repeat the process.
# train_samples_per_iteration tell us which type of gradient descent you want to use i.e normal or stochastic.

# to see details of your model use
summary(classifier)

prob_pred <- h2o.predict(classifier,newdata = as.h2o(testing1[,-z]))
# above prob_pred consist of probability values of each class.

prob_pred$predict
# prob_pred$predict consist of name of class which has highest probability value.

y_pred1 <- as.vector(prob_pred$predict)
# storing predicted class in y.
cm <- table(testing1$mutation,y_pred1)

m <- as.numeric(testing1$mutation)
n <- as.numeric(y_pred1)
pre25 <- multiclass.roc(m,n)
rs5 <- pre25[['rocs']]
plot.roc(rs5[[1]],col = "red",print.auc = TRUE)
plot.roc(rs5[[2]],col = "red",print.auc = TRUE)
plot.roc(rs5[[3]],col = "red",print.auc = TRUE)
plot.roc(rs5[[4]],col = "red",print.auc = TRUE)
plot.roc(rs5[[5]],col = "red",print.auc = TRUE,main = "ROC curve for ANN")
plot.roc(rs5[[6]],col = "red",print.auc = TRUE)
plot.roc(rs5[[7]],col = "red",print.auc = TRUE)
plot.roc(rs5[[8]],col = "red",print.auc = TRUE)
plot.roc(rs5[[9]],col = "red",print.auc = TRUE)

# Let us make best ROC'S for all models in one panel
par(mfrow=c(3,3))
plot.roc(rs1[[6]],col = "red",print.auc = TRUE,main = "ROC curve for decision tree")
plot.roc(rs2[[6]],col = "red",print.auc = TRUE,main = "ROC curve for SVM linear")
plot.roc(rs3[[6]],col = "red",print.auc = TRUE,main = "ROC curve for SVM radial")
plot.roc(rs4[[6]],col = "red",print.auc = TRUE,main = "ROC curve for Random Forest")
plot.roc(rs[[6]],col = "red",print.auc = TRUE,main = "ROC curve for XGBoost")
plot.roc(rs5[[5]],col = "red",print.auc = TRUE,main = "ROC curve for ANN")


# let us try to reduce sparcity more and make the machine learning models

orig3 <- orig
orig3$variation <- train_var$Variation
str(orig3$variation)

dtm2 <- DocumentTermMatrix(corp)

# First let us take only 90% of the words.
dtm2 <- removeSparseTerms(dtm2,0.9)

orig4 <- as.data.frame(as.matrix(dtm2))

orig4$mutation <- train_txt$Class

orig4$mutation <- as.factor(orig4$mutation)

x <- colnames(orig4)
for (i in 1:length(x)) {
  if(grepl("^[a-zA-Z0-9 ]*$", x[i])==FALSE){ 
    y <- which(colnames(orig4) == x[i])
    y <- as.numeric(y)
    
    orig4 <- orig4[,-y]}
}

colnames(orig4)[which(names(orig4) == "break")] <- "altbreak"
colnames(orig4)[which(names(orig4) == "function")] <- "altfunction"
colnames(orig4)[which(names(orig4) == "next")] <- "altnext"
colnames(orig4)[which(names(orig4) == "repeat")] <- "altrepeat"
z4 <- which(colnames(orig4) == "mutation")

set.seed(320)
intrain3 <- createDataPartition(y = orig4$mutation,p = 0.7,list = FALSE)
training3 <- orig4[intrain3,]
testing3 <- orig4[-intrain3,]

# applying XGBoost model.
num_classes <- 9
ynew <- as.numeric(training3$mutation) - 1
fitnew1 <- xgboost(data = as.matrix(training3[,-z4]),label = ynew,nrounds = 10,objective = "multi:softprob",num_class = num_classes)
y_pred4 <- predict(fitnew1,newdata = as.matrix(testing3[,-z4]))

prednew1 <- matrix(y_pred4, ncol=num_classes, byrow=TRUE)
pred_labels1 <- max.col(prednew1)
trnew1 <- table(testing3$mutation,pred_labels1)
# You will observe that accuracy didn't changed much by reducing the sparsity.

# Let us reduce the sparsity further.
dtm3 <- DocumentTermMatrix(corp)

dtm3 <- removeSparseTerms(dtm3,0.85)

orig5 <- as.data.frame(as.matrix(dtm3))

orig5$mutation <- train_txt$Class

orig5$mutation <- as.factor(orig5$mutation)

x <- colnames(orig5)
for (i in 1:length(x)) {
  if(grepl("^[a-zA-Z0-9 ]*$", x[i])==FALSE){ 
    y <- which(colnames(orig5) == x[i])
    y <- as.numeric(y)
    orig5 <- orig5[,-y]}
}

colnames(orig5)[which(names(orig5) == "break")] <- "altbreak"
colnames(orig5)[which(names(orig5) == "function")] <- "altfunction"
colnames(orig5)[which(names(orig5) == "next")] <- "altnext"
colnames(orig5)[which(names(orig5) == "repeat")] <- "altrepeat"
z5 <- which(names(orig5) == "mutation")
set.seed(320)
intrain4 <- createDataPartition(y = orig5$mutation,p = 0.7,list = FALSE)
training4 <- orig5[intrain4,]
testing4 <- orig5[-intrain4,]

# applying xgboost model.
num_classes <- 9
ynew1 <- as.numeric(training4$mutation) - 1
fitnew2 <- xgboost(data = as.matrix(training4[,-z5]),label = ynew1,nrounds = 10,objective = "multi:softprob",num_class = num_classes)
y_pred5 <- predict(fitnew2,newdata = as.matrix(testing4[,-z5]))
prednew2 <- matrix(y_pred5, ncol=num_classes, byrow=TRUE)
pred_labels2 <- max.col(prednew2)
trnew2 <- table(testing4$mutation,pred_labels2)
# you will observe that accuracy reduced by 2%.


# Now let us apply some more pre-processing techniques on the data set.
# The dimensional reduction technique that we are going to use is PCA(Principle Component Analysis)

preproc <- preProcess(training1[,-z],method = "pca",pcaComp = 2)
trainpc <- predict(preproc,training1[,-z])
trainpc$mutation <- training1$mutation
testpc <- predict(preproc,testing1[,-z])
testpc$mutation <- testing1$mutation

fit5 <- rpart(mutation~.,data = trainpc,method = "class")
pre5 <- predict(fit5,newdata = testpc,type = "class")
tr5 <- table(pre,testpc$mutation)

fit6 <- randomForest(mutation~.,data = trainpc,ntree = 2000)
pre6 <- predict(fit6,newdata = testpc)
tr6 <- table(pre6,testpc$mutation)

fit7 <- svm(mutation~.,data = trainpc,type = "C-classification",kernel = "radial")
pre7 <- predict(fit7,newdata = testpc)
tr7 <- table(pre7,testpc$mutation)

fit8 <- naiveBayes(x = trainpc[,-3],y = trainpc$mutation)
pre8 <- predict(fit8,newdata = testpc[,-3])
tr8 <- table(testpc$mutation,pre8)

h2o.init(nthreads = -1)
classifier2 <- h2o.deeplearning(y = "mutation",training_frame = as.h2o(trainpc),activation = "Rectifier",hidden = c(2,2),epochs = 100,train_samples_per_iteration = -2)
prob_pred2 <- h2o.predict(classifier2,newdata = as.h2o(testpc[,-3]))
prob_pred2$predict
y_pred2 <- as.vector(prob_pred2$predict)
cm2 <- table(testpc$mutation,y_pred2)

# Now let us try to predict gene with the help of machine learning algorithms.

orig2 <- orig
orig2$classgene <- train_var$Gene

intrain2 <- createDataPartition(y = orig2$classgene,p = 0.7,list = FALSE)
training2 <- orig2[intrain2,]
testing2 <- orig2[-intrain2,]

fit9 <- rpart(classgene~.,data = training2,method = "class")
pre9 <- predict(fit9,newdata = testing2,type = "class")
count <- 0
for (i in 1:length(pre9)) {
  if(pre9[i] == testing2[i,3458]){
    count <- count + 1
  }
}
accuracy9 <- (count / 869) * 100

fit10 <- randomForest(classgene~.,data = training2,ntree = 2000)
pre10 <- predict(fit10,newdata = testing2)
count <- 0
for (i in 1:length(pre10)) {
  if(pre10[i] == testing2[i,3458]){
    count <- count + 1
  }
}
accuracy10 <- (count / 869) * 100


# But since genes are of 264 types in our data set. And in real life there are even more.
# So it is impossible to predict genes by using any machine learning algorithm.
str(train_var$Gene)

# So let us apply any other technique to predict gene. By studying the text properly we came to know 
# that gene is present in the text data before "mutations" word.
# So let us extract gene from the text data using regular expression.

gene <- vector()
for(i in 1:3321)
{
  x <- train_txt[i,2]
  pattern="([A-Z0-9]{3,10}[ ][m|M][u][t][a][t][i][o][n]([s]?))"
  TempGene=str_extract(x,pattern)
  gene[i] = unlist(strsplit(TempGene, split=' ', fixed=TRUE))[1]
  if(is.na(gene[i])|gene[i]!=train_var[i,2])
  {
    pattern="[A-Z0-9]{3,10}[ ][v|V][a][r][i][a][n][t]([s]?)"
    TempGene=str_extract(x,pattern)
    gene[i] = unlist(strsplit(TempGene, split=' ', fixed=TRUE))[1]
    if(is.na(gene[i])|gene[i]!=train_var[i,2])
    {
      pattern="[A-Z0-9]{3,10}[ ][g|G][e][n][e]([s]?)"
      TempGene=str_extract(x,pattern)
      gene[i] = unlist(strsplit(TempGene, split=' ', fixed=TRUE))[1]
    }
  }
}

count = 0;
for(i in 1:3321)
{
  if(!(is.na(gene[i])) & (train_var[i,2] == gene[i]))
  {
    count=count+1
  }
  
}
print(count)
acc <- (count/3321) * 100


