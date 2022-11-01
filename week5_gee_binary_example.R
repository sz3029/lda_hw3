library(data.table)
library(gee)
library(geepack)
library(doBy)

obesity <- fread("~/Homer/P8157-2020/Data Sets/muscatine.dat")
colnames(obesity) <- c("id","gender","cohort","current_age","occasion","status")
obesity$gender <- as.factor(obesity$gender)
obesity$id <- as.factor(obesity$id)


# GEE Model 3: 
# - without cohort effects
# - quadratic trend with age 
# - no interaction with gender 

# Compare model based to sandwich 
gee3 <- gee(status ~ gender + (current_age + I(current_age^2)), id = id, data = obesity, family = binomial(link = "logit"), corstr = "unstructured")
summary(gee3)$coeff


gee5 <- geeglm(status ~ gender + (current_age + I(current_age^2)), id = id, data = obesity, family = binomial(link = "logit"), corstr = "unstructured")
summary(gee5)$coeff

# plotting to see quadratic effects 
age    <- unique(obesity$current_age)
gender <- unique(obesity$gender)
new_data <- expand.grid(list(current_age=age,gender=gender))
pred <- predict(gee5,newdata = new_data,type = "response")
new_data$prob <- pred
mat <- do.call(cbind,split(new_data,new_data$gender))
mat <- mat[,c(1,3,6)]
colnames(mat) <- c("age","prob_male","prob_female")
pdf(file = "quadratic_compare.pdf", height = 5, width = 8)
matplot(mat$age,mat[,-1], type="l", xlab = "age", ylab="Probability")
temp <- legend("topleft", legend = c(" ", " "),
               text.width = strwidth("1,000,000"),
               lty = 1:2, col=1:2, xjust = 1, yjust = 1,
               title = "Gender")
text(temp$rect$left + temp$rect$w, temp$text$y,
     c("Male", "Female"), pos = 2)
dev.off()




# GEE Model 2: 
# - without cohort effects
# - quadratic trend with age 
# - interaction with gender 
gee2 <- geeglm(status ~ gender * (current_age + I(current_age^2)), id = id, data = obesity, family = binomial(link = "logit"), corstr = "unstructured")
summary(gee2)
L <- matrix(0,ncol=6,nrow=2) # ncol = number of coeffcients in the model, nrow = number of tests 
L[1,c(5)]  <- c(1)
L[2,c(6)]  <- c(1)
L
esticon(gee2,L=L,joint.test = TRUE)
