library(data.table)
library(gee)
library(geepack)
library(doBy)

leprosy <- fread("~/Homer/P8157-2020/Data Sets/leprosy.txt", skip = 5)
colnames(leprosy) <- c("trt","pre","post")
leprosy$id <- 1:nrow(leprosy)
# wide to long 
leprosy <- melt(leprosy, id.vars = c("id","trt"), measure.vars = c("pre","post"), variable.name = "time", value.name = "count")
leprosy <- leprosy[order(id,time)]

# summarize data 
summary1 <- leprosy[,j=list(mean = mean(count), var = var(count)), by = c("trt","time")]
summary1

# make variables factor
leprosy$trt  <- factor(leprosy$trt,levels = c("C","A","B"))
leprosy$time <- factor(leprosy$time,levels = c("pre","post"))

# GEE Model 1: 
gee1 <- gee(count ~ trt * time, id = id, data = leprosy, family = poisson(link = "log"), corstr = "unstructured")
summary(gee1)$coeff

# GEE Model 2: 
gee2 <- geeglm(count ~ trt * time, id = id, data = leprosy, family = poisson(link = "log"), corstr = "exchangeable", std.err = "jack")
summary(gee2)$coeff

# Hypothesis test
L <- matrix(0,ncol=6, nrow = 3)
L[1,c(2,5)] <- 1
L[2,c(3,6)] <- 1
L[3,c(2,3,5,6)] <- c(1,-1,1,-1)
L
esticon(gee2,L,conf.int = TRUE)
