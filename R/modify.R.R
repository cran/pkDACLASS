modify.R <-
function(mat,n){
colnames(mat)<-c("m.z","y","level")
dd<-mat[order(mat$m.z),]
m.align<-matrix(NA, nrow = nrow(dd), max(dd$level))
idx<-cbind(1:nrow(m.align), dd$level)
m.align[idx]<-dd$y
data1<-as.data.frame(cbind(dd$m.z,m.align))
names.align<-paste("sample",1:max(dd$level),sep="")
colnames(data1)<-c("m.z",names.align)
foo<-function(x){
          y <- sum(x, na.rm = TRUE)
          ifelse(y==0, NA, y)
          }
ff.data<- aggregate(data1[,-1], list(data1$m.z), function(x) ifelse(
                                            all(is.na(x)),
                                                 NA,
                                                 na.exclude(x)[1]))
ff.data[is.na(ff.data)]<- 0
names.align.t<-paste("sample", 1:(max(dd$level)*0.5), sep="")
names.align.c<-paste("control", 1:(max(dd$level)*0.5), sep="")
colnames(ff.data) <- c("m.z",names.align.t,names.align.c)
ff.red <- function(x, n) (sum(x[grepl("^sample", names(x))] > 0) >= n) &
                     (sum(x[grepl("^control", names(x))] > 0) >= n)
f.data <- ff.data[apply(ff.data, 1, ff.red, n), ]
return(f.data)
}

