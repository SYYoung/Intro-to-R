R_from_book_interface <- function(){
    con <- url("http://www.jhsph.edu","r")
    head(x)
    
    # multiple elements
    x <- list(a=list(10,12,14),b=c(3.14,2.81))
    x[[c(2,1)]]
    
    x <- list(foo=1:4, bar=0.6, baz="hello")
    x[c(1,3)]
    
    # removing NA
    x <- c(1,2,NA,4,NA,5)
    x[!is.na(x)]
    
    x <- c(1,2,NA,4,NA,5)
    y<-c("a","b",NA,"d",NA,"f")
    good <- complete.cases(x,y)
    x[good]
    y[good]
    
}

from_book_vectorized <- function() {
    
}

from_book_dates <- function() {
    x <- as.Date("1970-01-01")
    
    x <- Sys.time()
    p <- as.POSIXlt(x)
    names(unclass(p))
    p$wday
    
    datestring <- c("January 10, 2012 10:40", "December 9, 2011 9:10")
    x <- strptime(datestring, "%B %d, %Y %H:%M")
    
    x <- as.Date("2012-01-01")
    y <- strptime("9 Jan 2011 11:34:21", "%d %b %Y %H:%M:%S")
    x <- as.POSITlt(x)
    x - y
    
}

from_book_dplyr <- function() {
    # functions is dplyr package: select, filter, arrange, rename, mutate, 
    # summarise, %>%
    install.packages("dplyr")
    library(dplyr)
    
    # select: for extraction of columns
    chicago <- readRDS("chicago.rds")
    subset <- select(chicago, -(city:dptp))
    i <- match("city", names(chicago))
    j <- match("dptp", names(chicago))
    head(chicago[,-(i:j)])
    subset <- select(chicago, ends_with("2"))
    
    # filter: for extraction of rows
    chic.f <- filter(chicago, pm25tmean2 > 30)
    chic.f <- filter(chicago, pm25tmean2 > 30  & tmpd > 80)
    select(chic.f, data, tmpd, pm25tmean2)
    
    # arrange: reorder rows
    chicago <- arrange(chicago, date)
    
    # raname:
    chicago <- rename(chicago, dewpoint=dptp, pm25=pm25tmean2)
    chicago <- mutate(chicago, pm25detrend=pm25 - mean(pm25, na.rm=TRUE))
    
    # group_by: generate summary statistics from the data frame within
    # strata defined by a variable
    chicago <- mutate(chicago, year=as.POSIXlt(date)$year+1900)
    years <- group_by(chicago, year)
    summarize(years, pm25=mean(pm25,na.rm=TRUE),
              o3=max(o3tmean2,na.rm=TRUE),
              no2=median(no2tmean2,nam.rm=TRUE))
    
    # by quantile
    qq <- quantile(chicago$pm25, seq(0,1,0.2), na.rm=TRUE)
    chicago <- mutate(chicago, pm25.quint=cut(pm25,qq))
    quint <- group_by(chicago, pm25.quint)
    summarize(quint, o3=mean(o3mean2,na.rm=TRUE),
              no2=mean(no2tmean2,na.rm=TRUE))
    
    # %>%
    mutate(chicago, month=as.POSIXlt(data)$mon+1) %>%
        group_by(month) %>%
        summarize(pm25=mean(pm25,na.rm=TRUE),
                  o3=max(o3tmean2,na.rm=TRUE),
                  no2=median(no2tmean2,na.rm=TRUE))
    
}

from_book_control <- function(){
    
}

function_book_functions <- function() {
    myplot <- function(x,y,type="l",...){
        plot(x,y,type=type,...) ## pass '...' to plot function
    }
    
    make.power <- function(n) {
        pow <- function(x) {
            x^n
        }
        pow
    }
    cube <- make.power(3)
    square <- make.power(2)
    cube(3)
    square(3)
    ls(environment(cube))
}