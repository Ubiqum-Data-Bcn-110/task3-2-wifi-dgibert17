p1 <- data.frame(1,2,2)
p1[2,] <- c(3,2,2)

p2 <- data.frame(5,3,1)
p2[2,] <- c(5,3,2)

p <- plot_ly(p1, x = ~X1, y = ~X2, z = ~X2.1, mode = "line")
p

pp <- plot_ly(p2, x = ~X5, y = ~X3, z = ~X1, mode = "line")


p3 <- data.frame(4,4,4)
p3[2,] <- c(4,3,2)

assign("ppp", plot_ly(p3, x = ~X4, y = ~X4.1, z = ~X4.2, mode = "line"))


pppp <- subplot(p, pp, ppp )

merge_points <- function(df){
  
  for ( i in 1:nrow(df) ){
    
    assign(paste("plot_real",i, sep=""), 
           data.frame(x = vector(df.nuevo.real.pred[1,c(1,4)]),
                      y = vector(df.nuevo.real.pred[1,c(2,5)]),
                      z = vector(df.nuevo.real.pred[1,c(3,6)])) 
          )
    
  }
  
  
  rm(plot_real312)
  
  
}

merge_points (df.nuevo.real.pred[1:10,])
