library(gatepoints)

kk2 <- data.frame(k2=1:10, k1=1:10)
plot(kk2, col = "red", pch = 16)
selectedPoints <- fhs(kk2)

?fhs

x <- cbind(1:10, 1:10)
rownames(x) <- 1:10
plot(x, pch = 16, col = "red")
fhs(x)


plot(runif(100)) 
legends_coord <- locator(1) 
print(legends_coord) 
legend(x= legends_coord[1], y= legends_coord[2], legend= "First Legend") 
plot(runif(100)) 
legends_coord <- locator(3) 
print(legends_coord) 
legend(x= legends_coord$x[1], y= legends_coord$y[1], legend= "some text", bty="n") 
legend(x= legends_coord$x[2], y= legends_coord$y[2], legend= "more text", bty="n") 
legend(x= legends_coord$x[3], y= legends_coord$y[3], legend= "even more text", bty="n")