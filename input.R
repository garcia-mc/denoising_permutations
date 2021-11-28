setwd("~/denoising_permutations")
library(image.CornerDetectionF9)
library(magick)

x <- image_read("kolmogorov.png")

x
image <- image_data(x, channels = "Gray")
image <- as.integer(image, transpose = TRUE)
image <- drop(image)
corners <- image_detect_corners(image, threshold = 25)
plt <- image_draw(x)
points(corners$x, image_info(x)$height - corners$y, col = "red", pch = 20, lwd = 0.5)
dev.off()
plt

covariates=data.frame(xcoord=corners$x,ycoord=(image_info(x)$height - corners$y))
n=nrow(covariates)

library(imager)

kol=load.image("kolmogorov.png")

map.rotation <- function(x,y){
  angle=pi/4
  return (list(x=cos(angle)*x -sin(angle)*y,y=sin(angle)*x+ cos(angle)*y))

}

orig_response=as.data.frame(map.rotation(covariates$xcoord,covariates$ycoord))

unknown_perm=sample(1:n,n)
response=orig_response[unknown_perm,]

plot(covariates,xlim=c(-2000,2000),ylim=c(0,2000))

#######

plot(response,col='blue',xlim=c(-2000,2000),ylim=c(0,2000))

UY=svd(response, nu = 2, nv = 2)[[2]]
UYorig=svd(orig_response, nu = 2, nv = 2)[[2]]

UA=svd(as.matrix(covariates), nu = 2, nv = 2)[[2]]

lY=apply(UY,1,function (x) sum(x^2))
lYorig=apply(UYorig,1,function (x) sum(x^2))

lA=apply(UA,1,function (x) sum(x^2))

plot(lY,lA)



ind_Y=order(lY)
ind_A=order(lA)

inverse=order(ind_Y)

total=ind_A[inverse]

plot(lY,lA[total])

total
unknown_perm















kol %>% plot
  
imrotate(kol,60) %>% plot(main="Rotating")
points(map.rotation60(corners$x,image_info(x)$height - corners$y), col = "red", pch = 20, lwd = 0.5)




