setwd("~/denoising_permutations")
library(image.CornerDetectionF9)
library(magick)

x <- image_read("kolmogorov.png")

x
image <- image_data(x, channels = "Gray")
image <- as.integer(image, transpose = TRUE)
image <- drop(image)
corners <- image_detect_corners(image, threshold = 30)
plt <- image_draw(x)
points(corners$x, image_info(x)$height - corners$y, col = "red", pch = 20, lwd = 0.5)
dev.off()
plt

library(imager)

kol=load.image("kolmogorov.png")

map.rotation60 <- function(x,y){
  angle=pi/3
  return (list(x=cos(angle)*x -sin(angle)*y+1458,y=sin(angle)*x+ cos(angle)*y))

}


kol %>% plot
  
imrotate(kol,60) %>% plot(main="Rotating")
points(map.rotation60(corners$x,image_info(x)$height - corners$y), col = "red", pch = 20, lwd = 0.5)




