
#### codigo para extrair paleta de cores de imagens:


######################## NOT WORKING
# library(devtools)
# install_github("andreacirilloac/paletter")
# 
# install.packages("palleter")
# 
# library(paletter)
# download.file("https://upload.wikimedia.org/wikipedia/commons/7/7d/A_Sunday_on_La_Grande_Jatte%2C_Georges_Seurat%2C_1884.jpg",
#               "image.jpg")
# colours_vector <- create_palette("/home/allan/Documents/image.jpg", number_of_colors=5)
# ??paletter
# 
# create_palette("data/nascita_venere.jpg",number_of_colors = 20)
#########################

install.packages("RImagePalette")
library(RImagePalette)

??RImagePalette


download.file("https://upload.wikimedia.org/wikipedia/commons/thumb/e/ea/Van_Gogh_-_Starry_Night_-_Google_Art_Project.jpg/1200px-Van_Gogh_-_Starry_Night_-_Google_Art_Project.jpg",
              "image.jpg")

image_matrix <- readJPEG("/home/allan/Documents/image.jpg")
colours_vector <- image_palette(image_matrix, n=4)
