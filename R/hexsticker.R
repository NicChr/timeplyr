# library(hexSticker)
# library(magick)
# library(sysfonts)
# library(tidyverse)
#
# time_image <- image_read("man/figures/time_seq.png")
# # image_transparent(time_image, "white")
# time_image <- image_modulate(time_image)
# time_image <- image_quantize(time_image)
# # time_image <- image_contrast(time_image, sharpen = 2)
# # time_image <- image_contrast(time_image)
# time_image <- image_background(time_image,
#                                color = "#002366",
#                                flatten = FALSE)
# image_write(time_image, path = "man/figures/time_seq_clean.png", format = "png")
# time_image
# # Sticker -----------------------------------------------------------------
#
# fonts_df <- font_files()
# # fonts_df$file
#
# font_add("Times New Roman", "times.ttf")
#
# p_image <- sticker(
#   subplot = time_image,
#   package = "timeplyr",
#   s_width = 1.2,
#   s_height = 1.1,
#   s_x = 1,
#   s_y = 0.8,
#   h_fill = "#002366",
#   h_color = "black",
#   url = "NicChr/timeplyr",
#   u_size = 4,
#   u_color = "white",
#   u_x = 1,
#   u_y = 0.075,
#   h_size = 1.75,
#   spotlight = TRUE,
#   l_y = 1,
#   l_x = 1,
#   l_alpha = 0,
#   p_color = "white",
#   p_family = "Times New Roman",
#   p_size = 25
# )
# print(p_image)
#
# time_image2 <- image_read("man/figures/timeplyr.png")
# time_image2 <- image_modulate(time_image2)
# time_image2 <- image_quantize(time_image2)
# time_image2 <- image_contrast(time_image2, sharpen = 3)
# image_write(time_image2, path = "man/figures/timeplyr.png", format = "png")
