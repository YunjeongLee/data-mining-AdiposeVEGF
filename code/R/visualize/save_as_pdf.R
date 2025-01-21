save_as_pdf <- function(filename, width=width_inches) {
  # This file convert png file to pdf file
  # This function needs "magick" library
  # Read the image
  img <- image_read(paste0(filename, ".png"))
  
  # Calculate the width in pixels (assuming 300 DPI)
  width_pixels <- width * 300
  
  # Resize the image
  img_resized <- image_scale(img, paste0(width_pixels, "x"))
  image_write(img_resized, paste0(filename, ".pdf"), format="pdf", density=300)
}