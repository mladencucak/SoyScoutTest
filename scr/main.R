# Planning ----------------------------------------------------------------

## Flow:
# Connect to the Google account 
# Build a guide
# User authentication for scouts project
# Publish using Rstudio connect? Or save the guide in Google folder 

## To do:
# Address redundant names like multiple files objects, etc. in case of downstream 
# failure.

## Done
# Update file locations (6/27/22)


# Packages  ---------------------------------------------------------------
# install.packages("reshape2") 

library("here")  
library("googledrive")  
library("googlesheets4")  
library("ggplot2")  
library("dplyr")  
library("tidyr")  
library("stringr") 
library("magick")  



# Resources and custom functions ---------------------------------------------------------------

## RESOURCES
## Guide to Publishing to RStudio: Connect w Google creds
# browseURL("https://babichmorrowc.github.io/post/google-account-creds/")
## Guide to using googlesheets to connect R to Google Sheets
# browseURL("https://datascienceplus.com/how-to-use-googlesheets-to-connect-r-to-google-sheets/")

## CUSTOM FUNCTIONS
## For formatting
source(here("scr/fun/img.R"))

# Authorize access to Google Drive ----------------------------------------

## This works to give me access
options(gargle_oauth_email  = "cropriskstatus@gmail.com")
drive_deauth()
drive_auth(path = ".secrets/client_secret.json")

# googlesheets4
googlesheets4::gs4_auth(email = "cropriskstatus@gmail.com",
                        cache = ".secrets",
                        use_oob = TRUE)
# googledrive
googledrive::drive_auth(email = "cropriskstatus@gmail.com", # Replace with your email! MD - MY EMAIL DOESN'T WORK
                        cache = ".secrets",
                        use_oob = TRUE)


# DISEASES #################################################################### 


# Set path to folder and retrieve contents (as a dribble)
# Path is set to disease folder
# Should I rename jp_folder in case of downstream issues? We don't want to accidentally
# direct the pest section to the disease folder if something fails to be created later. 
jp_folder = "https://drive.google.com/drive/folders/1dvyulDFc9Fm2ALgIfMJOHBq6fOi_91Mr"
folder_id_dis = drive_get(as_id(jp_folder))

# Create list of files in the folder
# Again, specific item name?
files = drive_ls(folder_id_dis)

# Access contents of image citations file as tibble object disimg
disimg <- 
  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1M2TiKPGn2FpxZTvzx4XJ97OpAYwW-45ZtIvD3jEROIM/edit#gid=145702069", 
                            sheet = "Disease" )

# Remove any files with terms 'images' or 'thumbnails' from the list of files found
# in disease folder
# Again, redundant names
files <- files[files$name != "images",]
files <- files[files$name != "thumbnails",]

# Remove files loaded from last session for complete re-loading of current files?
# It makes a list of files found in the disease folder with full names and loops
# through to make sure it's got everything, and then removes the files for a clean 
# upload later on.
sapply(list.files(here("/img/dis/"), full.names = TRUE, recursive = T), file.remove)


# Loop through all files in img/dis folder and download them, referencing files 
# object created above 
# For every file name in files object, add to list i_dir
for (i in seq_along(files$name)) {
  # for specific file, i = specific file, referenced by number in files object
  # i = 1
  # Create list of files
  i_dir = drive_ls(files[i, ])
  
  path <- here("img/dis",files$name[i])
  #mkdir
  if(!file.exists(path))  try(dir.create(path))
  i_dir <- 
    i_dir[i_dir$name !="thumbnail.jpg", ]
  
  #download files
  for (file_i in seq_along(i_dir$name)) {
    
    #fails if already exists
    try({
      drive_download(
        as_id(i_dir$id[file_i]),
        path = str_c(path, "/", i_dir$name[file_i])
      )
    })
    gc()
    # Sys.sleep(.1)  
  }
  
}

paths <- list.files(here("img/dis"),  all.files  = TRUE, recursive = T, full.names = T)

disimg <- 
  disimg %>% 
  mutate(pth = "img/dis") %>% 
  unite(pth, c(pth,folder, name), sep = "/")

# Get paths to the final image
# Get 
for (i in seq(27: nrow(disimg))) {
  # i = 1
  disimg[i , "img_pth"] <- paths[grepl(disimg[i , "pth"] %>% pull(), paths)]
  
  
  fin_pth <- paths[grepl(disimg[i , "pth"] %>% pull(), paths)]
  
  
  # # file.exists(fin_pth)
  # fin_dir <- paste0(strsplit(fin_pth, "/")[[1]][1: length(strsplit(fin_pth, "/")[[1]])-1], collapse ="/")
  # if(!file.exists(fin_dir))dir.create(fin_dir)
  
  img <- image_read(disimg[i , "img_pth"] %>% pull())
  
  image_info(img)$width
  
  border_width <- round((image_info(img)$width  )/30,0)
  # border_height <- round((image_info(img)$height  )/30,0)
  img <-
    image_border(img, "white", paste(border_width,border_width, sep = "x")) 
  
  heigh_offset <-  round((image_info(img)$height  )/120,0)
  caption_size <-  round((image_info(img)$width  )/50,0)
  
  img <-
    image_annotate(
      img,
      pull(disimg[i , "credits"]),
      location = paste0("+1+", heigh_offset),
      # location = paste0("+1", heigh_offset),
      size = caption_size,
      gravity = "south",
      color = "black"
    )
  
  img <- image_convert(img, "bmp")
  img <- image_scale(img, "")
  
  image_write(img, fin_pth)
  gc()
  Sys.sleep(.1)     
  print(fin_pth)
}




rm(files, folder_id_dis, i_dir, file_i, i, jp_folder, path, paths,caption_size)





# # PESTS -----------------------------------------------------------------

#folder link to id
jp_folder = "https://drive.google.com/drive/folders/13TzQECqe_fn7guClyCDvbMzeyneXdP63"
folder_id_pest = drive_get(as_id(jp_folder))


#find files in folder
(files = drive_ls(folder_id_pest))


url.to.spreadsheet.with.links.to.images <- 
  "https://docs.google.com/spreadsheets/d/1hsYCTnm7HqSOQdFrGc_S7UNMsIhmPuKWhpRdddsUkYk/edit#gid=515938431"
pestimg <- 
  googlesheets4::read_sheet(url.to.spreadsheet.with.links.to.images )

(files <- files[files$name != "images",])
(files <- files[files$name != "thumbnails",])

if(file.exists(here("img/pest"))==0)dir.create(here("img/pest"))

#Remove all files in final folder for re-loading 
# sapply(list.files(here("img/pest/"), full.names = TRUE, recursive = T), file.remove)

#loop dirs and download files inside them
for (i in seq_along(files$name)) {
  #list files
  i_dir = drive_ls(files[i, ])
  
  path <- here("img/pest",files$name[i])
  #mkdir
  if(!file.exists(path))  try(dir.create(path))
  i_dir <- 
    i_dir[i_dir$name !="thumbnail.jpg", ]
  
  #download files
  for (file_i in seq_along(i_dir$name)) {
    #fails if already exists
    try({
      drive_download(
        as_id(i_dir$id[file_i]),
        path = str_c(path, "/", i_dir$name[file_i])
      )
    })
  }
}

paths <- list.files(here("img/pest"),  all.files  = TRUE, recursive = T, full.names = T)

pestimg <- 
  pestimg %>% 
  mutate(pth = "img/pest") %>% 
  unite(pth, c(pth,folder, name), sep = "/")

# Get paths to the final image
# Get 
for (i in seq(1: nrow(pestimg))) {
  # i = 62
  pestimg[i , "img_pth"] <- paths[grepl(pestimg[i , "pth"] %>% pull(), paths)]
  
  fin_pth <- paths[grepl(pestimg[i , "pth"] %>% pull(), paths)]
  
  # # file.exists(fin_pth)
  # fin_dir <- paste0(strsplit(fin_pth, "/")[[1]][1: length(strsplit(fin_pth, "/")[[1]])-1], collapse ="/")
  # if(!file.exists(fin_dir))dir.create(fin_dir)
  
  img <- image_read(pestimg[i , "img_pth"] %>% pull())
  
  image_info(img)$width
  
  border_width <- round((image_info(img)$width  )/30,0)
  # border_height <- round((image_info(img)$height  )/30,0)
  img <-
    image_border(img, "white", paste(border_width,border_width, sep = "x")) 
  
  heigh_offset <-  round((image_info(img)$height  )/120,0)
  caption_size <-  round((image_info(img)$width  )/50,0)
  
  img <-
    image_annotate(
      img,
      pull(pestimg[i , "credits"]),
      location = paste0("+1+", heigh_offset),
      # location = paste0("+1", heigh_offset),
      size = caption_size,
      gravity = "south",
      color = "black"
    )
  image_write(img, fin_pth)
  gc()
  Sys.sleep(.1)     
  print(fin_pth)
}




rm(files, folder_id_pest, i_dir, file_i, i, jp_folder, path, paths,caption_size)





# # WEEDS -----------------------------------------------------------------



#folder link to id
weed.photo_folder = 
  "https://drive.google.com/drive/folders/1yDpRha96isITx7xwkxufqEsa6cob0aSB"
folder_id_weed = drive_get(as_id(weed.photo_folder))

#find files in folder
(files = drive_ls(folder_id_weed))


url.to.spreadsheet.with.links.to.images <- 
  "https://docs.google.com/spreadsheets/d/19eoqL4g9j_JtTwEz28ikY0Ou3UTGmOOvagbdOU0d3AE/edit#gid=906340208"
weedimg <- 
  googlesheets4::read_sheet(url.to.spreadsheet.with.links.to.images )

(files <- files[files$name != "images",])
(files <- files[files$name != "thumbnails",])

if(file.exists(here("img/weed"))==0)dir.create(here("img/weed"))

#Remove all files in final folder for re-loading 
# sapply(list.files(here("img/weed/"), full.names = TRUE, recursive = T), file.remove)


#loop dirs and download files inside them
for (i in seq_along(files$name)) {
  #list files
  i_dir = drive_ls(files[i, ])
  
  path <- here("img/weed",files$name[i])
  #mkdir
  if(!file.exists(path))  try(dir.create(path))
  
  i_dir <- 
    i_dir[i_dir$name !="thumbnail.jpg", ]
  
  #download files
  for (file_i in seq_along(i_dir$name)) {
    #fails if already exists
    try({
      drive_download(
        as_id(i_dir$id[file_i]),
        path = str_c(path, "/", i_dir$name[file_i])
      )
    })
  }
  gc()
  Sys.sleep(.1) 
}

paths <- list.files(here("img/weed"),  all.files  = TRUE, recursive = T, full.names = T)

weedimg <- 
  weedimg %>% 
  mutate(pth = "img/weed") %>% 
  unite(pth, c(pth,folder, name), sep = "/")

# Get paths to the final image

for (i in seq(1: nrow(weedimg))) {
  # i = 62
  weedimg[i , "img_pth"] <- paths[grepl(weedimg[i , "pth"] %>% pull(), paths)]
  
  fin_pth <- paths[grepl(weedimg[i , "pth"] %>% pull(), paths)]
  
  # # file.exists(fin_pth)
  # fin_dir <- paste0(strsplit(fin_pth, "/")[[1]][1: length(strsplit(fin_pth, "/")[[1]])-1], collapse ="/")
  # if(!file.exists(fin_dir))dir.create(fin_dir)
  
  img <- image_read(weedimg[i , "img_pth"] %>% pull())
  
  image_info(img)$width
  
  border_width <- round((image_info(img)$width  )/30,0)
  # border_height <- round((image_info(img)$height  )/30,0)
  img <-
    image_border(img, "white", paste(border_width,border_width, sep = "x")) 
  
  heigh_offset <-  round((image_info(img)$height  )/120,0)
  caption_size <-  round((image_info(img)$width  )/50,0)
  
  img <-
    image_annotate(
      img,
      pull(weedimg[i , "credits"]),
      location = paste0("+1+", heigh_offset),
      # location = paste0("+1", heigh_offset),
      size = caption_size,
      gravity = "south",
      color = "black"
    )
  image_write(img, fin_pth)
  gc()
  Sys.sleep(.1)     
  print(fin_pth)
}




rm(files, folder_id_weed, i_dir, file_i, i, jp_folder, path, paths,caption_size)



# ABIOTIC########################################################


#folder link to id
abio.photo_folder = 
  "https://drive.google.com/drive/folders/1upgHZ-Tvk6M1J0k1Hs3aUgnnr9SqQRnh"
folder_id_abio = drive_get(as_id(abio.photo_folder))

#find files in folder
(files = drive_ls(folder_id_abio))


url.to.spreadsheet.with.links.to.images <- 
  "https://docs.google.com/spreadsheets/d/1Mr35xqBpnbwZGhZr49LxmOmupArAwtzYTj8c6Ri7cKs/edit#gid=186519958"
abioimg <- 
  googlesheets4::read_sheet(url.to.spreadsheet.with.links.to.images )

(files <- files[files$name != "images",])
(files <- files[files$name != "thumbnails",])

if(file.exists(here("img/abio"))==0)dir.create(here("img/abio"))

#Remove all files in final folder for re-loading 
# do.call(file.remove, list(list.files(here("img/abio/"), full.names = TRUE,force=TRUE, recursive = T)))
sapply(list.files(here("img/abio/"), full.names = TRUE, recursive = T), file.remove)


#loop dirs and download files inside them
for (i in seq_along(files$name)) {
  #list files
  i_dir = drive_ls(files[i, ])
  
  path <- here("img/abio",files$name[i])
  #mkdir
  if(!file.exists(path))  try(dir.create(path))
  i_dir <- 
    i_dir[i_dir$name !="thumbnail.jpg", ]
  
  #download files
  for (file_i in seq_along(i_dir$name)) {
    #fails if already exists
    try({
      drive_download(
        as_id(i_dir$id[file_i]),
        path = str_c(path, "/", i_dir$name[file_i])
      )
    })
  }
}

paths <- list.files(here("img/abio"),  all.files  = TRUE, recursive = T, full.names = T)

abioimg <- 
  abioimg %>% 
  mutate(pth = "img/abio") %>% 
  unite(pth, c(pth,folder, name), sep = "/")

# Get paths to the final image
# Get 
for (i in seq(1: nrow(abioimg))) {
  # i = 1
  abioimg[i , "img_pth"] <- paths[grepl(abioimg[i , "pth"] %>% pull(), paths)]
  
  fin_pth <- paths[grepl(abioimg[i , "pth"] %>% pull(), paths)]
  
  # # file.exists(fin_pth)
  # fin_dir <- paste0(strsplit(fin_pth, "/")[[1]][1: length(strsplit(fin_pth, "/")[[1]])-1], collapse ="/")
  # if(!file.exists(fin_dir))dir.create(fin_dir)
  
  img <- image_read(abioimg[i , "img_pth"] %>% pull())
  
  image_info(img)$width
  
  border_width <- round((image_info(img)$width  )/30,0)
  # border_height <- round((image_info(img)$height  )/30,0)
  img <-
    image_border(img, "white", paste(border_width,border_width, sep = "x")) 
  
  heigh_offset <-  round((image_info(img)$height  )/120,0)
  caption_size <-  round((image_info(img)$width  )/50,0)
  
  img <-
    image_annotate(
      img,
      pull(abioimg[i , "credits"]),
      location = paste0("+1+", heigh_offset),
      # location = paste0("+1", heigh_offset),
      size = caption_size,
      gravity = "south",
      color = "black"
    )
  image_write(img, fin_pth)
  gc()
  Sys.sleep(.1)     
  print(fin_pth)
}




rm(files, folder_id_abio, i_dir, file_i, i, jp_folder, path, paths,caption_size)









