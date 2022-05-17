# Connect to the google account 
# Build a guide
# User authetification for scouts 
# publish using Rstudio connect? or save the guide in google folder 

library("here")

library("googledrive")
library("googlesheets4")
library("ggplot2")
library("dplyr")
library("tidyr")
library("stringr")
library("magick")

# browseURL("https://babichmorrowc.github.io/post/google-account-creds/")
# browseURL("https://datascienceplus.com/how-to-use-googlesheets-to-connect-r-to-google-sheets/")



# install.packages("reshape2")
# Authorize googlesheets4
googlesheets4::gs4_auth(email = "mladencucak@gmail.com",
                        cache = ".secrets",
                        use_oob = TRUE)
googledrive::drive_auth(email = "mladencucak@gmail.com", # Replace with your email!
                        cache = ".secrets",
                        use_oob = TRUE)


# get function for formating
source(here("scr/fun/img.R"))

##############################################
# Diseases
###############################################



#folder link to id
jp_folder = "https://drive.google.com/drive/u/0/folders/1MlWZjEHdXipBNEBURoMTYT1cBvQ7MPWK"
folder_id = drive_get(as_id(jp_folder))

#find files in folder
files = drive_ls(folder_id)


disimg <- 
  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1AnfIYUZqze3x3O1LlepmbAsMvFGQpvcQx3b523lUxe4/edit#gid=0", 
                            sheet = "Disease" )

files <- files[files$name != "images",]
files <- files[files$name != "thumbnails",]

#Remove all files in final folder for re-loading 
# sapply(list.files(here("img/dis/"), full.names = TRUE, recursive = T), file.remove)
 
#loop dirs and download files inside them
for (i in seq_along(files$name)) {
  # i = 1
  #list files
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




rm(files, folder_id, i_dir, file_i, i, jp_folder, path, paths,caption_size)



##############################################
# Pests
###############################################
#folder link to id
jp_folder = "https://drive.google.com/drive/u/0/folders/1cS3PleejCQPDomAglAtYzjfg2usn3nIb"
folder_id = drive_get(as_id(jp_folder))

#find files in folder
(files = drive_ls(folder_id))


url.to.spreadsheet.with.links.to.images <- 
  "https://docs.google.com/spreadsheets/d/1asffbRpcoz7LUrqdpkd6117wsADJGMo9rkuafei6eho/edit#gid=0"
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




rm(files, folder_id, i_dir, file_i, i, jp_folder, path, paths,caption_size)






# Weeds -------------------------------------------------------------------

#folder link to id
weed.photo_folder = 
  "https://drive.google.com/drive/u/0/folders/1CVRvmbzgZ4jgkb_FNGoO64_JH3q_ovOG"
folder_id = drive_get(as_id(weed.photo_folder))

#find files in folder
(files = drive_ls(folder_id))


url.to.spreadsheet.with.links.to.images <- 
  "https://docs.google.com/spreadsheets/d/1rHVMvOr_Q-nRI6fHe5rRknAmuEO7qseoiLc46SeqVXI/edit"
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




rm(files, folder_id, i_dir, file_i, i, jp_folder, path, paths,caption_size)




# Abiotic -----------------------------------------------------------------

 

#folder link to id
abio.photo_folder = 
  "https://drive.google.com/drive/u/0/folders/1YP1ZR8gPhL31_6GaUeNsHgoKVwe1xGBD"
folder_id = drive_get(as_id(abio.photo_folder))

#find files in folder
(files = drive_ls(folder_id))


url.to.spreadsheet.with.links.to.images <- 
  "https://docs.google.com/spreadsheets/d/1kuzmUzukeZHmReoerXrozlm4KS79GapN1j-NyVyYk3g/edit#gid=0"
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




rm(files, folder_id, i_dir, file_i, i, jp_folder, path, paths,caption_size)



# Beneficial  -------------------------------------------------------------

#folder link to id
bene.photo_folder =
  "https://drive.google.com/drive/u/0/folders/1TvpECh-fE34X7RBKU31rmKZ-yNr9wYqt"
folder_id = drive_get(as_id(bene.photo_folder))
#find files in folder
(files = drive_ls(folder_id))
url.to.spreadsheet.with.links.to.images <-
  "https://docs.google.com/spreadsheets/d/16XnrTvMZxI7iKHr8bJ7giwc7kUvgsArw-x2UAaK0EHs/edit#gid=0"
beneimg <-
  googlesheets4::read_sheet(url.to.spreadsheet.with.links.to.images)
(files <- files[files$name != "images", ])
(files <- files[files$name != "thumbnails", ])
if (file.exists(here("img/bene")) == 0)
  dir.create(here("img/bene"))
#Remove all files in final folder for re-loading
 sapply(list.files(
  here("img/bene/"),
  full.names = TRUE,
  recursive = T
), file.remove)

#loop dirs and download files inside them
for (i in seq_along(files$name)) {
  #list files
  i_dir = drive_ls(files[i,])
  path <- here("img/bene", files$name[i])
  #mkdir
  if (!file.exists(path))
    try(dir.create(path))
  i_dir <-
    i_dir[i_dir$name != "thumbnail.jpg",]
  #download files
  for (file_i in seq_along(i_dir$name)) {
    #fails if already exists
    try({
      drive_download(as_id(i_dir$id[file_i]),
                     path = str_c(path, "/", i_dir$name[file_i]))
    })
  }
  Sys.sleep(.1)
}
 
paths <-
  list.files(
    here("img/bene"),
    all.files  = TRUE,
    recursive = T,
    full.names = T
  )
beneimg <-
  beneimg %>%
  mutate(pth = "img/bene") %>%
  unite(pth, c(pth, folder, name), sep = "/")
# Get paths to the final image
# Get
for (i in seq(1:nrow(beneimg))) {
  # i = 1
  beneimg[i , "img_pth"] <-
    paths[grepl(beneimg[i , "pth"] %>% pull(), paths)]
  fin_pth <- paths[grepl(beneimg[i , "pth"] %>% pull(), paths)]
  # # file.exists(fin_pth)
  # fin_dir <- paste0(strsplit(fin_pth, "/")[[1]][1: length(strsplit(fin_pth, "/")[[1]])-1], collapse ="/")
  # if(!file.exists(fin_dir))dir.create(fin_dir)
  img <- image_read(beneimg[i , "img_pth"] %>% pull())
  image_info(img)$width
  border_width <- round((image_info(img)$width) / 30, 0)
  # border_height <- round((image_info(img)$height  )/30,0)
  img <-
    image_border(img, "white", paste(border_width, border_width, sep = "x"))
  heigh_offset <-  round((image_info(img)$height) / 120, 0)
  caption_size <-  round((image_info(img)$width) / 50, 0)
  img <-
    image_annotate(
      img,
      pull(beneimg[i , "credits"]),
      location = paste0("+1+", heigh_offset),
      # location = paste0("+1", heigh_offset),
      size = caption_size,
      gravity = "south",
      color = "black"
    )
  image_write(img, fin_pth)
  gc()
  Sys.sleep(.1)
  print(paste(i, fin_pth))
}

rm(files,
   folder_id,
   i_dir,
   file_i,
   i,
   jp_folder,
   path,
   paths,
   caption_size)
