if(rgplates:::getOS()=="linux") wd <- file.path(Sys.getenv("Dropbox"), "Software/icosa")
if(rgplates:::getOS()=="windows") wd <- file.path("D:/icosa")
if(rgplates:::getOS()=="osx") wd <- file.path("~/Desktop/icosa")
