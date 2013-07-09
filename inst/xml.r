require(XML)
source("xmlhelperfun.r")
### BASEL DATABASE 01
inputBS01 <- xmlParse("./Basel_01.xml")
### get slice and image nodes
slicenodeBS01 <-  getNodeSet(inputBS01,"/ImagicXML/Update/Row[@Table='Slice']")
imagenodeBS01 <- lapply(slicenodeBS01,getNodeSet,"Row/Field[@Name='Im_FileName']")
### get per-slice image names
imagesBS01 <- lapply(imagenodeBS01,getValueFun)
slicenameBS01 <- lapply(slicenodeBS01,getNodeSet,"Field[@Name='Slice_serial_number']")
### get slice names
sliceID_BS01<- sapply(slicenameBS01,getValueFun)
names(imagesBS01) <- sliceID_BS01
## get Tooth ID
zahnID_BS01<- sliceID_BS01
zahnID_BS01 <- strsplit(sliceID_BS01,split="_")
zahnID_BS01 <- sapply(zahnID_BS01,function(x) x <- paste(x[1],x[2],sep="_"))

### setup source and destinateion directories
from="/media/Labor Extern/schlager/BS01-Export/AD70DA7F672D4d5aB2F47BE9BDED4202.imp/Refs"
to <- "/media/Labor Extern/schlager/BS01-Export/sorted"
### create per-tooth/per-slice dirs/subdirs and copy images
copyfun(from,to,zahnID_BS01,sliceID_BS01)

###Error message after copying:
###In file.copy(fromfiles, slicedir) :
###  problem copying /media/Labor Extern/schlager/BS01-Export/AD70DA7F672D4d5aB2F47BE9BDED4202.imp/Refs/Image_000001.tif to /media/Labor Extern/schlager/BS01-Export/sorted/Z_1059/Z_1059_SL_1153/Image_000001.tif: No such file or directory

### BASEL DATABASE 02
inputBS02 <- xmlParse("./Basel_02.xml")
### get slice and image nodes
slicenodeBS02 <-  getNodeSet(inputBS02,"/ImagicXML/Update/Row[@Table='Slice']")
imagenodeBS02 <- lapply(slicenodeBS02,getNodeSet,"Row[@Table='Image']/Field[@Name='Im_FileName']")
moddatenodeBS02 <- lapply(slicenodeBS02,getNodeSet,"Row[@Table='Image']/Field[@Name='IAModificationDate']")
moddateBS02 <- lapply(moddatenodeBS02,getValueFun)
### get per-slice image names
imagesBS02 <- lapply(imagenodeBS02,getValueFun)
slicenameBS02 <- lapply(slicenodeBS02,getNodeSet,"Field[@Name='Slice_serial_number']")
### get slice names
sliceID_BS02<- sapply(slicenameBS02,getValueFun)
names(imagesBS02) <- sliceID_BS02
## get Tooth ID
zahnID_BS02<- sliceID_BS02
zahnID_BS02 <- strsplit(sliceID_BS02,split="_")
zahnID_BS02 <- sapply(zahnID_BS02,function(x) x <- paste(x[1],x[2],sep="_"))

### setup source and destinateion directories
from02="/media/Labor Extern/schlager/BS02-Export/120D069F3DC24193AED3AA31947E64B8.imp/Refs"
to02 <- "/media/Labor Extern/schlager/BS02-Export/sorted"
### create per-tooth/per-slice dirs/subdirs and copy images
copyfun(from02,to02,zahnID_BS02,sliceID_BS02,imagesBS02)

doc <- xmlParse("../import.xml")
getNodeSet(doc, "/r:ImageXML", c(r = "urn:schemas-microsoft-com:datatypes"))
