# https://cran.r-project.org/web/packages/tesseract/vignettes/intro.html#read_from_pdf_files
library(tesseract)
library(pdftools)
pngBITR11 <- pdftools::pdf_convert("./TestFiles/tesseract/Biotropica2011.pdf", dpi = 600)
BITR_TEST<- tesseract(options = list(preserve_interword_spaces=1))
textBITR11 <- tesseract::ocr(pngBITR11, engine = BITR_TEST)
cat(textBITR11)
write.table(textBITR11, file = "./TestFiles/tesseract/BITR11.txt", sep = "\n", col.names = NA,qmethod = "double")
write.table(textBITR11, file = "./TestFiles/tesseract/BITR11.csv", sep = "\n", col.names = NA,qmethod = "double")


BITR11text<-ocr("./TestFiles/tesseract/Biotropica2011.pdf")
BITR87text<-ocr("./TestFiles/tesseract/Biotropica1987.pdf")

AGtext<-ocr("./TestFiles/tesseract/AGRONOMY11.pdf")
BITR11text<-ocr("./TestFiles/tesseract/BITR99.pdf")
MEPStext<-ocr("./TestFiles/tesseract/MEPS95.jpg")

cat(AGtext)
foo<-cat(BITRtext)
cat(MEPStext)


library(pdftools)

BITR14 <- pdf_text("./TestFiles/pdftools/Biotropica2014.pdf")


BITR11 <- pdf_text("./TestFiles/pdftools/Biotropica2011.pdf")
BITR11 <- strsplit(BITR11, "\n")
head(BITR11[[1]])
write.table(BITR11, file = "./TestFiles/pdftools/BITR11")
BITR11<-as.data.frame(BITR11)
write.table(BITR11, file = "./TestFiles/pdftools/BITR11.csv", sep = "\n", col.names = NA,qmethod = "double")

BITR87 <- pdf_text("./TestFiles/pdftools/Biotropica1987.pdf")


# Some have their ed board on a web page, which you can find with the wayback machine.
# https://statistics.berkeley.edu/computing/r-reading-webpages











https://github.com/hrbrmstr/wayback


https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/






# https://github.com/hrbrmstr/wayback
devtools::install_github("hrbrmstr/wayback")
library(wayback)
library(tidyverse)
archive_available("https://www.esapubs.org/esapubs/contacts_main.htm")
get_mementos("https://www.esapubs.org/esapubs/contacts_main.htm")
res <- read_memento("https://www.esapubs.org/esapubs/contacts_main.htm")
res <- stringi::stri_split_lines(res)[[1]]
res <- c(head(res, 6), tail(res, 8))
cat(paste0(res, collaspe="\n"))

library(rvest)

url <- "https://web.archive.org/web/20011227224139/http://www.esapubs.org:80/esapubs/contacts_main.htm"
ecology2002<-read_html(url)
test_data_html <- html_nodes(ecology2002,'<frame src="/web/20011227224139fw_/http://www.esapubs.org:80/esapubs/contacts.htm" name="Main" frameborder="NO">')
xpath /html/frameset/frame[2] 
population <- url %>%
  read_html() %>%
  html_nodes(xpath='/html/frameset/frame[2]') %>%
  html_table()


url <- "http://websitedownloader.io/preview/?id=878370"
ecology2002<-read_html(url)
test_data_html <- html_nodes(ecology2002,'<frame src="/web/20011227224139fw_/http://www.esapubs.org:80/esapubs/contacts.htm" name="Main" frameborder="NO">')
xpath /html/frameset/frame[2] 
population <- url %>%
  read_html() %>%
  html_nodes(xpath='/html/frameset/frame[2]') %>%
  html_table()

/html/frameset/frame[2]

<frame src="/web/20011227224139fw_/http://www.esapubs.org:80/esapubs/contacts.htm" name="Main" frameborder="NO">

/html/frameset/frame[2]
Ecology2002 = readLines('https://web.archive.org/web/20011227224139/http://www.esapubs.org:80/esapubs/contacts_main.htm')
grep('Board of Editors',Ecology2002)

