
parsecsv <- function(file,header=TRUE,sep='\t',encoding="UTF-8") {	
	require(jsonlite)
	tableau<-read.csv(file=file, header=header, row.names=1, sep=sep,  na.strings="NA",  dec=".", strip.white=TRUE, blank.lines.skip=TRUE, encoding=encoding)
	return (jsonlite::toJSON(tableau))
}

afc <- function(data) {
	library(ade4)
	library(RSVGTipsDevice)
	library(factoextra)
	
	require(jsonlite)
	tableau <- jsonlite::fromJSON(data)
	namestbl<- gsub("([.]|[X]|[,])", "\ ", names(tableau))
	names(tableau)<-namestbl
	AFC<-tableau
	AFC<-apply(AFC, 2, function(x) ifelse(is.na(x), 0, x))
	AFC<- t(AFC)
	tbl<-AFC
	tbl[,margin.table(t(tbl),1)!=0]
	tbl[margin.table(t(tbl),2)!=0,]
	AFC<-tbl
	AFC.ca <- CA(AFC, ncp=7, graph=FALSE)
	
	
	
	fileName <- tempfile(fileext=".svg")
    on.exit(unlink(fileName))
	
	devSVGTips(file = fileName, toolTipMode=1, width = 10, height = 8, bg = "white", fg ="black", toolTipFontSize=8, onefile=TRUE)
	graph <- plot.CA(AFC.ca, axes=c(1,2), SeuilCol=0, SeuilLigne=0, Pem=c(0,0),  AFC, col.row="red", col.col="blue", col.row.sup = "#ec6804", col.col.sup = "#00d1ff", label=c("col", "col.sup", "row", "row.sup"),  title="")
	print(graph)
	dev.off()
	
	readChar(fileName, file.info(fileName)$size)
	
}
