
parsecsv <- function(file,header=TRUE,sep='\t',encoding="UTF-8") {	
	require(jsonlite)
	tableau<-read.csv(file=file, header=header, row.names=1, sep=sep,  na.strings="NA",  dec=".", strip.white=TRUE, blank.lines.skip=TRUE, encoding=encoding)
	return (jsonlite::toJSON(tableau))
}
