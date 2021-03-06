
parsecsv <- function(file,header=TRUE,sep='\t',encoding="UTF-8") {	
	require(jsonlite)
	tableau<-read.csv(file=file, header=header, row.names=1, sep=sep,  na.strings="NA",  dec=".", strip.white=TRUE, blank.lines.skip=TRUE, encoding=encoding)
	return (jsonlite::toJSON(tableau))
}


matrix_to_json <-function(m) { 
	require(jsonlite)
e<-list(colnames=colnames(m), rownames= rownames(m), values=m[])
return (jsonlite::toJSON(e))
}


json_to_matrix <- function(j) {
	require(jsonlite)
	e <- jsonlite::fromJSON(j)
	cells <- e$values
	rnames <- e$rownames
	cnames <- e$colnames
	nrow <- length(rnames)
	ncol <- length(cnames)
	mymatrix <- matrix(cells, nrow=nrow, ncol=ncol, byrow=FALSE,
  dimnames=list(rnames, cnames))
  return (mymatrix)
}

sqlite2df <- function(file) { 
	require("RSQLite")
	con = dbConnect(SQLite(),dbname = file)	
	results = dbSendQuery(con, "SELECT * FROM 'table'"); # attention : le nom de la table est : table donc il faut des quotes autour
	data = fetch(results);
	rownames(data) <- t(data[1]) # first column always the row name
	data[1] <- NULL
	dbClearResult(results);
	return (data) ;
}

compute_afc_sqlite <- function(data) {
	
	tableau <- sqlite2df(data)
	
	#return (matrix_to_json(tableau))
	namestbl<- gsub("([.]|[X]|[,])", "\ ", names(tableau))
	names(tableau)<-namestbl
	AFC<-tableau
	AFC<-apply(AFC, 2, function(x) ifelse(is.na(x), 0, x))
	AFC<- t(AFC)
	tbl<-AFC
	tbl[,margin.table(t(tbl),1)!=0]
	tbl[margin.table(t(tbl),2)!=0,]
	AFC<-tbl
	
	return (matrix_to_json(AFC))
}

compute_afc <- function(data) {
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
	
	return (matrix_to_json(AFC))
}

compute_chi2 <- function(afc) {
	library(FactoMineR)
	library(RSVGTipsDevice)
	library(ggplot2)

	require(jsonlite)
	
	
	AFC <- json_to_matrix(afc)


	chi2<-chisq.test(AFC)
	chi2 <- chi2[1:3]
	phi<-as.numeric(levels(as.factor((chi2[[1]]))))/sum(AFC)
	chi2<-data.frame(chi2,phi)
	chi2<-as.list(chi2)
	nom<-c("Chi Pearson","degrés de liberté","probabilité d'indépendance", "Phi-deux")
	names(chi2)<-nom

	return (jsonlite::toJSON(chi2))
}

chi2 <- function(data) {
	
	library(FactoMineR)
	library(RSVGTipsDevice)
	library(ggplot2)

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

	chi2<-chisq.test(AFC)
	chi2 <- chi2[1:3]
	phi<-as.numeric(levels(as.factor((chi2[[1]]))))/sum(AFC)
	chi2<-data.frame(chi2,phi)
	chi2<-as.list(chi2)
	nom<-c("Chi Pearson","degrés de liberté","probabilité d'indépendance", "Phi-deux")
	names(chi2)<-nom

	return (jsonlite::toJSON(chi2))

}
plot.CA <- function (x, axes = c(1, 2), SeuilCol = NULL, SeuilLigne = NULL, Pem=c(0,0),  tableau, Poids=NULL, 	
    xlim = NULL, ylim = NULL, invisible = NULL, col.row = "blue",
    col.col = "red", col.row.sup = "darkblue", col.col.sup = "darkred",
    label = "all", cex = .9, title = NULL, ...) {
    res.ca <- x
    if (!inherits(res.ca, "CA")) stop("Le tableau introduit ne convient pas")
    lab.row <- lab.col <- lab.row.sup <- lab.col.sup <- FALSE
    if(length(label)==1 && label=="all") lab.row <- lab.col <- lab.row.sup <- lab.col.sup <- TRUE
    if("row" %in% label) lab.row<-TRUE
    if("col" %in% label) lab.col<-TRUE
    if("row.sup" %in% label) lab.row.sup<-TRUE
    if("col.sup" %in% label) lab.col.sup<-TRUE



    require(RSVGTipsDevice)

    # Si le Seuil n'est pas définie il correspond à  la moyenne
    
    
	if(!is.null(SeuilLigne))
	{
		    mCol  <-SeuilLigne
		    mrow  <-100/nrow(res.ca$col$coord)
		    
	}
	if(!is.null(SeuilCol))
	{
		     mCol <-100/nrow(res.ca$row$coord)
		     mrow  <-SeuilCol
	}
	if(!is.null(SeuilLigne) & !is.null(SeuilCol))
	{
		    mCol <-SeuilLigne
		    mrow  <-SeuilCol
	}
	else
	{
		    mrow <-100/nrow(res.ca$row$coord)
		    mCol <-100/nrow(res.ca$col$coord)
	}

    # Construction des tableaux de coordonnÃ©es col. et lig. axes1 et axes2 
    tab.coord.col <- res.ca$col$coord[, axes]
    tab.contrib.col <-res.ca$col$contrib[, axes]
    tab.coord.row <- res.ca$row$coord[, axes]
    tab.contrib.row <-res.ca$row$contrib[, axes]


	# La taille des points sur la figure 
    p.contribution.row <- subset(tab.contrib.row, tab.contrib.row[,1] > mrow | tab.contrib.row[,2] > mrow)
    p.contribution.col <- subset(tab.contrib.col, tab.contrib.col[,1] > mCol | tab.contrib.col[,2] > mCol)



    
    # On construit le tableau des coords qui doivent avoir la mÃªme longueur d'oÃ¹ le OR 
    coord.col <- subset(tab.coord.col, tab.contrib.col[, 1] > mCol | tab.contrib.col[, 2]  > mCol)
    coord.row <- subset(tab.coord.row, tab.contrib.row[, 1] > mrow | tab.contrib.row[, 2] > mrow)

    # Transformation en matrice 
    coord.col <- as.matrix(coord.col)
    coord.row <- as.matrix(coord.row)
 
    # Retour Ã  l'execution normale de la fonction d'affichage 
    coord.row.sup <- coord.col.sup <- NULL




    if (!is.null(res.ca$row.sup)){
	coord.row.sup <- res.ca$row.sup$coord[, axes]
	coord.row.sup <- matrix(coord.row.sup, ncol=2)
	dimnames(coord.row.sup)<-list(row.names(res.ca$row.sup$coord))
	}
    if (!is.null(res.ca$col.sup)){
	coord.col.sup <- res.ca$col.sup$coord[, axes]
	coord.col.sup <- matrix(coord.col.sup, ncol=2)
	dimnames(coord.col.sup)<-list(row.names(res.ca$col.sup$coord))
	}

#    print(coord.row.sup)

    test.invisible <- vector(length = 4)
    if (!is.null(invisible)) {
        test.invisible[1] <- match("row", invisible)
        test.invisible[2] <- match("col", invisible)
        test.invisible[3] <- match("row.sup", invisible)
        test.invisible[4] <- match("col.sup", invisible)
    }
    else  test.invisible <- rep(NA, 4)
    if (is.null(xlim)) {
      xmin <- xmax <- 0
      if(is.na(test.invisible[1])) xmin <- min(xmin, coord.row[,1])
      if(is.na(test.invisible[1])) xmax <- max(xmax, coord.row[,1])
      if(is.na(test.invisible[3])) xmin <- min(xmin, coord.row.sup[, 1])
      if(is.na(test.invisible[3])) xmax <- max(xmax, coord.row.sup[, 1])
      if(is.na(test.invisible[2])) xmin <- min(xmin, coord.col[,1])
      if(is.na(test.invisible[2])) xmax <- max(xmax, coord.col[,1])
      if(is.na(test.invisible[4])) xmin <- min(xmin, coord.col.sup[, 1])
      if(is.na(test.invisible[4])) xmax <- max(xmax, coord.col.sup[, 1])
        xlim <- c(xmin, xmax) * 1.2
    }
    else {
      xmin = xlim[1]
      xmax = xlim[2]
    }
    if (is.null(ylim)) {
      ymin <- ymax <- 0
      if(is.na(test.invisible[1])) ymin <- min(ymin, coord.row[,2])
      if(is.na(test.invisible[1])) ymax <- max(ymax, coord.row[,2])
      if(is.na(test.invisible[3])) ymin <- min(ymin, coord.row.sup[,2])
      if(is.na(test.invisible[3])) ymax <- max(ymax, coord.row.sup[,2])
      if(is.na(test.invisible[2])) ymin <- min(ymin, coord.col[,2])
      if(is.na(test.invisible[2])) ymax <- max(ymax, coord.col[,2])
      if(is.na(test.invisible[4])) ymin <- min(ymin, coord.col.sup[,2])
      if(is.na(test.invisible[4])) ymax <- max(ymax, coord.col.sup[,2])
        ylim <- c(ymin, ymax) * 1.2
    }
    else {
      ymin = ylim[1]
      ymax = ylim[2]
    }
    sub.titre <- NULL
    if (is.null(title)) 
	{titre <- "Analyse factorielle des correspondances"}
    else {
#      sub.titre <- "Analyse factorielle des correspondances"
      titre <- title
    }

    plot(0, 0, main = titre, xlab = paste("Facteur ",axes[1]," (",signif(res.ca$eig[axes[1],2],4),"%)",sep=""), ylab = paste("Facteur ",axes[2]," (",signif(res.ca$eig[axes[2],2],4),"%)",sep=""), xlim = xlim, ylim = ylim, col = "white", asp=1, cex=cex)


	# Il faut avoir chargé le package R pem.R 

	nbval<-length(Pem)
	if(Pem[1]==0 && Pem[2]==0){
	SeuilPemInf=NULL
	SeuilPemSup=NULL
	}
	else{
		if(Pem[1]==0 & Pem[2]>0){
			SeuilPemInf=NULL
			SeuilPemSup=Pem[2]
			}
		if(Pem[1]>0 & Pem[2]==0){
			SeuilPemInf=Pem[1]
			SeuilPemSup=NULL
			}
		if(Pem[1]>0 & Pem[2]>0){
			SeuilPemInf=Pem[1]
			SeuilPemSup=Pem[2]
			}
	dessinPem(tableau,tab.coord.col,tab.coord.row,SeuilPemInf,SeuilPemSup, colgn="gray")
	
	
	}
	#dessinPem(tableau,tab.coord.col,tab.coord.row,Pem)






    if (!is.null(sub.titre)) title(sub = sub.titre, cex.sub = cex, font.sub = 2, col.sub = "steelblue4", adj = 0, line = 3.8)
    abline(h=0,lty=2)
    abline(v=0,lty=2)


    if (is.na(test.invisible[1])) 
    {
	nom<-rownames(coord.row)
	for(i in 1:length(coord.row[,1]))
	{
	setSVGShapeToolTip(title=paste(nom[i]), desc=paste("(", "facteur", axes[1] ,":",  round(p.contribution.row[i,1],3), "; ", "facteur", axes[2], ":",  round(p.contribution.row[i,2],3), ")" ))
	# taille des points 
	if(!is.null(Poids)){
	p.contribution.r<-log(p.contribution.row[i,1]+p.contribution.row[i,2])
	}
	else{
	p.contribution.r<-cex
	}
	points(coord.row[i,1], coord.row[i,2], pch=25, bg=col.row, col=col.row,cex=p.contribution.r)
#	points(coord.row[i,1], coord.row[i,2], pch=19, col=col.row, cex=cex)
	}	

#	points(coord.row, pch = 25, col = col.row, cex = cex)
	if (lab.row)  	text(coord.row[, 1], y = coord.row[, 2], labels = rownames(coord.row), pos = 3, col = col.row, cex = cex)
    }

    if (is.na(test.invisible[2])) {	
	nomCol<-rownames(coord.col)
	for(i in 1:length(nomCol))
	{
	setSVGShapeToolTip(title=paste(nomCol[i]), desc=paste("(", "facteur", axes[1] ,":",  round(p.contribution.col[i,1],3), "; ", "facteur", axes[2], ":",  round(p.contribution.col[i,2],3), ")" ))
	# taille des points 
	if(!is.null(Poids)){p.contribution.c<-log(p.contribution.col[i,1]+p.contribution.col[i,2]) }   
	else{p.contribution.c<-cex}

#	points(coord.col[i,1], coord.col[i,2], pch=12, col=col.col, cex=cex)
	points(coord.col[i,1], coord.col[i,2], pch=12, col=col.col,cex=p.contribution.c)
	}

   #	points(coord.col[, 1], y = coord.col[, 2], pch = 25, col = col.col, cex = cex)
	if (lab.col) text(coord.col[, 1], y = coord.col[, 2], labels = rownames(coord.col), pos = 3, col = col.col, cex = cex)
    }
    if (!is.null(res.ca$col.sup) & is.na(test.invisible[4])) {
      points(coord.col.sup[, 1], y = coord.col.sup[, 2], pch = 25, col = col.col.sup, cex = cex)
      if (lab.col.sup) text(coord.col.sup[, 1], y = coord.col.sup[, 2], labels = rownames(coord.col.sup), pos = 3, col = col.col.sup, cex = 0.8)
    }
    if (!is.null(res.ca$row.sup) & is.na(test.invisible[3])) {
      points(coord.row.sup[, 1], y = coord.row.sup[, 2], pch = 25, col = col.row.sup, cex = cex)
      if (lab.row.sup) text(coord.row.sup[, 1], y = coord.row.sup[, 2], labels = rownames(coord.row.sup), pos = 3, col = col.row.sup, cex = 0.8)
    }
    
    ###########################################  
    # Ajout des seuillages sur les graphiques #
    ###########################################

    Xleg<-xmin * 1
    Yleg<-ymin * 1.1
    text(Xleg,Yleg,paste("Seuil en colonne :",mrow,", seuil en ligne :",mCol,""), col="red", cex=.7)	 
	
}

plot_afc <- function(afc) {
	require(FactoMineR)
	require(RSVGTipsDevice)
	require(ggplot2)
	
	require(jsonlite)
	#AFC <- jsonlite::fromJSON(afc)
	AFC <- json_to_matrix(afc)
	
	AFC.ca <- CA(AFC, ncp=7, graph=FALSE)
	
	
	
	fileName <- tempfile(fileext=".svg")
    on.exit(unlink(fileName))
	
	devSVGTips(file = fileName, toolTipMode=1, width = 10, height = 8, bg = "white", fg ="black", toolTipFontSize=8, onefile=TRUE)
	graph <- plot.CA(AFC.ca, axes=c(1,2), SeuilCol=0, SeuilLigne=0, Pem=c(0,0),  AFC, col.row="red", col.col="blue", col.row.sup = "#ec6804", col.col.sup = "#00d1ff", label=c("col", "col.sup", "row", "row.sup"),  title="")
	print(graph)
	dev.off()
	
	readChar(fileName, file.info(fileName)$size)
	
}

Myggplot.CA <- function(res, xax = 1, yax = 2, label = NULL, SeuilLigne=NULL, SeuilCol=NULL,  SeuilPem=FALSE, alpha = 0.5, LabelSize.row=5, LabelSize.col=7, titre = NULL,  col.row = "darkblue", col.col = "red", col.row.sup = "khaki3", col.col.sup = "DarkSeaGreen4", classification=FALSE, palette = "Set2", ...) {

#	if (!inherits(res, "CA")) stop("Le tableau introduit ne convient pas")
#	    lab.row <- lab.col <- lab.row.sup <- lab.col.sup <- FALSE


	if(!is.null(SeuilLigne) & is.null(SeuilCol)){
		    mCol <- 0
		    mrow <-SeuilLigne
	}
	else if(!is.null(SeuilCol) & is.null(SeuilLigne)){
		     mCol <-SeuilCol
		     mrow <- 0
	}
	else if(!is.null(SeuilLigne) & !is.null(SeuilCol)){
		    mCol  <- SeuilCol
		    mrow  <- SeuilLigne
	}
	else{
		    mrow <- 0
		    mCol <- 0
	}


	# Si il y a une classification

	if(classification==TRUE){

		tab.coord.col<-data.frame(x=res[[2]]$x, y=res[[2]]$y, row.names=rownames(res[[2]]))
		tab.coord.row<-data.frame(x=res[[1]]$x, y=res[[1]]$y, row.names=rownames(res[[1]]))

	    	tab.contrib.row <-as.matrix(data.frame(res[[1]][, 6],res[[1]][, 7]))
	    	tab.contrib.col <-as.matrix(data.frame(res[[2]][, 6],res[[2]][, 7]))
		eigOfF1<-round(res[[3]][1], 3) 
		eigOfF2<-round(res[[3]][2], 3)
		coord.col <- subset(tab.coord.col, tab.contrib.col[, 1] > mCol | tab.contrib.col[, 2] > mCol)
	    	coord.row <- subset(tab.coord.row, tab.contrib.row[, 1] > mrow | tab.contrib.row[, 2] > mrow)
		col.a<-subset(res[[1]], tab.contrib.row[, 1] > mrow | tab.contrib.row[, 2] > mrow)
		col.b<-subset(res[[2]], tab.contrib.col[, 1] > mCol | tab.contrib.col[, 2] > mCol)
		col.row<-as.vector(col.b[,5])
		col.col<-as.vector(col.a[,5])
		print(col.col)
		print(col.row)
#		col.col<-as.vector(col.row[,4])		


	}
	else{
		tab.coord.row <- data.frame(x = res$row$coord[, xax], y = res$row$coord[, yax])
		tab.coord.col <- data.frame(x = res$col$coord[, xax], y = res$col$coord[, yax])
		tab.contrib.col <-res$col$contrib
	    	tab.contrib.row <-res$row$contrib
		eigOfF1<-round(res$eig[xax, 2],3)
		eigOfF2<-round(res$eig[yax, 2],3)
		coord.col <- subset(tab.coord.col, tab.contrib.col[, 1] > mCol | tab.contrib.col[, 2] > mCol)
	    	coord.row <- subset(tab.coord.row, tab.contrib.row[, 1] > mrow | tab.contrib.row[, 2] > mrow)
	}



	NameOfRows<-data.frame(coord.row,name=rownames(coord.row))
	NameOfCols<-data.frame(coord.col,name=rownames(coord.col))

	g <- ggplot(data = coord.row, aes_string(x = "x", y = "y"), fill=condition) + 
		geom_vline(xintercept = 0, col="black") +
		geom_hline(yintercept = 0, col="black") +
		scale_x_continuous(paste0("Facteur"," ", xax, "(",eigOfF1,"% )")) +
		scale_y_continuous(paste0("Facteur"," ", yax, "(",eigOfF2 ,"% )")) + 
		geom_point(alpha=0.3, col=col.col) +
		geom_text(data=NameOfRows, aes(x,y, label=name),  hjust=0.1, vjust=-0.6, size=LabelSize.col, col=col.col) 
	g <- g + labs(title=titre)
	g <- g + geom_point(data=coord.col, size=1, shape=6) +
		geom_text(data=NameOfCols, aes(x,y, label=name), col=col.row, hjust=0.25, vjust=-0.7, size=LabelSize.row) 


	if (!is.null(res$row.sup$coord)){
	coord.row.sup <- data.frame(x=res$row.sup$coord[, xax], y=res$row.sup$coord[, yax])
	NameOfRowsSup<-data.frame(coord.row.sup,name=rownames(res$row.sup$coord))

	g <- g + geom_point(data=coord.row.sup, color=col.row.sup, size=1,  shape=25) +
		geom_text(data=NameOfRowsSup, aes(x,y, label=name),  hjust=0.2, vjust=-0.5, size=LabelSize.row+1.5, col=col.row.sup)  
	} 
	if (!is.null(res$col.sup$coord)){

	coord.col.sup <- data.frame(x=res$col.sup$coord[, xax], y=res$col.sup$coord[, yax])
	NameOfColsSup<-data.frame(coord.col.sup,name=rownames(res$col.sup$coord))
	LabelSize.col<-LabelSize.col+1
	g <- g + geom_point(data=coord.col.sup, color=col.col.sup, size=0.85, shape=25) +
		geom_text(data=NameOfColsSup, aes(x,y, label=name),  hjust=0.2, vjust=-0.5, size=LabelSize.col, col=col.col.sup)  
	} 
	g + coord_fixed(ratio=6)

	if(SeuilPem==TRUE){
	g <- g + theme_bw(base_size = 16)
	g <- g + theme(panel.border = element_rect(size=1.5,colour = "darkgray"), panel.grid.minor = element_line(colour = "gray", linetype = "dashed"))
	}
	else{
	g <- g + theme_bw(base_size = 16)
	g <- g + theme(panel.border = element_rect(size=1.5,colour = "darkgray"), panel.grid.minor = element_line(colour = "gray", linetype = "dashed"))

	}
	g <- g + theme(plot.title = element_text(size=12))
	g <- g + theme(axis.text = element_text(size = 10)) # change les textes des axes
	g <- g + theme(axis.title = element_text(size = 14)) # change les titres des axes


	return(g)
 
}



plot_afc2 <- function(afc) {
	require(FactoMineR)
	require(RSVGTipsDevice)
	require(ggplot2)
	
	require(jsonlite)
	#AFC <- jsonlite::fromJSON(afc)
	AFC <- json_to_matrix(afc)
	
	AFC.ca <- CA(AFC, ncp=7, graph=FALSE)
	
	
	
	fileName <- tempfile(fileext=".svg")
    on.exit(unlink(fileName))
	
	devSVGTips(file = fileName, toolTipMode=1, width = 10, height = 8, bg = "white", fg ="black", toolTipFontSize=8, onefile=TRUE)
	#graph <- Myggplot.CA(AFC.ca, axes=c(1,2), SeuilCol=0, SeuilLigne=0, Pem=c(0,0),  AFC, col.row="red", col.col="blue", col.row.sup = "#ec6804", col.col.sup = "#00d1ff", label=c("col", "col.sup", "row", "row.sup"),  title="")
	graph <- Myggplot.CA(AFC.ca,xax = 1, yax = 2, label = NULL, SeuilLigne=NULL, SeuilCol=NULL,  SeuilPem=FALSE, alpha = 0.5, LabelSize.row=5, LabelSize.col=7, titre = NULL,  col.row = "darkblue", col.col = "red", col.row.sup = "khaki3", col.col.sup = "DarkSeaGreen4", classification=FALSE, palette = "Set2")
	print(graph)
	dev.off()
	
	readChar(fileName, file.info(fileName)$size)
	
}

afc <- function(data) { # deprecated
	require(FactoMineR)
	require(RSVGTipsDevice)
	require(ggplot2)
	
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
