#' Sankey plot using ggplot2
#'
#' This function creates the sankey plots using the ggplot2 package in R.
#' More details in the vignettes.
#' @param data Input file
#' @param datacol Plot parameter
#' @param pdfname Output file
#' @export
#' @examples
#' create_sankey(data,datacol,pdfname)

create_sankey <- function(data,datacol,pdfname){
  require(ggplot2)

  dat = read.delim(data,header=T,sep="\t") #reading the data

  #Shaping the data for ggplot2
  arr = c()
  for(i in 1:dim(dat)[1]){
    tmp = dat[i,]
    risk = as.data.frame(rep(tmp[,1],dim(dat)[2]-1))
    year = colnames(tmp)[-1]
    val = tmp[,2:dim(dat)[2]]
    val = t(val)
    tmp_arr = cbind(risk,year,val)
    colnames(tmp_arr) = c('risk','year','val')
    arr = rbind(tmp_arr,arr)
  }

  arr$year = gsub('^X','',arr$year)
  #assigns color to the area under the sankey figure
  mycols = rainbow(dim(dat)[1])
  cols = c()
  for(i in 1:length(mycols)){
    tmp = rep(mycols[i],dim(risk)[1])
    tmp = t(tmp)
    cols = c(tmp,cols)
  }
  arr = cbind(arr,cols)

  #assign colors to the graph geom_ribbon lines/sankey figure
  col = mycols

  #Creating the sankey diagram using the geom_ribbon function
  p = ggplot(arr, aes(x=year,y=val,group=risk,fill=risk)) + ylab('') + xlab('') +
    geom_ribbon(aes(ymin = val - 1, ymax = val)) + scale_fill_manual(values=col, name="fill") +
    geom_ribbon(aes(x=year, ymin=val, ymax=val*2), fill=cols,alpha=0.7) +
    theme(axis.ticks.y = element_blank(),axis.text.y = element_blank()) +
    theme(legend.title = element_text(colour="black", size=10, face="bold")) +
    guides(fill=guide_legend(title=datacol))

  #Plotting the sankey diagram with and without vertical bars which are denoted as white boxes
  pdf(paste0(pdfname,'_verticalbars.pdf'))
    out1 = p + geom_vline(xintercept = c(2,3,4), color='white',size=8)
    print(out1)
  dev.off()

  pdf(paste0(pdfname,'_nobars.pdf'))
    out2 = p + geom_vline(xintercept = c(2,3,4), color='white')
    print(out2)
  dev.off()
}

