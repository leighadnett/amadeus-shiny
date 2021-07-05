

function_find_asset<-function(all_assets)
{

  
  Universe<-subset(all_assets,select=c(factset_sector_desc,factset_industry_desc,id))
  
  
  ch<-collapsibleTreeSummary(
    Universe,
    c("factset_sector_desc","factset_industry_desc","id"),
    maxPercent = 100,
    nodeSize = "leafCount",
    collapsed = T,
    zoomable = FALSE,
    fill=colorRampPalette(c("#04103b","#dd0400"))
  )
  
  return(ch)
}

