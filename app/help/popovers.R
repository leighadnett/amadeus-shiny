
add_popovers<-function(session)
{

  addTooltip(session,"menu  ", "Follow this menu to add assets to your portfolio/universe."
             , placement = "right", trigger = "hover", options = list(container = "body"))
  
  addTooltip(session,"select_database", "Currently offers data from Factset and Investing.com.
             Please note that only the Factset Single Stock database contains total return time-series.
             Make sure to use assets without payouts (such as accumulating ETFs) when using the other databases.
             ", placement = "right", trigger = "hover", options = list(container = "body"))
 
  addTooltip(session,"asset_input_table", "This is your portfolio overview table. Add assets to it by using the sidebar menu on the left. 
             You can get further information on how to use this input table by clicking in the cells of its first row.
             You can hide or show it by clicking on the toggle button above."
             , placement = "top", trigger = "hover", options = list(container = "body"))

  addTooltip(session,"select_base_currency", "This is the base currency of your portfolio. All returns will be converted into this currency. 
              If you choose to hedge foreign exchange rate risk for your portfolio or parts of it, the necessary hedges will be implemented automatically.
             ", placement = "right", trigger = "hover", options = list(container = "body"))

  addTooltip(session,"select_asset_class", "You need to select a database and an asset-class before you can add securities below.
               If you select Investing.com you can afterwards search for assets by entering a name or ISIN. If you select the Factset ETF database, you need to search by inserting ISIN or FactsetID.
               Click on search asset afterwards to load investable assets.
               ", placement = "right", trigger = "hover", options = list(container = "body"))
  
  addTooltip(session,"search_asset",  "Click here to load investable assets from Investing.com or the Factset ETF database after inserting a search-value. 
            Then select the asset you would like to add to your Portfolio from the new menu above. 
             ", placement = "right", trigger = "hover", options = list(container = "body"))

  addTooltip(session,"asset_weight",  "You can also adjust the weight of individual assets afterwards by changing the respective column in the portfolio overview table.
             ", placement = "right", trigger = "hover", options = list(container = "body"))
  
  
  addTooltip(session,"run_single_security_overview", "Please note that this function defaults to total return for the Factset Single Stock Databaset but loads prices only for Factset ETFs and Investing.com data.
             ", placement = "right", trigger = "hover", options = list(container = "body"))
  
}