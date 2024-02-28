# Define server logic
shinyServer(function(input, output) {
  
  # Footer
  footer_server('psrcfooter')
  
  # Main Panel
  banner_server('ferryBanner', 
                banner_title = "Ferries", 
                banner_subtitle = "Regional Transportation Plan",
                banner_url = "https://www.psrc.org/planning-2050/regional-transportation-plan")
  
  left_panel_server('leftMain', page_nm = "Main")
  ferry_overview_server('ferryOverview')
  ferry_ua_server('UAferry')
  
})    
