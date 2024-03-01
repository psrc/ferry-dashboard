# Define server logic
shinyServer(function(input, output) {
  
  # Footer
  footer_server('psrcfooter')
  
  # Main Panel
  left_panel_server('leftMain', page_nm = "Main")
  ferry_overview_server('ferryOverview')
  
  # Urban Areas
  ferry_ua_server('UAferry')
  
  # Demographics
  ferry_demographics_server('DEMOGRAPHICSferry')
  
})    
