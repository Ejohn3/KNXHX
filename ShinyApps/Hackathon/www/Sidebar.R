sidebar <- dashboardSidebar(
  sidebarMenu(
    
    menuItem('Solid Waste Management',tabName='CRQMain',startExpanded = TRUE,
             menuItem('Solid Waste Management',tabName='CRQMain',startExpanded = TRUE,
                      menuSubItem("Overview",tabName='Overview'),
                      menuSubItem("Data",tabName='Filter_Data')#,
                      #menuSubItem('Forecast',tabName='forecasts'),
                     # menuSubItem('Data Entry',tabName = 'Handson')
             ),
             menuItem('Garbage Violators',tabName='Violators',startExpanded = TRUE,
                      menuSubItem("Map",tabName='Map'),
                      menuSubItem("Clusters",tabName='Clusters'),
                      
    				 menuItem('Insert New Data',tabName='HandsOnMenu'),
    				 menuItem('Change Existing Data','ChangeData')
    )
  )
)
)
