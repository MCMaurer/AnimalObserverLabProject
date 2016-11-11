shinyUI(fluidPage(

  ##Application title
  br(),
  includeMarkdown("rmarkdown/WelcomeAO.Rmd"),
	br(),
  br(),
  br(),
	tabsetPanel(
	id = "panels",
	tabPanel("What is AO Toolbox?", includeMarkdown("rmarkdown/AnimalObserver.Rmd")),
    tabPanel("Create behavioral protocol file",
    br(),
    sidebarLayout(
  
    # Sidebar with a slider input
    sidebarPanel(
    fileInput('dyadic', '1. Upload dyadic interactions csv file',
                accept=c('text/csv', 
								 'text/comma-separated-values,text/plain', 
								 '.csv')),
	fileInput('scan', '2. Upload activity (scan) csv file',
                accept=c('text/csv',
								 'text/comma-separated-values,text/plain', 
								 '.csv')),
	fileInput('solo', '3. Upload self-directed/health csv file',
                accept=c('text/csv',
								 'text/comma-separated-values,text/plain', 
								 '.csv')),
	fileInput('foods', '4. Upload optional list of food items (csv file)',
                accept=c('text/csv',
								 'text/comma-separated-values,text/plain', 
								 '.csv')),
	textInput("version", label = '5. Enter protocol version (mandatory):', value = "vX.X"),
	hr(),
    actionButton("run", label = "Run", icon=icon("play")),  textOutput("text1"),
	downloadButton('downloadBehaviorsJson', 'Download behaviors.json')
    ),
    mainPanel(
      tabsetPanel(id="panels2",
	  tabPanel("Instructions", includeMarkdown("rmarkdown/Create_structure.Rmd"))#,

    )
    ))),
	################################################
	################################################
    tabPanel("Create group composition file",
    br(),
    sidebarLayout(
   sidebarPanel(
    #helpText("Upload list of individuals"),
    fileInput('compo', 'Upload group composition csv file',
                accept=c('text/csv', 
								 'text/comma-separated-values,text/plain', 
								 '.csv')),
      textOutput("text2"),
	hr(),
	downloadButton('downloadAnimalsJson', 'Download animals.json')
    ),
    mainPanel(
      tabsetPanel(id="panels3",
      tabPanel("Instructions", includeMarkdown("rmarkdown/Create_animals_list.Rmd"))#,
      #tabPanel("Group composition")
      )
    )
    )
    ),
	################################################
	################################################
	tabPanel("Create global variables file",
	br(),
    sidebarLayout(
    # Sidebar with a slider input
    sidebarPanel(
    #helpText("Upload file"),
    
      fileInput('layout', '1. Upload your layout_info.json file (if you do not have one yet, skip this step and select "template" below)'),
      
      radioButtons("run3", "2. Choose whether you want to edit the uploaded layout_info.json file or use the default template",
                   c("uploaded file" = "upl",
                     "template" = "tpl")),

      actionButton("template", label = "Start editing options",  icon=icon("play")),
      br(),
      br(),
      fileInput('dayVars', '3. Upload session variables csv file',
                accept=c('text/csv', 
								 'text/comma-separated-values,text/plain', 
								 '.csv')),
	  fileInput('focalVars', '4. Upload focal variables csv file',
                accept=c('text/csv', 
								 'text/comma-separated-values,text/plain', 
								 '.csv')),
	  fileInput('scanVars', '5. Upload scan variables csv file',
                accept=c('text/csv', 
								 'text/comma-separated-values,text/plain', 
								 '.csv')),
	  fileInput('contVars', '6. Upload continuous focal variables csv file (optional)',
                accept=c('text/csv', 
								 'text/comma-separated-values,text/plain', 
								 '.csv')), 
br(),
    textInput("versionLayout", label = '7. Enter protocol version (mandatory):', value = "vX.X"),
	hr(),
    actionButton("run2", label = "Run", icon=icon("play")),
    textOutput("text3"),
	downloadButton('downloadLayoutJson', 'Download layout_info.json')
    ),    
    mainPanel(
      tabsetPanel(id="panels4",
                  tabPanel("Instructions",
                  includeMarkdown("rmarkdown/Create_layout_info.Rmd")
                  ),
                  tabPanel("Session setup",
                  #includeMarkdown("rmarkdown/Create_structure.Rmd"),
                  br(),
   				  htmlOutput("TextPin"),
   				  br(),
                  rHandsontableOutput("layoutPin"),
                  br(), 
                  htmlOutput("TextOptions"),
                  rHandsontableOutput("layoutOptions"),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br()
                  )
                  #tabPanel("Session variables"),
                  #tabPanel("Focal variables"),
                  #tabPanel("Scan variables"),
                  #tabPanel("Continuous variables"),
                  #tabPanel("View protocol")
    )
    )
    )
    ),
	################################################
	################################################
	  tabPanel("Additional customizations", includeMarkdown("rmarkdown/Customizations.Rmd"),
	  br(),
	  br(),
	  br(),
	  br(),
	  br(),
	  br(),
	  br(),
	  br(),
	  br(),
	  br(),
	  br(),
	  br()),
  	################################################
	################################################
    tabPanel("Convert collected data to csv",
    br(),
   
    sidebarLayout(

    # Sidebar with a slider input
    sidebarPanel(
    #helpText("Upload collected data"),
    fileInput('json.output.file', '1. Upload "Username_Date_Time.dat"'),
	fileInput('behaviors.json', '2. Upload "behaviors.json"'),
    fileInput('layout_info.json', '3. Upload "layout_info.json"'),
    checkboxInput("colmerge", "Merge identically-named columns from dyadic and self-directed/health data", FALSE),
	    HTML('<hr style="height:1px;border:none;color:#333;background-color:#333;" />'),
    downloadButton("downloadZip", label = "Download all data as zip"),
    br(), br(), h5("    OR"), br(),      
      downloadButton('downloadSessionsTable', 'Download list of sessions'),
      downloadButton('downloadFocalsTable', 'Download list of focals'),
      downloadButton('downloadBehaviorsTable', 'Download list of behaviors'),
	  downloadButton('downloadScansTable', 'Download list of scans'),
	  downloadButton('downloadBackgroundTapsTable', 'Download list of background taps'),
	  downloadButton('downloadCommentsTable', 'Download list of comments'),
	  downloadButton('downloadDayVarsTable', 'Download day variables'),
	  downloadButton('downloadFocalVarsTable', 'Download focal variables'),
	  downloadButton('downloadContinuousVarsTable', 'Download global variables'),
  	  downloadButton('downloadScanVarsTable', 'Download scan variables')
    ),
    mainPanel(
      tabsetPanel(id="panels5",
          tabPanel("Instructions", includeMarkdown("rmarkdown/Convert_datatocsv.Rmd")),
          tabPanel("Output Tables",
		br(),br(),
		textOutput("sessionsTable.csv"),
		tableOutput("table1"),
		br(),
		textOutput("focalsTable.csv"),
		tableOutput("table2"),
		br(),
		textOutput("behaviorsTable.csv"),
		tableOutput("table3"),
		br(),
		textOutput("scansTable.csv"),
		tableOutput("table4"),
		br(),
		textOutput("backgroundTapsTable.csv"),
		tableOutput("table5"),
		br(),
		textOutput("commentsTable.csv"),
		tableOutput("table6"),
		br(),
		textOutput("dayVarsTable.csv"),
		tableOutput("table7"),
		br(),
		textOutput("focalVarsTable.csv"),
		tableOutput("table8"),
		br(),
		textOutput("continuousVarsTable.csv"),
		tableOutput("table9"),
		br(),
		textOutput("scanVarsTable.csv"),
		tableOutput("table10"),
		br()),
		tabPanel("Data Inspection (in development)", 
		wellPanel(
      h4("Pick a Session/Focal"),
      sliderInput("session_nb",
                  "Session number:",
                  min = 0, max = 0, value = 0, step=1, round=T, ticks=F),
                  br(),
      sliderInput("focal_nb",
                  "Focal number:",
                  min = 0, max = 0, value = 0, step=1, round=T, ticks=F)
    	),
    	htmlOutput("networkTitle"),br(),
    	visNetworkOutput("network_behav", height = "600px")
    	
		)
		)
)
)
),
  	################################################
	################################################
tabPanel("PostgreSQL Database (in development)", 
br(),
    sidebarLayout(
    # Sidebar with a slider input
    sidebarPanel(
    HTML("<h4><b>    Connect to postgreSQL database</b></h4>"),
	br(),
    textInput("postgresHost", label = "Host name or IP address", value = "localhost"),
    textInput("postgresDBname", label = "Database name (if you have not set up an Animal Observer database yet enter the default database name)", value = "postgres"),
	textInput("postgresUser", label = "User name", value = "postgres"),
	numericInput("postgresPort", label = "Port", value = "5432"),
	passwordInput("postgresPwd", label = "Password (non-encrypted, use a VPN for safer connection!)", value = "postgres"),
	actionButton("postgresConnect", label = "Connect"),
	textOutput("DoneConnect"),	
	HTML('<hr style="height:2px;border:none;color:#000;background-color:#000;" />
'),
	HTML("<h4><b>    First time users: do you want to create a relational database structure matching your <em>layout_info.json</em> and <em>behaviors.json</em> files?</b></h4>"),
	br(),
	fileInput('behaviors.json2', '1. Upload "behaviors.json"'),
    fileInput('layout_info.json2', '2. Upload "layout_info.json"'),
	textInput("newDBname", label = "3. Database name (no space)", value = "animal_observer"),
	checkboxInput("colmerge2", "Merge identically-named columns from dyadic and self-directed/health data", TRUE),

	actionButton("createEmptyDB", label = "Create empty database structure"),
	textOutput("newDBcreated"),
	HTML('<hr style="height:2px;border:none;color:#000;background-color:#000;" />
'),
	HTML("<h4><b>    Upload collected data to existing database</b></h4>"),
	br(),
	fileInput('datafile.dat', '1. Upload "Username_Date_Time.dat"'),
	fileInput('behaviors.json3', '        2. Upload "behaviors.json"'),
    fileInput('layout_info.json3', '        3. Upload "layout_info.json"'),
    checkboxInput("colmerge3", "Merge identically-named columns from dyadic and self-directed/health data (warning: should match the database structure set up above)", TRUE),
	actionButton("runDbUpload6", label = "Upload .dat file to database"),
	textOutput("dataUploaded"),
    #textOutput("text4"),
    #HTML('<hr style="height:1px;border:none;color:#333;background-color:#333;" />'),
	#fileInput('zipFolder', 'OPTION 2. Use the "convert collected data to CSV" tab to convert the .dat file to csv files and download the resulting zip folder. You can then edit files within this zip folder (if needed) and upload the resulting zip folder below. The zip folder must contain the files you wish to upload to the database, named as in the output of the "convert data to csv" procedure.'),
	#actionButton("runDbUpload2", label = "Upload zip folder to database"),
    #textOutput("text5"),
    br()
    ),
    mainPanel(
    htmlOutput("postgresDBnameOutput"),br(),
    tableOutput("table11"),
    br()
    )
)
)
  	################################################
	################################################
)
)
)

































