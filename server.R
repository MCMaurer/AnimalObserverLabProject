# Define server logic for random distribution application
shinyServer(function(input, output, session) {

###########################################
################1st tab
dyadicInput <- reactive({
   	dyadic <- input$dyadic
    if (is.null(dyadic))
      return(NULL)
    read.csv(dyadic$datapath, check.names=F)
  })
scanInput <- reactive({
   	scan <- input$scan
    if (is.null(scan))
      return(NULL)
    read.csv(scan$datapath, check.names=F)
  })
soloInput <- reactive({
   	solo <- input$solo
    if (is.null(solo))
      return(NULL)
    read.csv(solo$datapath, check.names=F)
  })	
foodInput <- reactive({
   	food <- input$foods
    if (is.null(food))
      return(NULL)
    read.csv(food$datapath, check.names=F)
  })
  
textInput <- reactive({
	version <- input$version
	return(version)
})

observeEvent(input$link_to_structure, {
  newvalue <- "Create behavioral protocol file"
  updateTabsetPanel(session,"panels",newvalue)
})

observeEvent(input$link_to_structure2, {
  newvalue <- "Create behavioral protocol file"
  updateTabsetPanel(session,"panels",newvalue)
})

observeEvent(input$link_to_studyanimals, {
  newvalue <- "Create group composition file"
  updateTabsetPanel(session,"panels",newvalue)
})

observeEvent(input$link_to_globalvar, {
  newvalue <- "Create global variables file"
  updateTabsetPanel(session,"panels",newvalue)
})

observeEvent(input$link_to_customization, {
  newvalue <- "Additional customizations"
  updateTabsetPanel(session,"panels",newvalue)
})

observeEvent(input$link_to_foods, {
  newvalue <- "Foods"
  updateTabsetPanel(session,"panels2",newvalue)
})

observeEvent(input$link_to_dyad, {
  newvalue <- "Dyadic"
  updateTabsetPanel(session,"panels2",newvalue)
})

observeEvent(input$link_to_scan, {
  newvalue <- "Scan"
  updateTabsetPanel(session,"panels2",newvalue)
})

observeEvent(input$link_to_solo, {
  newvalue <- "Solo"
  updateTabsetPanel(session,"panels2",newvalue)
})

observeEvent(input$link_to_animals, {
  newvalue <- "Create group composition file"
  updateTabsetPanel(session,"panels",newvalue)
})

observeEvent(input$link_to_welcome, {
  newvalue <- "What is AO Toolbox?"
  updateTabsetPanel(session,"panels",newvalue)
})

observeEvent(input$link_to_welcome2, {
  newvalue <- "What is AO Toolbox?"
  updateTabsetPanel(session,"panels",newvalue)
})

observeEvent(input$link_to_grp, {
  newvalue <- "Group composition"
  updateTabsetPanel(session,"panels3",newvalue)
})

observeEvent(input$link_to_globalvar2, {
  newvalue <- "Create global variables file"
  updateTabsetPanel(session,"panels",newvalue)
})

observeEvent(input$link_to_globalvar3, {
  newvalue <- "Create global variables file"
  updateTabsetPanel(session,"panels",newvalue)
})
  
observeEvent(input$link_to_sessionSetup, {
  newvalue <- "Session setup"
  updateTabsetPanel(session,"panels4",newvalue)
})
  
observeEvent(input$link_to_structure3, {
  newvalue <- "Create behavioral protocol file"
  updateTabsetPanel(session,"panels",newvalue)
})

observeEvent(input$link_to_globalvar4, {
  newvalue <- "Create global variables file"
  updateTabsetPanel(session,"panels",newvalue)
})

observeEvent(input$link_to_pSQL, {
  newvalue <- "PostgreSQL Database"
  updateTabsetPanel(session,"panels6",newvalue)
})

observeEvent(input$link_to_pSQL2, {
  newvalue <- "PostgreSQL Database"
  updateTabsetPanel(session,"panels6",newvalue)
})
  
  
dataOutput1 <- eventReactive(input$run, {
    if(is.null(dyadicInput()) | is.null(scanInput()) | is.null(soloInput()) | textInput()=="vX.X"){
			return(NULL)
			} else 	if (is.null(foodInput())) {
			return(prepareBehaviorsJson(dyadicInput(), scanInput(), soloInput(),textInput()))
			} else {
			allBehaviorsTables <- dyadicScanSolo(dyadicInput(), scanInput(), soloInput(), foodInput())
			return(prepareBehaviorsJson(allBehaviorsTables$dyadic.all, allBehaviorsTables$scan.all, allBehaviorsTables$solo.all, textInput()))
			}
})

output$text1 <- renderText({
	if(is.null(dataOutput1())){return(NULL)}
		return("DONE!")	
		})


output$downloadBehaviorsJson <- downloadHandler(
    filename = function() { 
		 paste('behaviors.json') 
	 },
    content = function(file) {
    	writeLines(dataOutput1(), con=file)
    }
  )

###########################################
################2nd tab
compoInput <- reactive({
   	compo <- input$compo
    if (is.null(compo))
      return(NULL) else {
    return(prepareGroupCompo(read.csv(compo$datapath, check.names=F)))
    }
})

output$text2 <- renderText({
	if(is.null(compoInput())) return(NULL) else return("DONE")
})

output$downloadAnimalsJson <- downloadHandler(
    filename = function() {
		 paste('animals.json') 
	 },
    content = function(file) {
    	writeLines(compoInput(), con=file)
    }
)

###########################################
################3rd tab

values <-  reactiveValues()

dataOutput2 <- eventReactive(input$template, {
  if(input$run3=="upl" & is.null(dataOutput3())){
    return(NULL)
  } else if (input$run3=="upl" & !is.null(dataOutput3())){
  	#cat(file=stderr(), "input$run3==upl")
  	#cat(file=stderr(), paste(dataOutput3()))
    return(dataOutput3())
  } else {
  	#cat(file=stderr(), "input$run3==template")
return(readLayoutJson(fromJSON(file="layout_info_default.json")))
  }
})

dataOutput3 <- reactive({
  if(is.null(input$layout)){
    return(NULL)
  } else {
    return(readLayoutJson(fromJSON(paste(readLines(input$layout$datapath, warn=F), collapse=""))))
  }
})


dataPinLayout = reactive({
    if(is.null(dataOutput2())) return(NULL)
    if(is.null(input$layoutPin)){
    	temp = dataOutput2()
    	MAT <- temp[[1]]
    } else {
    	MAT=hot_to_r(input$layoutPin)
    }
    for(i in 1:ncol(MAT)) MAT[,i] <- as.character(MAT[,i])
     values[["pinLayout"]] = MAT
     return(MAT)
  })

dataOptionsLayout = reactive({  
    if(is.null(dataOutput2())) return(NULL)
    if(is.null(input$layoutOptions)){
    	temp = dataOutput2()
    	MAT <- as.matrix(data.frame(settings=names(temp[[2]]), values=unlist(temp[[2]]), stringsAsFactors=F))[-1,]
    } else {
    	MAT=hot_to_r(input$layoutOptions)
    }
     for(i in 1:ncol(MAT)) MAT[,i] <- as.character(MAT[,i])
     values[["optionsLayout"]] = MAT
     return(MAT)
  })
  
dayVarsInput <- reactive({
   	dayVars <- input$dayVars
    if (is.null(dayVars))
      return(NULL)
    read.csv(dayVars$datapath, check.names=F)
  })
focalVarsInput <- reactive({
   	focalVars <- input$focalVars
    if (is.null(focalVars))
      return(NULL)
    read.csv(focalVars$datapath, check.names=F)
  })
scanVarsInput <- reactive({
   	scanVars <- input$scanVars
    if (is.null(scanVars))
      return(NULL)
    read.csv(scanVars$datapath, check.names=F)
  })	  
contVarsInput <- reactive({
   	contVars <- input$contVars
    if (is.null(contVars))
      return(NULL)
    read.csv(contVars$datapath, check.names=F)
  })


dataOutput4 <- eventReactive(input$run2, {
	#cat(file=stderr(), "inside dataOutput4\n")

    if(is.null(dayVarsInput()) | is.null(focalVarsInput()) | is.null(scanVarsInput()) | versionTextInput()=="vX.X"){
    		#cat(file=stderr(), "dataOutput4 returning NULL\n")

			return(NULL)
			}

    	temp <- list()
    	temp[[1]] <- values[["pinLayout"]]  
    	temp[[2]] <- listFromCsv(dat = dayVarsInput())
    	temp[[3]] <- listFromCsv(dat = focalVarsInput())
   		temp[[4]] <- listFromCsv(dat = scanVarsInput())
   		temp[[5]] <- listFromCsv(dat = contVarsInput())
	
	optionnames <- values[["optionsLayout"]][,1]
	optionvalues<- as.character(values[["optionsLayout"]][,2])
	
	temp[[6]] <- data.frame(values=c(versionTextInput(), optionvalues))
	temp[[6]] <- as.data.frame(t(as.matrix(temp[[6]])))
	names(temp[[6]]) <- c("version", optionnames)
	#cat(file=stderr(), paste(temp))
    return(createLayoutJSON(temp))
})

output$text3 <- renderText({
	if(is.null(dataOutput4())){return(NULL)}
		return("DONE!")	
		})


output$downloadLayoutJson <- downloadHandler(
    filename = function() { 
		 paste('layout_info.json') 
	 },
    content = function(file) {
    	writeLines(dataOutput4(), con=file)
    }
  )

output$layoutOptions <- renderRHandsontable({
    MAT = dataOptionsLayout()
       if (!is.null(MAT)) {
        return(rhandsontable(MAT, usesTypes=F, rowHeaders=1:length(MAT)) %>%
      hot_table(highlightCol = FALSE, highlightRow = TRUE,
            allowRowEdit = FALSE,
            columnSorting = FALSE,exportToCsv = TRUE) %>%
            hot_col(col="settings", readOnly=TRUE)
    )
    }
  })

output$layoutPin <- renderRHandsontable({
    MAT = dataPinLayout()
       if (!is.null(MAT)) {
      return(rhandsontable(MAT, rowHeaders=NULL) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE,
            columnSorting = FALSE, exportToCsv = TRUE) %>%
            hot_col(col = 1, readOnly = FALSE) %>%
            hot_context_menu(allowRowEdit=TRUE, allowColEdit=FALSE)
       )
    }
  })

versionTextInput <- reactive({
	version <- input$versionLayout
	return(version)
})
  

output$TextPin <- renderUI({
	if (!(is.null(dataOutput2()) & is.null(dataOutput3()))){
				includeMarkdown("rmarkdown/pincodes_instructions.Rmd")
	}
})

output$TextOptions <- renderUI({
	if (!(is.null(dataOutput2()) & is.null(dataOutput3()))){
		includeMarkdown("rmarkdown/options_instructions.Rmd")
}
})


###########################################
################4th tab
json.output.file.input <- reactive({
   if (is.null(input$json.output.file))
      return(NULL)
    readLines(input$json.output.file$datapath, warn=F)
  })
  
behaviors.json.input <- reactive({
    if (is.null(input$behaviors.json))
      return(NULL)
    readLines(input$behaviors.json$datapath, warn=F)
  })
layout_info.json.input <- reactive({
    if (is.null(input$layout_info.json))
      return(NULL)
    readLines(input$layout_info.json$datapath, warn=F)
  })

dataOutput <- reactive({
		if(is.null(json.output.file.input()) | is.null(behaviors.json.input()) | is.null(layout_info.json.input())) {return(NULL)} else 
		jsonOutputConversion(json.output.file.input(), behaviors.json.input(), layout_info.json.input(), colmerge=input$colmerge)
})
		
	output$sessionsTable.csv <- renderText({
	if(is.null(dataOutput())) return(NULL)
		"sessionsTable.csv"
		})
	output$focalsTable.csv <- renderText({
	if(is.null(dataOutput())) return(NULL)
		"focalsTable.csv"
		})
	output$behaviorsTable.csv <- renderText({
	if(is.null(dataOutput())) return(NULL)
		"behaviorsTable.csv"
		})
	output$scansTable.csv <- renderText({
	if(is.null(dataOutput())) return(NULL)
		"scansTable.csv"
		})
	output$backgroundTapsTable.csv <- renderText({
	if(is.null(dataOutput())) return(NULL)
		"backgroundTapsTable.csv"
		})
	output$commentsTable.csv <- renderText({
	if(is.null(dataOutput())) return(NULL)
		"commentsTable.csv"
		})
    output$dayVarsTable.csv <- renderText({
    	if(is.null(dataOutput())) return(NULL)
    		"dayVarsTable.csv"
    		})
    	output$focalVarsTable.csv <- renderText({
    if(is.null(dataOutput())) return(NULL)
   		"focalVarsTable.csv"
    	})
    output$continuousVarsTable.csv <- renderText({
   	if(is.null(dataOutput())) return(NULL)
   		"continuousVarsTable.csv"
    	})
    output$scanVarsTable.csv <- renderText({
   	if(is.null(dataOutput())) return(NULL)
   		"scanVarsTable.csv"
   		})
    		
	output$table1 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$sessionsTable
		}, include.rownames=F)
	
	output$table2 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$focalsTable
		}, include.rownames=F)

	output$table3 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$behaviorsTable
		}, include.rownames=F)

	output$table4 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$scansTable
		}, include.rownames=F)
		
	output$table5 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$backgroundTapsTable
		}, include.rownames=F)
		
	output$table6 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$commentsTable
		}, include.rownames=F)
		
	output$table7 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$dayVarsTable
		}, include.rownames=F)
		
	output$table8 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$focalVarsTable
		}, include.rownames=F)
		
	output$table9 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$continuousVarsTable
		}, include.rownames=F)

	output$table10 <- renderTable({
		if(is.null(dataOutput())) return(NULL)
		dataOutput()$scanVarsTable
		}, include.rownames=F)

	output$downloadSessionsTable <- downloadHandler(
    filename = function() { 
		 paste('sessionsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$sessionsTable, file, row.names=F)
    }
  )
	output$downloadFocalsTable <- downloadHandler(
    filename = function() { 
		 paste('focalsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$focalsTable, file, row.names=F)
    }
  )
	output$downloadBehaviorsTable <- downloadHandler(
    filename = function() { 
		 paste('behaviorsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$behaviorsTable, file, row.names=F)
    }
  )
	output$downloadScansTable <- downloadHandler(
    filename = function() { 
		 paste('scansTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$scansTable, file, row.names=F)
    }
  )
  	output$downloadBackgroundTapsTable <- downloadHandler(
    filename = function() { 
		 paste('backgroundTapsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$backgroundTapsTable, file, row.names=F)
    }
  )
   	output$downloadCommentsTable <- downloadHandler(
    filename = function() { 
		 paste('commentsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$commentsTable, file, row.names=F)
    }
  )
  output$downloadDayVarsTable <- downloadHandler(
    filename = function() { 
		 paste('dayVarsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$dayVarsTable, file, row.names=F)
    }
  )
  output$downloadFocalVarsTable <- downloadHandler(
    filename = function() { 
		 paste('focalVarsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$focalVarsTable, file, row.names=F)
    }
  )
  output$downloadContinuousVarsTable <- downloadHandler(
    filename = function() { 
		 paste('continuousVarsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$continuousVarsTable, file, row.names=F)
    }
  )
  output$downloadScanVarsTable <- downloadHandler(
    filename = function() { 
		 paste('scanVarsTable.csv', sep='') 
	 },
    content = function(file) {
     write.csv(dataOutput()$scanVarsTable, file, row.names=F)
    }
  )
  
  output$downloadZip <- downloadHandler(
       filename = function() {
         paste("AO_OutPut_", Sys.time(), ".zip", sep="")
       },
       content = function(fname) {
         fs <- c()
         tmpdir <- tempdir()
         initwd <- getwd()
         setwd(tempdir())
         
         for (i in 1:length(dataOutput())){
         	#cat(file=stderr(), paste0(names(dataOutput())[i], ".csv"))
         	write.csv(dataOutput()[[i]], file=paste0(names(dataOutput())[i], ".csv"), row.names=F)
         	
         }
                  
         zip(zipfile=fname, files=paste0(names(dataOutput()), ".csv"))
       },
       contentType = "application/zip"
     )
     
observe({
	session_nb <- input$session_nb
	focal_nb <- input$focal_nb
	max_session_nb <- nrow(dataOutput()$sessionsTable)
	focalListTemp <- dataOutput()$focalsTable[as.character(dataOutput()$focalsTable$session_start_timeStamp)==as.character(dataOutput()$sessionsTable$session_start_timeStamp[session_nb]),]
	
	focalStartTime <- as.character(focalListTemp$focal_start_timeStamp[focal_nb])
	
	behavListTemp <- dataOutput()$behaviorsTable[as.character(dataOutput()$behaviorsTable$focal_start_timeStamp)== focalStartTime,]
	 focal_ID <- as.character(focalListTemp$focal_individual_ID[focal_nb])

#cat(file=stderr(), paste("focalstarttime =", as.character(focalListTemp$focal_start_timeStamp[focal_nb])))
	
 	 updateSliderInput(session, "session_nb",
      label = paste("Session number:", session_nb), max= max_session_nb, min=1
      )
 
    updateSliderInput(session, "focal_nb",
      label = paste("Focal number:", focal_nb), max=nrow(focalListTemp),min=1
      )
     

    output$networkTitle <- renderUI({
    	if (is.null(focalStartTime) ) return(NULL) else if (length(focalStartTime)==0) return(NULL) else {
    		HTML(paste0("<h4 align='center'>Focal Individual: ", focal_ID, "<br/>Focal start time: ", focalStartTime, "</h4>"))
    		}
    })


    output$network_behav <- renderVisNetwork({
    
    	if (is.null(behavListTemp) ) return(NULL) else if (nrow(behavListTemp)==0) return(NULL) else {
    ids <- 	unique(c(focal_ID , as.character(behavListTemp$actor), as.character(behavListTemp$subject)))
    focal_index <- which(ids==focal_ID)
    behavListTemp$actor <- factor(behavListTemp$actor, levels=ids)
    behavListTemp$subject <- factor(behavListTemp$subject, levels=ids)
    colorNodes <- rep(colors()[589], length(ids))
    colorNodes[focal_index] <- colors()[76]
    nodes <- data.frame(id = 1:length(ids), label=ids, shape="box", shadow=T)
    
    lowbound <- which(names(behavListTemp)=="subject")
    upperbound <- which(names(behavListTemp)=="comment")
     modifiers <- behavListTemp[,(lowbound+1):(upperbound-1)]
    edgeLabels <- apply(modifiers,1,function(v) {
    	v1 <- v[v!=""]
	return(v1[1])
    }
    )
    titleEdges <- apply(modifiers,1,function(v) {
    	v1 <- v[v!=""]
	return(paste(v1, collapse="/"))
    }
    )
    edges <- data.frame(from = as.numeric(behavListTemp$actor), to = as.numeric(behavListTemp$subject), arrows="to", label= edgeLabels, title=titleEdges)
    visNetwork(nodes, edges) %>% visPhysics(solver = "repulsion", repulsion=list(nodeDistance=400))
    }
  }
  )  
    })

######################################
###################postgres connection

DBname <- reactive({
	return(input$postgresDBname)
})

DBuser <- reactive({
	return(input$postgresUser)
})

DBhost <- reactive({
	return(input$postgresHost)
})

DBpwd <- reactive({
	return(input$postgresPwd)
})

DBport <- reactive({
	return(input$postgresPort)
})

database <- eventReactive(input$postgresConnect, {
	#cat(file=stderr(), paste(DBname(), DBhost(), DBport(), DBuser(), DBpwd(), collapse=", "))
	if(is.null(DBname()) | is.null(DBuser()) | is.null(DBhost()) | is.null(DBpwd()) | is.null(DBport())) return(NULL)
    
    drv <- dbDriver("PostgreSQL")
    	all_cons <- dbListConnections(drv)
    for(con in all_cons) dbDisconnect(con)

    con <- dbConnect(dbDriver("PostgreSQL"), dbname = DBname(),
                 host = DBhost(), port = DBport(),
                 user = DBuser(), password = DBpwd())
#cat(file=stderr(), paste(DBname(), DBhost(), DBport(),DBuser(), DBpwd(), collapse=", "))
    return(con)
})  		

output$DoneConnect <- renderText({
	if(is.null(database())){return(NULL)}
		return("SUCCESS!")	
		})
		
output$postgresDBnameOutput <- renderUI({
    	if(is.null(database())) return(NULL)
		HTML(paste0("<h4 align='left'>Content of database <b><em>", DBname(),"</b></em></h4>"))
    	}
    )


 
output$table11 <- renderTable({
		if(is.null(database())) return(NULL)
				#cat(file=stderr(), "test")
				ans <- getTableList(database(), DBname())
all_cons <- dbListConnections(dbDriver("PostgreSQL"))
    for(con in all_cons) dbDisconnect(con)
		return(ans)
		}, include.rownames=F)
  


############################################
behaviors.json.input2 <- reactive({
    if (is.null(input$behaviors.json2))
      return(NULL)
      #cat(file=stderr(), "behaviors.json.input2")
    readLines(input$behaviors.json2$datapath, warn=F)
  })
layout_info.json.input2 <- reactive({
    if (is.null(input$layout_info.json2))
      return(NULL)
    readLines(input$layout_info.json2$datapath, warn=F)
  })


newDBname <- reactive({
	if(input$newDBname=="") return(NULL)
	return(input$newDBname)
})


createDB <- eventReactive(input$createEmptyDB, {
	#cat(file=stderr(), "test")	
	if(is.null(behaviors.json.input2()) | is.null(layout_info.json.input2()) | is.null(newDBname())) {
		return(NULL)
		}
	 cat(file=stderr(), "Creating empty database...\n")
    #listTables1 <- jsonOutputConversion(json.output.file =NULL, behaviors.json.input2(), layout_info.json.input2(), colmerge=input$colmerge2)
    con <- dbConnect(dbDriver("PostgreSQL"), dbname = DBname(),
                 host = DBhost(), port = DBport(),
                 user = DBuser(), password = DBpwd())
ans <- createListSQLTables(behav = behaviors.json.input2(), layout=layout_info.json.input2(), colmerge=input$colmerge2, con=con, newdbname= newDBname(), username= DBuser(), hostname= DBhost(), pwd= DBpwd())
all_cons <- dbListConnections(dbDriver("PostgreSQL"))
    for(con in all_cons) dbDisconnect(con)
    return(ans)
})  


output$newDBcreated <- renderText({
	if(is.null(createDB())){return(NULL)}
	updateTextInput(session, "postgresDBname",
      value = newDBname())
        output$table11 <- renderTable({
				#cat(file=stderr(), "test")
		con <- dbConnect(drv=dbDriver("PostgreSQL"), dbname = DBname(),
                 host = DBhost(), port = DBport(),
                 user = DBuser(), password = DBpwd())
		return(getTableList(con, DBname()))
		}, include.rownames=F)
		all_cons <- dbListConnections(dbDriver("PostgreSQL"))
    for(con in all_cons) dbDisconnect(con)
		return("SUCCESS!")	
})


############################################

outputblah <- eventReactive(input$runDbUpload6, {
   #cat(file=stderr(), "uploading dat file1")
  #if (is.null(dataOutput5())) return(NULL)
return("SUCCESS")
})


json.output.file.input3 <- reactive({
   if (is.null(input$datafile.dat))
      return(NULL)
     #cat(file=stderr(), "json.output.file.input3")
    readLines(input$datafile.dat$datapath, warn=F)
})

behaviors.json.input3 <- reactive({
    if (is.null(input$behaviors.json3))
      return(NULL)
    readLines(input$behaviors.json3$datapath, warn=F)
  })
  
layout_info.json.input3 <- reactive({
    if (is.null(input$layout_info.json3))
      return(NULL)
    readLines(input$layout_info.json3$datapath, warn=F)
  })

dataOutput5 <- reactive({
			   #cat(file=stderr(), "jsonOutputConversion1")

		if(is.null(json.output.file.input3()) | is.null(behaviors.json.input3()) | is.null(layout_info.json.input3()) | is.null(outputblah())) {return(NULL)} else 
		   #cat(file=stderr(), "jsonOutputConversion")
		
	outputTables <- jsonOutputConversion(json.output.file.input3(), behaviors.json.input3(), layout_info.json.input3(), colmerge=input$colmerge3)
    cat(file=stderr(), "Uploading file...\n")
	con <- dbConnect(drv=dbDriver("PostgreSQL"), dbname = DBname(),
           host = DBhost(), port = DBport(),
           user = DBuser(), password = DBpwd())
if(nrow(outputTables$sessionsTable)>0) uploadSessionsTable(outputTables$sessionsTable, con)
if(nrow(outputTables$focalsTable)>0) uploadFocalsTable(outputTables$focalsTable, con)
if(nrow(outputTables$behaviorsTable)>0) uploadBehaviorsTable(outputTables$behaviorsTable, con)
if(nrow(outputTables$scansTable)>0) uploadScansTable(outputTables$scansTable, con)
if(nrow(outputTables$scansTable)>0) uploadScanData(outputTables$scansTable, con)
if(nrow(outputTables$scanVarsTable)>0) uploadScanVariables(outputTables$scanVarsTable, con)
if("continuous_focal_variables" %in% dbListTables(con) & nrow(outputTables$continuousVarsTable)>0) {
uploadContinuousVariables(outputTables$continuousVarsTable, con)
}
if(nrow(outputTables$scanVarsTable)>0) uploadFocalVariables(outputTables$focalVarsTable, con)
if(nrow(outputTables$dayVarsTable)>0) uploadSessionVariables(outputTables$dayVarsTable, con)
if(nrow(outputTables$backgroundTapsTable)>0) uploadBackgroundTapsTable(outputTables$backgroundTapsTable, con)
if(nrow(outputTables$commentsTable)>0) uploadCommentTable(outputTables$commentsTable, con)

cat(file=stderr(), "File uploaded!\n")
return("SUCCESS")
})

output$dataUploaded <- renderText({
	if(is.null(dataOutput5())){return(NULL)}
	all_cons <- dbListConnections(dbDriver("PostgreSQL"))
    for(con in all_cons) dbDisconnect(con)
		return("SUCCESS!")	
})
})

# con <- dbConnect(drv=dbDriver("PostgreSQL"), dbname = "animal_observer", host = "localhost", port = 5432, user = "postgres", password = "postgres")
# dbConnect(drv=dbDriver("PostgreSQL"), dbname = "postgres", host = "localhost", port = 5432, user = "postgres", password = "postgres")
# dbGetQuery(con, "select *  from pg_tables where schemaname!='pg_catalog' AND schemaname!='information_schema';")##table list
# dbGetQuery(con, "select count(*) from information_schema.columns where table_name='list_food_items';")##number of columns
# dbGetQuery(con, "select column_name from information_schema.columns where table_name='list_food_items';")[,1]##column names
# dbGetQuery(con, "SELECT schemaname,relname,n_live_tup FROM pg_stat_user_tables ORDER BY n_live_tup DESC;")##number of rows, approximate
# dbGetQuery(con, "SELECT * FROM main_tables.list_food_items;")



