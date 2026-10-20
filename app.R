library(shiny)
library(DT)

# 1. UI Section (Paste content from your old ui.R here)
ui <- fluidPage(  tags$head(
    tags$script(src = "https://datatables.net")
  ),
                navbarPage("Finding Probability",
                           tabPanel("From Frequency",
                                    sidebarLayout(
                                      sidebarPanel(
                                        p(tags$b("Frequency Table")),
                                        p("Double click in the table cells to enter your data. Tab or double-click a new cell to save values."),
                                        DT::DTOutput("x1"),
                                      ),
                                      
                                      # Show plot
                                      mainPanel(
                                        h4("Directions"),
                                        p("Double-click in the cells in the table to the left to change the values to match your data. After entering a value, click Tab or double-click on a new cell to save the value. Values entered represent absolute frequency (count) and should be whole numbers."), 
                                        hr(),
                                        h4("Tables"),
                                        fluidRow(column(6,
                                                        h5("Frequency"),
                                                        tableOutput("contab")),
                                                 column(6,
                                                        h5("Relative Frequency"),
                                                        tableOutput("probtab"))),
                                        hr(),
                                        h4("Probability"),
                                        tableOutput("prob")
                                      )
                                    )),
                           tabPanel("From Relative Frequency",
                                    sidebarLayout(
                                      sidebarPanel(
                                        p(tags$b("Relative Frequency Table")),
                                        p("Double-click in the table cells to enter your data. Tab or double-click in a new to save values."),
                                        DT::DTOutput("x2")
                                      ),
                                      
                                      # Show plot
                                      mainPanel(
                                        h4("Directions"),
                                        p("Double-click in the cells in the table to the left to change the values to match your data. After entering a value, click Tab or double-click a new cell to save the value. Values entered represent relative frequency and should be between 0 and 1."),
                                        fluidRow(column(6,
                                                        h4("Relative Frequency"),
                                                        tableOutput("relcontab")),
                                                 column(6,
                                                        h4("Probability"),
                                                        tableOutput("probrel")))
                                      )
                                    )
                                    
                           )))


  
  # 2. Server Section (Paste content from your old server.R here)
callback <- c(
  "var colnames = table.columns().header().to$().map(function() {",
  "  return this.innerHTML;",
  "}).get();",
  "Shiny.onInputChange('colnames', colnames);",
  "table.on('dblclick.dt', 'thead th', function(e) {",
  "  var $th = $(this);",
  "  var index = $th.index();",
  "  var colname = $th.text(), newcolname = colname;",
  "  var $input = $('<input type=\"text\">')",
  "  $input.val(colname);",
  "  $th.empty().append($input);",
  "  $input.on('change', function() {",
  "    newcolname = $input.val();",
  "    if(newcolname != colname){",
  "      $(table.column(index).header()).text(newcolname);",
  "      colnames[index] = newcolname;",
  "      Shiny.onInputChange('colnames', colnames);",
  "    }",
  "    $input.remove();",
  "  }).on('blur', function() {",
  "    $(table.column(index).header()).text(newcolname);",
  "    $input.remove();",
  "  });",
  "});"
)

  server <- function(input, output){
    x = as.data.frame(matrix(data = c(1,1,1,1), 
                             nrow = 2, 
                             ncol = 2, 
                             byrow = TRUE, 
                             dimnames = list(c("A1", "A2"), c("B1", "B2"))))
    y <- reactiveValues(data = x)
    
    output$x1 <- renderDT(x, 
                          selection = "none", 
                          editable = list(target = "cell"), 
                          rownames = TRUE,
                          callback = JS(callback),
                          options = list(dom = 't', ordering = FALSE))
    
    proxy = dataTableProxy('x1')
    
    
    observeEvent(input$x1_cell_edit, {
      info = input$x1_cell_edit
      str(info)
      y$data <<- editData(y$data, info, rownames = TRUE)
      replaceData(proxy, y$data, resetPaging = FALSE)})
    
    # observeEvent(input$x1_cell_edit, {
    #    info = input$x1_cell_edit
    #    str(info)
    #    i = info$row
    #    j = info$col
    #    v = info$value
    #    x$data[i, j] <<- DT::coerceValue(v, x$data[i, j])
    #    replaceData(proxy, x, resetPaging = FALSE)  
    #  })
    
    
    
    
    output$contab <- renderTable({
      tab <- y$data
      tot_obs <- sum(tab)
      tot_rows <- rowSums(tab)
      tot_cols <- colSums(tab)
      tab[,"Total"] <- tot_rows
      tab["Total", ] <- c(tot_cols, tot_obs)
      colnames(tab) = c(input$colnames[2:3], "Total")
      tab
    }, rownames = TRUE, digits = 0)
    
    output$probtab <- renderTable({
      tab1 <- y$data
      tot_obs1 <- sum(tab1)
      prob_t <- tab1/tot_obs1
      tot_rows1 <- rowSums(tab1)/tot_obs1
      tot_cols1 <- colSums(tab1)/tot_obs1
      prob_t[,"Total"] <- tot_rows1
      prob_t["Total", ] <- c(tot_cols1, sum(prob_t[,"Total"]))
      colnames(prob_t) = c(input$colnames[2:3], "Total")
      prob_t
    }, rownames = TRUE, digits = 4)
    
    #render plot
    output$prob <- renderTable({
      tot_obs <- sum(y$data)
      pA1 <- sum(y$data[1,])/tot_obs
      pA2 <- sum(y$data[2,])/tot_obs
      pB1 <- sum(y$data[,1])/tot_obs
      pB2 <- sum(y$data[,2])/tot_obs
      pA1_AND_B1 <- y$data[1,1]/tot_obs
      pA1_AND_B2 <- y$data[1,2]/tot_obs
      pA2_AND_B1 <- y$data[2,1]/tot_obs
      pA2_AND_B2 <- y$data[2,2]/tot_obs
      pA1_OR_B1 <- pA1 + pB1 - pA1_AND_B1
      pA1_OR_B2 <- pA1 + pB2 - pA1_AND_B2
      pA2_OR_B1 <- pA2 + pB1 - pA2_AND_B1
      pA2_OR_B2 <- pA2 + pB2 - pA2_AND_B2
      pA1_giv_B1 <- pA1_AND_B1/pB1
      pA1_giv_B2 <- pA1_AND_B2/pB2
      pA2_giv_B1 <- pA2_AND_B1/pB1
      pA2_giv_B2 <- pA2_AND_B2/pB2
      pB1_giv_A1 <- pA1_AND_B1/pA1
      pB1_giv_A2 <- pA2_AND_B1/pA2
      pB2_giv_A1 <- pA1_AND_B2/pA1
      pB2_giv_A2 <- pA2_AND_B2/pA2
      A1 <- row.names(y$data)[1]
      A2 <- row.names(y$data)[2]
      B1 <- input$colnames[2]
      B2 <- input$colnames[3]
      prob <- c(paste0("P(", A1, ")"), 
                paste0("P(", A2, ")"),
                paste0("P(", B1, ")"),
                paste0("P(", B2, ")"),
                paste0("P(", A1, " AND ", B1, ")"),
                paste0("P(", A1, " AND ", B2, ")"),
                paste0("P(", A2, " AND ", B1, ")"),
                paste0("P(", A2, " AND ", B2, ")"),
                paste0("P(", A1, " OR ", B1, ")"),
                paste0("P(", A1, " OR ", B2, ")"),
                paste0("P(", A2, " OR ", B1, ")"),
                paste0("P(", A2, " OR ", B2, ")"),
                paste0("P(", A1, " | ", B1, ")"),
                paste0("P(", A1, " | ", B2, ")"),
                paste0("P(", A2, " | ", B1, ")"),
                paste0("P(", A2, " | ", B2, ")"),
                paste0("P(", B1, " | ", A1, ")"),
                paste0("P(", B1, " | ", A2, ")"),
                paste0("P(", B2, " | ", A1, ")"),
                paste0("P(", B2, " | ", A2, ")"))
      
      vals <- c(pA1, pA2, pB1, pB2,
                pA1_AND_B1, pA1_AND_B2,
                pA2_AND_B1, pA2_AND_B2,
                pA1_OR_B1, pA1_OR_B2,
                pA2_OR_B1, pA2_OR_B2,
                pA1_giv_B1, pA1_giv_B2,
                pA2_giv_B1, pA2_giv_B2,
                pB1_giv_A1, pB1_giv_A2,
                pB2_giv_A1, pB2_giv_A2)
      data.frame(Probability = prob, Value = vals)
    }, digits = 4)
    
    x_p = as.data.frame(matrix(data = c(0.25,0.25,0.25,0.25), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("A1", "A2"), c("B1", "B2"))))
    y_p <- reactiveValues(data = x_p)
    
    output$x2 <- renderDT(x_p, 
                          selection = "none", 
                          editable = list(target = "cell"), 
                          rownames = TRUE,
                          callback = JS(callback),
                          options = list(dom = 't', ordering = FALSE))
    
    proxy = dataTableProxy('x2')
    
    
    observeEvent(input$x2_cell_edit, {
      info = input$x2_cell_edit
      str(info)
      y_p$data <<- editData(y_p$data, info, rownames = TRUE)
      replaceData(proxy, y_p$data, resetPaging = FALSE)})
    #output$x2 = renderDT(y_p$data, selection = 'none', editable = TRUE, options = list(dom = 't'))
    
    #proxy_p = dataTableProxy('x2')
    
    #observeEvent(input$x2_cell_edit, {
    #  info_p = input$x2_cell_edit
    #  str(info_p)
    #  i_p = info_p$row
    #  j_p = info_p$col
    #  v_p = info_p$value
    #  y_p$data[i_p, j_p] <- DT::coerceValue(v_p, y_p$data[i_p, j_p])
    #  replaceData(proxy_p, y_p$data, resetPaging = FALSE)  
    #})
    
    output$relcontab <- renderTable({
      rtab <- y_p$data
      tot_obsr <- sum(rtab)
      tot_rowsr <- rowSums(rtab)
      tot_colsr <- colSums(rtab)
      rtab[,"Total"] <- tot_rowsr
      rtab["Total", ] <- c(tot_colsr, tot_obsr)
      colnames(rtab) = c(input$colnames[2:3], "Total")
      rtab
    }, rownames = TRUE, digits = 4)
    
    output$probrel <- renderTable({
      pA1p <- sum(y_p$data[1,])
      pA2p <- sum(y_p$data[2,])
      pB1p <- sum(y_p$data[,1])
      pB2p <- sum(y_p$data[,2])
      pA1_AND_B1p <- y_p$data[1,1]
      pA1_AND_B2p <- y_p$data[1,2]
      pA2_AND_B1p <- y_p$data[2,1]
      pA2_AND_B2p <- y_p$data[2,2]
      pA1_OR_B1p <- pA1p + pB1p - pA1_AND_B1p
      pA1_OR_B2p <- pA1p + pB2p - pA1_AND_B2p
      pA2_OR_B1p <- pA2p + pB1p - pA2_AND_B1p
      pA2_OR_B2p <- pA2p + pB2p - pA2_AND_B2p
      pA1_giv_B1p <- pA1_AND_B1p/pB1p
      pA1_giv_B2p <- pA1_AND_B2p/pB2p
      pA2_giv_B1p <- pA2_AND_B1p/pB1p
      pA2_giv_B2p <- pA2_AND_B2p/pB2p
      pB1_giv_A1p <- pA1_AND_B1p/pA1p
      pB1_giv_A2p <- pA2_AND_B1p/pA2p
      pB2_giv_A1p <- pA1_AND_B2p/pA1p
      pB2_giv_A2p <- pA2_AND_B2p/pA2p
      
      A1 <- row.names(y_p$data)[1]
      A2 <- row.names(y_p$data)[2]
      B1 <- input$colnames[2]
      B2 <- input$colnames[3]
      probp <- c(paste0("P(", A1, ")"), 
                 paste0("P(", A2, ")"),
                 paste0("P(", B1, ")"),
                 paste0("P(", B2, ")"),
                 paste0("P(", A1, " AND ", B1, ")"),
                 paste0("P(", A1, " AND ", B2, ")"),
                 paste0("P(", A2, " AND ", B1, ")"),
                 paste0("P(", A2, " AND ", B2, ")"),
                 paste0("P(", A1, " OR ", B1, ")"),
                 paste0("P(", A1, " OR ", B2, ")"),
                 paste0("P(", A2, " OR ", B1, ")"),
                 paste0("P(", A2, " OR ", B2, ")"),
                 paste0("P(", A1, " | ", B1, ")"),
                 paste0("P(", A1, " | ", B2, ")"),
                 paste0("P(", A2, " | ", B1, ")"),
                 paste0("P(", A2, " | ", B2, ")"),
                 paste0("P(", B1, " | ", A1, ")"),
                 paste0("P(", B1, " | ", A2, ")"),
                 paste0("P(", B2, " | ", A1, ")"),
                 paste0("P(", B2, " | ", A2, ")"))
      valsp <- c(pA1p, pA2p, pB1p, pB2p,
                 pA1_AND_B1p, pA1_AND_B2p,
                 pA2_AND_B1p, pA2_AND_B2p,
                 pA1_OR_B1p, pA1_OR_B2p,
                 pA2_OR_B1p, pA2_OR_B2p,
                 pA1_giv_B1p, pA1_giv_B2p,
                 pA2_giv_B1p, pA2_giv_B2p,
                 pB1_giv_A1p, pB1_giv_A2p,
                 pB2_giv_A1p, pB2_giv_A2p)
      data.frame(Probability = probp, Value = valsp)
    }, digits = 4)
    
  }
  
    
    # 3. App Launch (This binds the UI and Server together)
    shinyApp(ui = ui, server = server)
