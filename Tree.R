  # This tree_df function 
    # Rendering the ui elements to select the node to subset,
    # how far back to subset, and tree options (text size, height, width)
    output$select_node_render <- renderUI({
      output <- tagList(
        # fluidRow(
        #   column(
        #     12,
        #     selectizeInput(
        #       inputId = "select_nodelabel",
        #       label = "Select Nodelabel:",
        #       choices = tree_df() %>%
        #         select(label) %>%
        #         arrange(label) %>%
        #         pull(label),#same as tree_df$label
        #       width = "100%"
        #     )
        #   ) 
        # ),
        fluidRow(
          column(
            12,
            checkboxInput(
              inputId = "tip_labels",
              label= "Tip label"
            ))),
        fluidRow(
          column(
            12,
            checkboxInput(
              inputId = "node_shapes",
              label= "Node shapes"
            ))),
        fluidRow(
          column(
            12,
            checkboxInput(
              inputId = "ancestral_states",
              label= "Ancestral states"
            ))),
        fluidRow(
          column(
            12,
            numericInput(
              inputId = "clade",
              label= "Subtree - Internal Node number:",
              value= length(getTipLabels(tree))+1
            ))),
        fluidRow(
          column(
            12,
            selectizeInput(
              inputId = "select_layout",
              label = "Select Layout:",
              choices = c("rectangular", "slanted"),
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(
            12,
            numericInput(
              inputId = "tree_text_size",
              label = "Select label text size:",
              min = 2,
              value = 3
            )
          )
          ),
        fluidRow(
          column(
            12,
            numericInput(
              inputId = "tree_plot_height",
              label = "Select plot height",
              value = 1200
            )
          )
          ),
        fluidRow(
          column(
            12,
            numericInput(
              inputId = "tree_width_multiply",
              label = "Select plot width multiplier:",
              value = 1.4,
              min = 1,
              step = 0.1
            )
          )
        )
      )

      return(output)
    })

    # creating the tree
    output$plotly_tree <- renderPlotly({
      req(input$tree_width_multiply,
          input$tree_text_size,
          input$tree_plot_height)

      # creating the plot
      #aes_string(y = colnames(keep_high)[1], x ="value", group = "Predictors", key="Key")
      p <- tree %>%
        ggtree(layout = input$select_layout)
      if(input$node_shapes) p <- p + geom_point(aes(node=node, parent=parent, label = states, label2=label))
      if(!input$node_shapes) p <- p + geom_point(aes(node=node, parent=parent, label = states, label2=label), alpha=0)
      p<- p+ theme_tree2() +
        scale_color_manual(values = c(`1` = "red", `0` = "black"))

      p1<- p + lims(x = c(0, max(p$data$x) * input$tree_width_multiply))
      ggplotly(p1, tooltip =  c("node", "parent", "label2", "label"), height = input$tree_plot_height, source="tree")
    })
    
    output$plot_tree <- renderPlot({
      req(input$tree_width_multiply,
          input$tree_text_size,
          input$tree_plot_height)
      
      # creating the plot
      #aes_string(y = colnames(keep_high)[1], x ="value", group = "Predictors", key="Key")
      p <- tree %>%
        ggtree(layout = input$select_layout)
      if(input$node_shapes) p <- p + geom_point(aes(label = states, label2=label))
      if(input$tip_labels)  p <- p + geom_tiplab(size = input$tree_text_size)
      if(input$ancestral_states) p <- p + geom_text(aes(x=branch, label=states))
      p <-viewClade(p, input$clade)
      p <- p +theme_tree2() +
        scale_color_manual(values = c(`1` = "red", `0` = "black"))
      p1<- p + lims(x = c(0, max(p$data$x) * input$tree_width_multiply))
      p1
    })
    
    

