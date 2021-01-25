library(RColorBrewer)
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
              inputId = "colour_by_states",
              label= "Colour by States"
            ))),
        fluidRow(
          column(
            12,
            checkboxInput(
              inputId = "node_number",
              label= "Node number"
            ))),
        fluidRow(
          column(
            12,
            checkboxInput(
              inputId = "tip_shapes",
              label= "Tip shapes"
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
              inputId = "node",
              label= "Subtree - Internal Node number:",
              value= length(getTipLabels(tree))+1
            ))),
        fluidRow(
          column(
            12,
            selectizeInput(
              inputId = "select_layout",
              label = "Select Layout:",
              choices = c("rectangular", 
                          "slanted",
                          "fan", 
                          "circular", 
                          "radial", 
                          "unrooted", 
                          "equal_angle", 
                          "daylight"
                          ),
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
              value = 1000
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
        ggtree(aes(label = states, label2=label))
      tree <- tree %>%
        fortify() %>% 
        as_tibble()
      if(input$colour_by_states) {
        nbColours<-length(unique(unlist(lapply(tree$states, first.word))))#the states are added as a list in order to unlist them I need to take only the first word otherwise we get too many states
        getPalette = colorRampPalette(brewer.pal(9, "Set1"))#these 9 colours will be interpolated to obtain  the most divergent result
        p <- p + geom_tree(aes(color=unlist(lapply(tree$states, first.word))), yscale = "states")+
          scale_fill_manual(values=getPalette(nbColours))
        }

      ggplotly(p, tooltip =  c("node", "parent", "label2", "label"), source="tree") %>%
        layout(legend = list(
          orientation = "h" , y=-0.01))
    })
    
    output$plotly_ui <- renderUI({
      plotlyOutput("plotly_tree", height = input$tree_plot_height)
    })
    
    output$plot_tree <- renderPlot({
      req(input$tree_width_multiply,
          input$tree_text_size,
          input$tree_plot_height)
      
      # creating the plot
      subtree <- tree %>%
        fortify() %>% 
        offspring(.node=input$node, include_self=TRUE)
      
      subtree2 <- tree %>%
        fortify() %>% 
        as_tibble() %>%
        filter(node %in% subtree$node)
     
      p<- subtree2 %>% ggtree(layout=input$select_layout) 
      if(input$node_shapes) p <- p + geom_nodepoint(aes(label = states, label2=label))
      if(input$tip_shapes) p <- p + geom_tippoint(aes(label = states, label2=label))
      if(input$tip_labels)  p <- p + geom_tiplab(size = input$tree_text_size)
      if(input$ancestral_states) p <- p + geom_text(aes(x=branch, label=states))
      if(input$node_number) p <- p +    geom_label(mapping = aes(label = node), size = 4)
      if(input$colour_by_states) {
        nbColours<-length(unique(unlist(lapply(subtree$states, first.word))))#the states are added as a list in order to unlist them I need to take only the first word otherwise we get too many states
        getPalette = colorRampPalette(brewer.pal(9, "Set1"))#these 9 colours will be interpolated to obtain  the most divergent result
        p <- p + geom_tree(aes(color=unlist(lapply(subtree$states, first.word))), yscale = "states")+
          scale_fill_manual(values=getPalette(nbColours))+
          theme(legend.position="bottom")
      }
      p <- p
      p
    })
    
    output$plot_ui <- renderUI({
      req(input$tree_width_multiply,
          input$tree_text_size,
          input$tree_plot_height)
      plotOutput("plot_tree", height = input$tree_plot_height)
    })
    
    
    
    

