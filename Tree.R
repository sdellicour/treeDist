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
              inputId = "colour_by_states",
              label= "Colour by States"
            ))),
        fluidRow(
          column(
            12,
            numericInput(
              inputId = "tree_plot_height",
              label = "Select plot height",
              value = 1000
            )
          )),
        fluidRow(
          column(
            12,
            checkboxInput(
              inputId = "tip_labels",
              label= "Annotation tree - Tip label"
            ))),
        fluidRow(
          column(
            12,
            numericInput(
              inputId = "tree_text_size",
              label = "Annotation tree - Tip label size:",
              min = 0,
              value = 3
            ))),
        fluidRow(
          column(
            12,
            numericInput(
              inputId = "node",
              label= "Annotation tree - Internal Node number:",
              value= length(getTipLabels(tree))+1
            ))),        
        fluidRow(
              column(
                12,
                selectizeInput(
                  inputId = "select_layout",
                  label = "Annotation tree - Select Layout:",
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
                ))),
        fluidRow(
          column(
            12,
            checkboxInput(
              inputId = "node_number",
              label= "Annotation tree - Node number"
            ))),
        fluidRow(
          column(
            12,
            numericInput(
              inputId = "node_number_size",
              label= "Annotation tree - Node number size",
              value = 3
            ))),
        fluidRow(
          column(
            12,
            checkboxInput(
              inputId = "tip_shapes",
              label= "Annotation tree - Tip shapes"
            ))),
        fluidRow(
          column(
            12,
            checkboxInput(
              inputId = "node_shapes",
              label= "Annotation tree - Node shapes"
            ))),
        fluidRow(
          column(
            12,
            checkboxInput(
              inputId = "ancestral_states",
              label= "Annotation tree - Ancestral states"
            ))),
        fluidRow(
          column(
            12,
            numericInput(
              inputId = "ancestral_states_size",
              label= "Annotation tree -  Ancestral states Size",
              value =3
            ))),
        fluidRow(
          column(
            12,
            numericInput(
              inputId = "annotation_plot_legend_size",
              label= "Annotation tree -  Legend text Size",
              value =15
            ))),
        )
      return(output)
    })

    # creating the tree
    output$plotly_tree <- renderPlotly({
      req(input$tree_text_size,
          input$tree_plot_height)
      # creating the plot
      #aes_string(y = colnames(keep_high)[1], x ="value", group = "Predictors", key="Key")
      p <- tree %>%
        ggtree(aes(label = get(state, tree), label2=label))
      tree <- tree %>%
        fortify() %>% 
        as_tibble()
      if(input$colour_by_states) {
        nbColours<-length(unique(unlist(lapply(get(state, tree), first.word))))#the states are added as a list in order to unlist them I need to take only the first word otherwise we get too many states
        getPalette = colorRampPalette(brewer.pal(9, "Set1"))#these 9 colours will be interpolated to obtain  the most divergent result
        p <- p + geom_tree(aes(color=unlist(lapply(get(state, tree), first.word))))+
          scale_colour_manual(values=getPalette(nbColours))
        }

      ggplotly(p, tooltip =  c("node", "parent", "label2", "label"), source="tree") %>%
        layout(legend = list(orientation = "h" , y=-0.25, title=get(state, tree)))
    })
    
    output$plotly_ui <- renderUI({
      plotlyOutput("plotly_tree", height = input$tree_plot_height)
    })
    
    output$plot_tree <- renderPlot({
      req(input$tree_text_size,
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
      if(input$node_shapes) p <- p + geom_nodepoint(aes(label = get(state, subtree2), label2=label))
      if(input$tip_shapes) p <- p + geom_tippoint(aes(label = get(state, subtree2) , label2=label))
      if(input$tip_labels)  p <- p + geom_tiplab(size = input$tree_text_size)
      if(input$ancestral_states) p <- p + geom_text(aes(x=branch, label= get(state, subtree2), vjust =-0.5), size= input$ancestral_states_size )
      if(input$node_number) p <- p +    geom_label(mapping = aes(label = node), size = input$node_number_size)
      if(input$colour_by_states) {
        nbColours<-length(unique(unlist(lapply(get(state, subtree2), first.word))))#the states are added as a list in order to unlist them I need to take only the first word otherwise we get too many states
        getPalette = colorRampPalette(brewer.pal(9, "Set1"))#these 9 colours will be interpolated to obtain  the most divergent result
        p <- p + geom_tree(aes(color=unlist(lapply(get(state, subtree2), first.word))))+
          scale_colour_manual(values=getPalette(nbColours))+
          theme(legend.position="bottom", legend.text=element_text(size= input$annotation_plot_legend_size), legend.title = element_blank())
      }
      p
    })
    
    output$plot_ui <- renderUI({
      req(input$tree_text_size,
          input$tree_plot_height)
      plotOutput("plot_tree", height = input$tree_plot_height)
    })