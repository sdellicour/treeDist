  # This tree_df function 
    tree_df <- reactive({
      output <- tree %>% 
        as_tibble()
    })
    
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
    output$tree <- renderPlotly({
      req(input$tree_width_multiply,
          input$tree_text_size,
          input$tree_plot_height)


      # extracting the tip labels from the sub tree
      if (isS4(tree)) {
        tip_labels <- tree@phylo$tip.label
      } else {
        tip_labels <- tree$tip.label
      }

      # doing some basic manipulation on labels
      # this will only really work for labels of the format
      # ;k__;p__;c__;o__;f__;g__;s__
      labels_df <- tibble(
        tip_label = tip_labels,
        genus = str_extract(tip_label, "[^;]+;[^;]+$") %>% str_replace(";[^;]+$", ""),
        species = str_extract(tip_labels, "[^;]+$")
      )  %>%
        mutate(
          species = if_else(is.na(genus), "", str_replace(species, "s__", "")),
          genus = if_else(is.na(genus), tip_label, str_replace(genus, "g__", ""))
        )

      # creating the plot
      #aes_string(y = colnames(keep_high)[1], x ="value", group = "Predictors", key="Key")
      p <- tree %>%
        ggtree(layout = input$select_layout)+
        geom_point(aes(label = states, label2=label))+
        #geom_tiplab(aes(label = paste(genus, species)),
         #           size = input$tree_text_size) +
        theme_tree2() +
        scale_color_manual(values = c(`1` = "red", `0` = "black"))

      p1<- p + lims(x = c(0, max(p$data$x) * input$tree_width_multiply))
      ggplotly(p1, tooltip =  c("label2", "label"), height = input$tree_plot_height, source="tree")
    })

