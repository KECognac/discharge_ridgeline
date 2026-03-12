library(shiny)
library(bslib)
library(tidyverse)
library(ggridges)
library(viridis)
library(leaflet)
library(showtext)
library(dataRetrieval)
library(sf)
library(data.table)
library(DT)

# Loading fonts
font_add_google("DM Serif Display", "abril")
font_add_google("Tajawal", "tawa")
showtext_auto()

# Set theme
theme_set(theme_light())

# Function to make ridgeline plot
make_plot <- function(site_no, large_plot = FALSE) {
  # Validate input
  if (is.null(site_no) || site_no == "") {
    return(NULL)
  }
  
  tryCatch({
    # Get site info
    site_info <- dataRetrieval::readNWISsite(site_no)
    
    # Download discharge data
    dis <- dataRetrieval::readNWISdv(siteNumbers = site_no,
                                     parameterCd = '00060') %>% 
      data.table() %>%
      dplyr::rename("discharge" = "X_00060_00003",
                    "date" = "Date") %>%
      dplyr::select(site_no, date, discharge) %>%
      dplyr::mutate(year = dataRetrieval::calcWaterYear(date),
                    year_char = as.factor(as.character(year)),
                    wy_doy = as.integer(date - as_date(paste0(year_char, "-10-01"))) + 1,
                    #doy_date = as.Date(paste(2024, yday(date), sep = "-"), format = "%Y-%j")) %>%
                    doy_date = as.Date("2000-10-01") + (wy_doy - 1)) %>%
      filter(!is.na(discharge))
    
    if (nrow(dis) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No discharge data available for this site") +
               theme_void())
    }
    
    # Adjust sizes based on whether it's a large plot or not
    title_size <- if (large_plot) 12 else 12
    axis_text_size <- if (large_plot) 12 else 12
    
    # Plot discharge
    ggplot(dis, aes(x = doy_date,
                    y = fct_reorder(year_char, year),
                    height = discharge / quantile(dis$discharge, 0.98, na.rm = TRUE),
                    group = year)) +
      geom_ridgeline(alpha = 0.7, scale = 1, linewidth = 0.1, fill = "#154360", color = "white") +
      labs(y = "Daily River Flow by Water Year", x = "", title = site_info$station_nm) +
      scale_x_date(date_labels = "%b", date_breaks = "1 month") +
      theme(
        plot.title = element_text(hjust = 0, size = 12),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0, unit = "pt")),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5),
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0, hjust=0)
      )
  }, error = function(e) {
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = paste("Error loading data:", e$message)) +
      theme_void()
  })
}


# UI
ui <- page_fillable(
  title = "Make a Streamflow Ridgeline Plot",
  
  # State selection at the top
  card(
    card_header("Select State"),
    selectInput("state", "Choose a state to view USGS gage sites:",
                choices = c("Select a state" = "", sort(c(state.abb, "DC"))),
                selected = "",
                width = "100%",
                selectize = FALSE,
                size = 3
    )
  ),
  
  # Conditional panel for site selection and results
  conditionalPanel(
    condition = "input.state != ''",
    
    
    # Map and plot
    layout_columns(
      col_widths = c(12, 12),
      # On mobile, these will stack vertically
      card(
        card_header("USGS Flow Gauges - Click on map to select a site"),
        leafletOutput("map", height = "400px")
      ),
      card(
        card_header(
          "Streamflow Ridgeline Plot",
          # Add button to open plot in new window
          conditionalPanel(
            condition = "output.plot_available",
            actionButton("open_plot_modal", "Open in New Window", 
                         class = "btn-outline-primary btn-sm",
                         style = "float: right; margin-top: -5px;")
          )
        ),
        plotOutput("ridgeline_plot", height = "500px")
      )
    )
  )
)


# Server
server <- function(input, output, session) {
  # Reactive value to store selected site
  selected_site <- reactiveVal(NULL)
  
  # Output to control button visibility
  output$plot_available <- reactive({
    !is.null(selected_site())
  })
  outputOptions(output, "plot_available", suspendWhenHidden = FALSE)
  
  # Reactive expression to get NWIS sites for selected state
  nwis_sites <- reactive({
    req(input$state)
    
    withProgress(message = "Loading USGS sites... (takes up to 20s)", value = 0, {
      tryCatch({
        sites <- dataRetrieval::whatNWISdata( #dataRetrieval::whatNWISsites(
          stateCd = input$state,
          parameterCd = "00060",
          siteType = "ST",
          service = "site"
          #hasDataTypeCd = "dv"
        ) %>%
          filter(!is.na(dec_lat_va), !is.na(dec_long_va), count_nu > 500) %>%
          st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4326)
        
        incProgress(1)
        sites
      }, error = function(e) {
        showNotification(paste("Error loading sites:", e$message), type = "error")
        NULL
      })
    })
  })
  
  # Render map
  output$map <- renderLeaflet({
    if (is.null(nwis_sites()) || nrow(nwis_sites()) == 0) {
      leaflet() %>%
        addTiles() %>%
        setView(lng = -98, lat = 39, zoom = 4)
    } else {
      sites_df <- nwis_sites() %>%
        mutate(
          lng = st_coordinates(.)[,1],
          lat = st_coordinates(.)[,2]
        ) %>%
        st_drop_geometry()
      
      leaflet(sites_df) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~lng, lat = ~lat,
          radius = 5,
          popup = ~paste0("<strong>", station_nm, "</strong><br>",
                          "Site No: ", site_no, "<br>"),
          layerId = ~site_no,
          stroke = TRUE,
          fillOpacity = 0.7,
          color = "#154360"
        ) %>%
        fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat))
    }
  })
  
  # Handle map clicks
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (!is.null(click)) {
      selected_site(click$id)
    }
  })
  
  
  # Handle table selection
  observeEvent(input$site_table_rows_selected, {
    req(input$site_table_rows_selected, nwis_sites())
    row_selected <- input$site_table_rows_selected
    site_data <- nwis_sites() %>% st_drop_geometry()
    selected_site(site_data$site_no[row_selected])
  })
  
  # Render ridgeline plot
  output$ridgeline_plot <- renderPlot({
    if (is.null(selected_site())) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Select a site from the map or table to view discharge data",
                 size = 5) +
        theme_void()
    } else {
      withProgress(message = "Loading discharge data...", value = 0, {
        plot_result <- make_plot(selected_site())
        incProgress(1)
        plot_result
      })
    }
  })
  
  # Handle opening plot in modal with export buttons
  observeEvent(input$open_plot_modal, {
    req(selected_site())
    
    showModal(modalDialog(
      title = "Streamflow Ridgeline Plot",
      plotOutput("ridgeline_plot_large", height = "1000px"),
      footer = div(
        downloadButton("download_tiff", "Download TIFF", class = "btn-primary", style = "margin-right: 10px;"),
        downloadButton("download_pdf", "Download PDF", class = "btn-primary", style = "margin-right: 10px;"),
        modalButton("Close")
      ),
      size = "xl",
      easyClose = TRUE
    ))
  })
  
  # Render large ridgeline plot for modal
  output$ridgeline_plot_large <- renderPlot({
    req(selected_site())
    withProgress(message = "Loading discharge data...", value = 0, {
      plot_result <- make_plot(selected_site(), large_plot = TRUE)
      incProgress(1)
      plot_result
    })
  })
  
  # Download handler for TIFF
  output$download_tiff <- downloadHandler(
    filename = function() {
      req(selected_site())
      site_info <- dataRetrieval::readNWISsite(selected_site())
      safe_name <- gsub("[^A-Za-z0-9_-]", "_", site_info$station_nm)
      paste0(safe_name, "_", selected_site(), "_ridgeline.tiff")
    },
    content = function(file) {
      req(selected_site())
      
      # Create the plot
      plot_obj <- make_plot(selected_site(), large_plot = TRUE)
      
      # Save as TIFF with high quality settings
      tiff(file, width = 8.5, height = 11, units = "in", res = 300, compression = "lzw")
      print(plot_obj)
      dev.off()
    }
  )
  
  # Download handler for PDF
  output$download_pdf <- downloadHandler(
    filename = function() {
      req(selected_site())
      site_info <- dataRetrieval::readNWISsite(selected_site())
      safe_name <- gsub("[^A-Za-z0-9_-]", "_", site_info$station_nm)
      paste0(safe_name, "_", selected_site(), "_ridgeline.pdf")
    },
    content = function(file) {
      req(selected_site())
      
      # Create the plot
      plot_obj <- make_plot(selected_site(), large_plot = TRUE)
      
      # Save as PDF
      pdf(file, width = 8.5, height = 11)
      print(plot_obj)
      dev.off()
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)