#' Maximize button for plot
#'
#' @param plot_id ID of plot output to maximize
#' @param ns_full
#' @return A shiny tag containing the maximize button
create_maximize_button <- function(plot_id, ns_full) {
  button_id <- paste0(plot_id, "_maximize")
  
  shiny::tags$button(
    id = ns_full(button_id),
    class = "btn btn-sm btn-light plot-maximize-btn",
    title = "Maximize plot",
    onclick = paste0("maximizePlot('", ns_full(plot_id), "', '", ns_full(button_id), "');"),
    shiny::icon("expand")
  )
}

#' Modal container for plots
#'
#' @param ns_full
#' @return A shiny tag containing the modal container
create_plot_modal <- function(ns_full) {
  shiny::tags$div(
    id = ns_full("plot_modal_container"),
    class = "modal fade",
    tabindex = "-1",
    role = "dialog",
    `aria-hidden` = "true",
    
    shiny::tags$div(
      class = "modal-dialog modal-lg modal-dialog-centered",
      style = "max-width: 90%; width: 90%;",
      
      shiny::tags$div(
        class = "modal-content",
        
        # Modal header
        shiny::tags$div(
          class = "modal-header",
          shiny::tags$h5(class = "modal-title", id = ns_full("plot_modal_title"), "Plot"),
          shiny::tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            `aria-label` = "Close",
            shiny::tags$span(`aria-hidden` = "true", HTML("&times;"))
          )
        ),
        
        # Modal body
        shiny::tags$div(
          class = "modal-body p-0",
          id = ns_full("plot_modal_body"),
          style = "min-height: 800px;"
        )
      )
    )
  )
}

#' JavaScript functions for plot maximization
#'
#' @return A shiny tag containing the JavaScript code
plot_maximize_js <- function() {
  shiny::tags$script(HTML("
    // Function to maximize a plot in a modal
    function maximizePlot(plotId, buttonId) {
      // Get the original plot div
      var originalPlot = document.getElementById(plotId);
      
      // If not found, try with the plotly class
      if (!originalPlot) {
        originalPlot = document.querySelector('.js-plotly-plot[id^=\"' + plotId + '\"]');
      }
      
      if (!originalPlot) {
        console.error('Plot not found:', plotId);
        return;
      }
      
      // Get the button element to extract plot title
      var button = document.getElementById(buttonId);
      var plotTitle = '';
      
      // Find the closest card header or section title
      var header = button.closest('.card-header');
      if (header) {
        plotTitle = header.textContent.trim();
      } else {
        var section = button.closest('div').querySelector('.section-title');
        if (section) {
          plotTitle = section.textContent.trim();
        } else {
          // Default title
          plotTitle = 'Plot View';
        }
      }
      
      // Set the modal title
      document.getElementById(plotId.replace(/[^-]*$/, 'plot_modal_title')).textContent = plotTitle;
      
      // Get the modal body
      var modalBody = document.getElementById(plotId.replace(/[^-]*$/, 'plot_modal_body'));
      
      // Clear previous content
      modalBody.innerHTML = '';
      
      // If it's a plotly plot
      if (originalPlot.classList.contains('js-plotly-plot')) {
        // Create a new container for the plot
        var newPlotContainer = document.createElement('div');
        newPlotContainer.id = 'modal-' + plotId;
        newPlotContainer.style.width = '100%';
        newPlotContainer.style.height = '800px';
        modalBody.appendChild(newPlotContainer);
        
        // Clone the plot to the modal
        Plotly.newPlot(
          newPlotContainer.id,
          JSON.parse(JSON.stringify(originalPlot.data)),
          JSON.parse(JSON.stringify(originalPlot.layout)),
          {responsive: true}
        );
      } else {
        // For other types of plots or content
        var clone = originalPlot.cloneNode(true);
        clone.style.width = '100%';
        clone.style.height = '800px';
        modalBody.appendChild(clone);
      }
      
      // Show the modal
      $('#' + plotId.replace(/[^-]*$/, 'plot_modal_container')).modal('show');
    }
    
    // custom CSS for the maximize button
    document.head.insertAdjacentHTML('beforeend', `
      <style>
        .plot-maximize-btn {
          position: absolute;
          top: 10px;
          right: 10px;
          z-index: 100;
          opacity: 0.6;
          font-size: 0.8rem;
          padding: 3px 6px;
        }
        .plot-maximize-btn:hover {
          opacity: 1;
        }
        .plot-container {
          position: relative;
        }
      </style>
    `);
  "))
}