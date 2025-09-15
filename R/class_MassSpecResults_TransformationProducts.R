# MARK: MassSpecResults_TransformationProducts
#' @title Constructor and methods to store transformation products results from non-target analysis
#' @description The `MassSpecResults_TransformationProducts` class is a child of [StreamFind::Results] and is used to store transformation products results from non-target analysis.
#' @param parents A data.table containing parent compound information.
#' @param transformation_products A list containing transformation products for each parent compound.
#' @export
#' @seealso [StreamFind::Results]
#'
MassSpecResults_TransformationProducts <- function(
    parents = data.table::data.table(),
    transformation_products = list()) {
  x <- structure(
    list(
      type = "MassSpec",
      name = "MassSpecResults_TransformationProducts",
      software = "StreamFind",
      version = as.character(packageVersion("StreamFind")),
      parents = parents,
      transformation_products = transformation_products
    ),
    class = c("MassSpecResults_TransformationProducts", "Results")
  )
  if (is.null(validate_object(x))) {
    return(x)
  } else {
    stop("Invalid MassSpecResults_TransformationProducts object!")
  }
}

#' @describeIn MassSpecResults_TransformationProducts Validate the MassSpecResults_TransformationProducts object, returning NULL if valid.
#' @template arg-ms-tp-x
#' @export
#'
validate_object.MassSpecResults_TransformationProducts <- function(x) {
  checkmate::assert_data_table(x$parents)
  checkmate::assert_list(x$transformation_products)
  if (length(x$transformation_products) > 0) {
    if (nrow(x$parents) > 0) {
      checkmate::assert_true(length(x$transformation_products) == length(unique(x$parents$name)))
    }
    for (tp in x$transformation_products) {
      checkmate::assert_data_frame(tp)
    }
  }
  NextMethod()
  NULL
}

# MARK: Methods
# Methods ------

# MARK: show
#' @describeIn MassSpecResults_TransformationProducts Show the MassSpecResults_TransformationProducts object.
#' @template arg-ms-tp-x
#' @export
#'
show.MassSpecResults_TransformationProducts <- function(x) {
  cat("Number of parents: ", nrow(x$parents), "\n")
  if (length(x$transformation_products) > 0) {
    tp_counts <- vapply(x$transformation_products, nrow, integer(1))
    cat("Transformation products per parent: ", paste(tp_counts, collapse = ", "), "\n")
    cat("Total transformation products: ", sum(tp_counts), "\n")
  } else {
    cat("Number of transformation products: ", 0, "\n")
  }
}

# MARK: `[`
#' @describeIn MassSpecResults_TransformationProducts Subset the MassSpecResults_TransformationProducts object.
#' @template arg-ms-tp-x
#' @export
#'
`[.MassSpecResults_TransformationProducts` <- function(x, i) {
  x$parents <- x$parents[i, ]
  if (length(x$transformation_products) > 0) {
    x$transformation_products <- x$transformation_products[i]
  }
  x
}

# MARK: get_transformation_products
#' @describeIn MassSpecResults_TransformationProducts Get transformation products for specified parents and replicates.
#' @template arg-ms-tp-x
#' @param parents A character vector of parent compound names to filter transformation products. If NULL, all parents are included.
#' @param parentsReplicate A character vector of replicate names to filter parent compounds. If NULL, all replicates are included.
#' @param productsReplicate A character vector of replicate names to filter transformation products. If NULL, all replicates are included.
#' @param onlyHits Logical, if TRUE (default), only transformation products detected in the analysis are returned. If FALSE, all transformation products are returned with a detection status.
#' @export
#'
get_transformation_products.MassSpecResults_TransformationProducts <- function(
    x,
    parents = NULL,
    parentsReplicate = NULL,
    productsReplicate = NULL,
    onlyHits = TRUE) {
  if (!is.null(parents)) {
    if (!all(parents %in% names(x$transformation_products))) {
      stop(paste("Parents not found!"))
    }
    transformation_list <- x$transformation_products[parents]
    parents_dt <- x$parents[x$parents$name %in% parents, ]
  } else {
    transformation_list <- x$transformation_products
    parents_dt <- x$parents
  }

  all_transformations <- data.table::data.table()

  for (parent_name in names(transformation_list)) {
    parents_info <- parents_dt[parents_dt$name %in% parent_name, ]
    if (!is.null(parentsReplicate)) {
      if ("replicate" %in% colnames(parents_info)) {
        parents_info <- parents_info[parents_info$replicate %in% parentsReplicate, ]
      }
    }
    if (nrow(parents_info) == 0) next

    tps <- transformation_list[[parent_name]]
    if (is.null(tps) || nrow(tps) == 0) next
    tps <- data.table::copy(tps)
    if (!is.null(productsReplicate)) {
      if ("replicate" %in% colnames(tps)) {
        tps$analysis[!tps$replicate %in% productsReplicate] <- NA_character_
        tps$replicate[!tps$replicate %in% productsReplicate] <- NA_character_
      }
    }

    if ("replicate" %in% colnames(parents_info)) {
      parent_replicates <- unique(parents_info$replicate)
      parent_replicates <- parent_replicates[!is.na(parent_replicates)]
      if (length(parent_replicates) > 0) {
        tps$parent_replicate <- paste(parent_replicates, collapse = "; ")
      } else {
        tps$parent_replicate <- NA_character_
      }
    }

    if ("group" %in% colnames(parents_info)) {
      parent_groups <- unique(parents_info$group)
      parent_groups <- parent_groups[!is.na(parent_groups)]
      if (length(parent_groups) > 0) {
        tps$parent_group <- paste(parent_groups, collapse = "; ")
      } else {
        tps$parent_group <- NA_character_
      }
    }

    if ("analysis" %in% colnames(tps)) {
      tps$detected <- !is.na(tps$analysis)
    }

    if (onlyHits) {
      tps <- tps[tps$detected, ]
    }
    all_transformations <- rbind(all_transformations, tps, fill = TRUE)
  }

  all_transformations
}

# MARK: plot_transformation_products_network
#' @describeIn MassSpecResults_TransformationProducts Plot a network of transformation products using visNetwork.
#' @template arg-ms-tp-x
#' @param parents A character vector of parent compound names to include in the network. If NULL, all parents are included.
#' @param parentsReplicate A character vector of replicate names to filter parent compounds. If NULL, all replicates are included.
#' @param productsReplicate A character vector of replicate names to filter transformation products. If NULL, all replicates are included.
#' @return A visNetwork object representing the transformation products network.
#' @export
#'
plot_transformation_products_network.MassSpecResults_TransformationProducts <- function(
    x,
    parents = NULL,
    parentsReplicate = NULL,
    productsReplicate = NULL) {
  if (!require(visNetwork)) {
    stop("visNetwork package is required for this function")
  }
  if (!require(ChemmineR)) {
    stop("ChemmineR package is required for structure visualization")
  }
  if (!require(base64enc)) {
    stop("base64enc package is required for image encoding")
  }
  
  # Create side-by-side structure comparison plots
  create_structure_image <- function(smiles, compound_name, parent_smiles = NULL, width = 380, height = 380) {
    if (!require(ChemmineR)) {
      stop("ChemmineR package is required for structure visualization")
    }
    if (!require(ggplot2)) {
      stop("ggplot2 package is required for ggsave functionality")
    }
    tryCatch(
      {
        sdf <- smiles2sdf(smiles)
        temp_file <- tempfile(fileext = ".png")
        
        # If parent SMILES is provided, create side-by-side comparison
        if (!is.null(parent_smiles)) {
          parent_sdf <- smiles2sdf(parent_smiles)
          
          # Create side-by-side plot with parent and transformation product
          # Use png() directly instead of ggsave for base R plots
          png(filename = temp_file, 
              width = width * 2.5, height = height, 
              res = 150, bg = "white")
          
          # Set up a 1x2 grid for side-by-side plotting with different widths
          layout(matrix(c(1, 2), 1, 2), widths = c(0.4, 0.6))
          
          # Plot parent structure on the left (smaller)
          par(mar = c(2, 1, 2, 0.5))
          plotStruc(parent_sdf[[1]], atomcex = 0.6, 
                   atomnum = FALSE,
                   main = "Parent")
          
          # Plot transformation product on the right (larger)
          par(mar = c(2, 0.5, 2, 1))
          plotStruc(sdf[[1]], atomcex = 0.8, 
                   atomnum = FALSE,
                   main = paste("TP:", compound_name))
          
          # Close the device
          dev.off()
        } else {
          # Normal structure plotting without parent comparison
          png(filename = temp_file, 
              width = width * 1.2, height = height, 
              res = 150, bg = "white")
          
          par(mar = c(1, 1, 1, 1))
          plotStruc(sdf[[1]])
          
          dev.off()
        }
        
        img_base64 <- base64enc::base64encode(temp_file)
        unlink(temp_file)
        return(paste0("data:image/png;base64,", img_base64))
      },
      error = function(e) {
        warning(paste("Could not create structure for", compound_name, ":", e$message))
        return(NULL)
      }
    )
  }
  if (!is.null(parents)) {
    if (!all(parents %in% names(x$transformation_products))) {
      stop(paste("Parents not found!"))
    }
    transformation_list <- x$transformation_products[parents]
    parents_dt <- x$parents[x$parents$name %in% parents, ]
  } else {
    transformation_list <- x$transformation_products
    parents_dt <- x$parents
  }
  all_nodes <- data.table::data.table()
  all_edges <- data.table::data.table()
  for (parent_name in names(transformation_list)) {
    parent_info <- parents_dt[parents_dt$name %in% parent_name, ]
    if (nrow(parent_info) == 0) next
    if (!"replicate" %in% colnames(parent_info)) {
      parent_info$replicate <- ""
    }
    if (!"analysis" %in% colnames(parent_info)) {
      parent_info$analysis <- ""
    }
    if (!"group" %in% colnames(parent_info)) {
      parent_info$group <- ""
    }

    tps <- transformation_list[[parent_name]]
    if (is.null(tps) || nrow(tps) == 0) next
    if (!"replicate" %in% colnames(tps)) {
      tps$replicate <- ""
    }
    if (!"analysis" %in% colnames(tps)) {
      tps$analysis <- ""
    }
    if (!"group" %in% colnames(tps)) {
      tps$group <- ""
    }
    if (!"parent_similarity" %in% colnames(tps)) {
      tps$parent_similarity <- 0
    }
    if (!"max_shared_fragments" %in% colnames(tps)) {
      tps$max_shared_fragments <- 0
    }

    parent_info$replicate[is.na(parent_info$replicate)] <- ""
    parent_info$analysis[is.na(parent_info$analysis)] <- ""
    parent_info$group[is.na(parent_info$group)] <- ""
    tps$replicate[is.na(tps$replicate)] <- ""
    tps$analysis[is.na(tps$analysis)] <- ""
    tps$group[is.na(tps$group)] <- ""
    

    parent_info_summary <- parent_info[, .(
      name = name[1],
      formula = formula[1],
      mass = mass[1],
      SMILES = SMILES[1]
    ), by = name]

    parents_info_details <- unique(parent_info[, c("analysis", "replicate", "group")])

    create_details_parent_table <- function(details_df) {
      if (nrow(details_df) == 0) return("")
      table_html <- "<table border='1' cellpadding='3' cellspacing='0' style='border-collapse: collapse; font-size: 10px;'>"
      table_html <- paste0(table_html, "<tr style='background-color: #f0f0f0;'>")
      table_html <- paste0(table_html, "<th>Analysis</th><th>Replicate</th><th>Group</th>")
      table_html <- paste0(table_html, "</tr>")
      for (i in seq_len(nrow(details_df))) {
        table_html <- paste0(table_html, "<tr>")
        table_html <- paste0(table_html, "<td>", ifelse(is.na(details_df$analysis[i]), "-", details_df$analysis[i]), "</td>")
        table_html <- paste0(table_html, "<td>", ifelse(is.na(details_df$replicate[i]), "-", details_df$replicate[i]), "</td>")
        table_html <- paste0(table_html, "<td>", ifelse(is.na(details_df$group[i]), "-", details_df$group[i]), "</td>")
        table_html <- paste0(table_html, "</tr>")
      }
      table_html <- paste0(table_html, "</table>")
      return(table_html)
    }

    tps_summary <- tps[, .(
      formula = formula[1],
      mass = mass[1],
      generation = generation[1],
      transformation = paste(unique(transformation[!is.na(transformation) & transformation != ""]), collapse = "<br>"),
      SMILES = SMILES[1],
      parent_name = parent_name[1]
    ), by = name]

    cols_tps_keep <- c("name", "analysis", "replicate", "group", "parent_similarity", "max_shared_fragments")
    cols_tps_keep <- cols_tps_keep[cols_tps_keep %in% colnames(tps)]
    tps_details <- unique(tps[, cols_tps_keep, with = FALSE])
    tps_details <- tps_details[!(analysis %in% "" & replicate %in% "" & group %in% "")]

    parent_info_summary$color <- "lightgreen"
    if (!is.null(parentsReplicate)) {
      if (all(!parents_info_details$replicate %in% parentsReplicate)) {
        parent_info_summary$color <- "lightgray"
      }
    }

    tps_summary$color <- "lightblue"
    tps_summary$color[tps_summary$generation == 1] <- "orange"
    tps_summary$color[tps_summary$generation == 2] <- "yellow"
    
    if (!is.null(productsReplicate)) {
      if (any(!tps$replicate %in% productsReplicate)) {
        names_in_replicate <- unique(tps$name[tps$replicate %in% productsReplicate])
        names_not_in_replicate <- unique(tps$name[!tps$name %in% names_in_replicate])
        tps_summary$color[tps_summary$name %in% names_not_in_replicate] <- "lightgray"
      }
    }

    parent_image <- NULL
    parent_smiles <- NULL
    if ("SMILES" %in% colnames(parent_info_summary) && !is.na(parent_info_summary$SMILES[1])) {
      parent_smiles <- parent_info_summary$SMILES[1]
      parent_image <- create_structure_image(parent_smiles, parent_info_summary$name)
    }

    parent_node <- data.table::data.table(
      id = parent_info_summary$name,
      label = parent_info_summary$name,
      group = "Parent",
      value = 20,
      shape = "circle",
      title = paste0(
        "<p><b>Parent: ", parent_info_summary$name, "</b><br>",
        "Formula: ", parent_info_summary$formula, "<br>",
        "Mass: ", round(parent_info_summary$mass, 4), "<br>",
        if (nrow(parents_info_details) > 0) paste0("<br><b>Details:</b><br>", create_details_parent_table(parents_info_details), "<br>") else "",
        if (!is.null(parent_image)) paste0("<img src='", parent_image, "' width='500' height='400' style='display:block; margin:10px;'><br>") else "",
        "</p>"
      ),
      color = parent_info_summary$color,
      shadow = TRUE,
      font.size = 14,
      stringsAsFactors = FALSE
    )

    tp_images <- rep(NA_character_, nrow(tps_summary))
    if ("SMILES" %in% names(tps_summary)) {
      for (i in seq_len(nrow(tps_summary))) {
        if (!is.na(tps_summary$SMILES[i])) {
          tp_img <- create_structure_image(tps_summary$SMILES[i], tps_summary$name[i], parent_smiles)
          if (!is.null(tp_img)) {
            tp_images[i] <- tp_img
          }
        }
      }
    }

    create_details_table <- function(details_df) {
      if (nrow(details_df) == 0) return("")
      table_html <- "<table border='1' cellpadding='3' cellspacing='0' style='border-collapse: collapse; font-size: 10px;'>"
      table_html <- paste0(table_html, "<tr style='background-color: #f0f0f0;'>")
      table_html <- paste0(table_html, "<th>Analysis</th><th>Replicate</th><th>Group</th><th>Parent Similarity</th><th>Max Shared Fragments</th>")
      table_html <- paste0(table_html, "</tr>")
      for (i in seq_len(nrow(details_df))) {
        table_html <- paste0(table_html, "<tr>")
        table_html <- paste0(table_html, "<td>", ifelse(is.na(details_df$analysis[i]), "-", details_df$analysis[i]), "</td>")
        table_html <- paste0(table_html, "<td>", ifelse(is.na(details_df$replicate[i]), "-", details_df$replicate[i]), "</td>")
        table_html <- paste0(table_html, "<td>", ifelse(is.na(details_df$group[i]), "-", details_df$group[i]), "</td>")
        table_html <- paste0(table_html, "<td>", ifelse(is.na(details_df$parent_similarity[i]), "-", details_df$parent_similarity[i]), "</td>")
        table_html <- paste0(table_html, "<td>", ifelse(is.na(details_df$max_shared_fragments[i]), "-", details_df$max_shared_fragments[i]), "</td>")
        table_html <- paste0(table_html, "</tr>")
      }
      table_html <- paste0(table_html, "</table>")
      return(table_html)
    }

    tp_nodes <- data.table::data.table(
      id = tps_summary$name,
      label = tps_summary$name,
      group = paste0("Generation_", tps_summary$generation),
      value = 15 - tps_summary$generation * 2,
      shape = "box",
      title = sapply(seq_len(nrow(tps_summary)), function(i) {
        tp_name <- tps_summary$name[i]
        tp_details <- tps_details[tps_details$name == tp_name, ]
        details_table <- create_details_table(tp_details)
        paste0(
          "<p><b>", tp_name, "</b><br>",
          "Formula: ", tps_summary$formula[i], "<br>",
          "Mass: ", round(tps_summary$mass[i], 4), "<br>",
          "Generation: ", tps_summary$generation[i], "<br>",
          "Transformation: ", tps_summary$transformation[i], "<br>",
          if (nrow(tp_details) > 0) paste0("<br><b>Details:</b><br>", details_table, "<br>") else "",
          ifelse(!is.na(tp_images[i]), paste0("<img src='", tp_images[i], "' width='500' height='400' style='display:block; margin:10px;'><br>"), ""),
          "</p>"
        )
      }),
      color = tps_summary$color,
      shadow = FALSE,
      font.size = 10,
      stringsAsFactors = FALSE
    )

    current_nodes <- rbind(parent_node, tp_nodes)
    all_nodes <- rbind(all_nodes, current_nodes)

    edges <- data.frame(
      from = tps_summary$parent_name,
      to = tps_summary$name
    )
    edges$color <- "lightgray"
    edges$width <- 1
    edges$stringsAsFactors <- FALSE
    edges <- unique(edges)

    all_edges <- rbind(all_edges, edges)
  }
  network <- visNetwork(all_nodes, all_edges, height = "800px", width = "100%") %>%
    visOptions(
      highlightNearest = list(enabled = TRUE, degree = 10, hover = TRUE),
      nodesIdSelection = list(enabled = TRUE)
    ) %>%
    visLayout(randomSeed = 123) %>%
    visPhysics(
      enabled = TRUE,
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(
        gravitationalConstant = -50,
        centralGravity = 0.01,
        springLength = 100,
        springConstant = 0.08
      )
    ) %>%
    visNodes(
      size = 30,
      borderWidth = 2,
      borderWidthSelected = 4
    )
  network
}
