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

    # Filter by parent replicate if specified
    if (!is.null(parentsReplicate)) {
      if ("replicate" %in% colnames(parents_info)) {
        parents_info <- parents_info[parents_info$replicate %in% parentsReplicate, ]
      }
    }
    if (nrow(parents_info) == 0) next

    tps <- transformation_list[[parent_name]]
    if (is.null(tps) || nrow(tps) == 0) next

    # Create a copy to avoid modifying the original data
    tps <- data.table::copy(tps)

    # Filter by product replicate if specified
    if (!is.null(productsReplicate)) {
      if ("replicate" %in% colnames(tps)) {
        # Mark rows that don't match the specified replicate
        tps$analysis[!tps$replicate %in% productsReplicate] <- NA_character_
        tps$replicate[!tps$replicate %in% productsReplicate] <- NA_character_
      }
    }

    # Add parent replicate and group information if available
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

    # Add detection status based on analysis column
    if ("analysis" %in% colnames(tps)) {
      tps$detected <- !is.na(tps$analysis)
    }

    if (onlyHits) {
      tps <- tps[tps$detected, ]
    }

    # Combine all transformation products
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
    if (!is.null(parentsReplicate)) {
      if ("replicate" %in% colnames(parent_info)) {
        parent_info <- parent_info[parent_info$replicate %in% parentsReplicate, ]
      } else {
        parent_info$replicate <- ""
        parent_info$analysis <- ""
      }
    } else {
      parent_info$replicate <- ""
      parent_info$analysis <- ""
    }
    if (nrow(parent_info) == 0) next

    has_replicate_filter <- FALSE
    tps <- transformation_list[[parent_name]]
    if (!is.null(productsReplicate)) {
      has_replicate_filter <- TRUE
      if ("replicate" %in% colnames(tps)) {
        tps$analysis[!tps$replicate %in% productsReplicate] <- ""
        tps$replicate[!tps$replicate %in% productsReplicate] <- ""
      } else {
        tps$analysis <- ""
        tps$replicate <- ""
      }
    } else {
      tps$analysis <- ""
      tps$replicate <- ""
    }
    if (is.null(tps) || nrow(tps) == 0) next

    if (!"group" %in% colnames(parent_info)) {
      parent_info$group <- ""
    }

    if (!"group" %in% colnames(tps)) {
      tps$group <- ""
    }

    tps$group[is.na(tps$group)] <- ""
    parent_info$group[is.na(parent_info$group)] <- ""

    parent_info <- parent_info[, .(
      formula = formula[1],
      mass = mass[1],
      SMILES = SMILES[1],
      analysis = paste(unique(analysis[!is.na(analysis) & analysis != ""]), collapse = "<br>"),
      replicate = paste(unique(replicate[!is.na(replicate) & replicate != ""]), collapse = "<br>"),
      group = paste(unique(group[!is.na(group) & group != ""]), collapse = "<br>")
    ), by = name]

    tps <- tps[, .(
      formula = formula[1],
      mass = mass[1],
      generation = generation[1],
      transformation = paste(unique(transformation[!is.na(transformation) & transformation != ""]), collapse = "<br>"),
      SMILES = SMILES[1],
      analysis = paste(unique(analysis[!is.na(analysis) & analysis != ""]), collapse = "<br>"),
      replicate = paste(unique(replicate[!is.na(replicate) & replicate != ""]), collapse = "<br>"),
      group = paste(unique(group[!is.na(group) & group != ""]), collapse = "<br>"),
      parent_name = parent_name[1]
    ), by = name]

    parent_info$color <- "lightgreen"
    if (any(!parent_info$replicate %in% "")) {
      parent_info$color[parent_info$replicate %in% ""] <- "lightgray"
    }

    tps$color <- "lightblue"
    tps$color[tps$generation == 1] <- "orange"
    tps$color[tps$generation == 2] <- "yellow"
    if (any(!tps$replicate %in% "") || has_replicate_filter) {
      tps$color[tps$replicate %in% ""] <- "lightgray"
    }

    parent_image <- NULL
    parent_smiles <- NULL
    if ("SMILES" %in% colnames(parent_info) && !is.na(parent_info$SMILES[1])) {
      parent_smiles <- parent_info$SMILES[1]
      parent_image <- create_structure_image(parent_smiles, parent_name)
    }

    parent_node <- data.table::data.table(
      id = parent_name,
      label = parent_name,
      group = "Parent",
      value = 20,
      shape = "circle",
      title = paste0(
        "<p><b>Parent: ", parent_name, "</b><br>",
        "Replicate: <br>", parent_info$replicate, "<br>",
        "Group: <br>", parent_info$group, "<br>",
        "Formula: ", parent_info$formula, "<br>",
        "Mass: ", round(parent_info$mass, 4), "<br>",
        if (!is.null(parent_image)) paste0("<img src='", parent_image, "' width='500' height='400' style='display:block; margin:10px;'><br>") else "",
        "</p>"
      ),
      color = parent_info$color,
      shadow = TRUE,
      font.size = 14,
      stringsAsFactors = FALSE
    )

    tp_images <- rep(NA_character_, nrow(tps))
    if ("SMILES" %in% names(tps)) {
      for (i in seq_len(nrow(tps))) {
        if (!is.na(tps$SMILES[i])) {
          tp_img <- create_structure_image(tps$SMILES[i], tps$name[i], parent_smiles)
          if (!is.null(tp_img)) {
            tp_images[i] <- tp_img
          }
        }
      }
    }

    tp_nodes <- data.table::data.table(
      id = tps$name,
      label = tps$name,
      group = paste0("Generation_", tps$generation),
      value = 15 - tps$generation * 2,
      shape = "box",
      title = paste0(
        "<p><b>", tps$name, "</b><br>",
        "Replicate: <br>", tps$replicate, "<br>",
        "Group: <br>", tps$group, "<br>",
        "Formula: ", tps$formula, "<br>",
        "Mass: ", round(tps$mass, 4), "<br>",
        "Generation: ", tps$generation, "<br>",
        "Transformation: ", tps$transformation, "<br>",
        ifelse(!is.na(tp_images), paste0("<img src='", tp_images, "' width='500' height='400' style='display:block; margin:10px;'><br>"), ""),
        "</p>"
      ),
      color = tps$color,
      shadow = FALSE,
      font.size = 10,
      stringsAsFactors = FALSE
    )

    #tp_nodes <- unique(tp_nodes)
    current_nodes <- rbind(parent_node, tp_nodes)
    all_nodes <- rbind(all_nodes, current_nodes)

    edges <- data.frame(
      from = tps$parent_name,
      to = tps$name
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
