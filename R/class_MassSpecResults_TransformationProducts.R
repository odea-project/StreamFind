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
  create_structure_image <- function(smiles, compound_name, width = 380, height = 380) {
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
        ggplot2::ggsave(
          filename = temp_file,
          plot = {
            par(mar = c(0.1, 0.1, 0.1, 0.1))
            plotStruc(sdf[[1]])
          },
          device = "png",
          width = width / 72, # Convert pixels to inches
          height = height / 72,
          dpi = 150,
          bg = "white",
          units = "in"
        )
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
    parents_info <- parents_dt[parents_dt$name %in% parent_name, ]
    if (!is.null(parentsReplicate)) {
      if ("replicate" %in% colnames(parents_info)) {
        parents_info <- parents_info[parents_info$replicate %in% parentsReplicate, ]
      }
    }
    if (nrow(parents_info) == 0) next
    tps <- transformation_list[[parent_name]]
    if (!is.null(productsReplicate)) {
      if ("replicate" %in% colnames(tps)) {
        tps$analysis[!tps$replicate %in% productsReplicate] <- NA_character_
        tps$replicate[!tps$replicate %in% productsReplicate] <- NA_character_
      }
    }
    if (is.null(tps) || nrow(tps) == 0) next

    tps$color <- "lightblue"
    tps$color[tps$generation == 1] <- "orange"
    tps$color[tps$generation == 2] <- "yellow"
    if ("analysis" %in% colnames(parents_info)) {
      tps$color[is.na(tps$analysis)] <- "lightgray"
    }
    tps_group <- character()
    if ("group" %in% colnames(tps)) {
      tps_group <- tps[, c("name", "group")]
      tps_group <- tps_group[, .(group = paste(unique(group[!is.na(group)]), collapse = "<br>")), by = "name"]
      tps_group_char <- tps_group$group
    } else {
      tps_group_char <- rep("", nrow(tps))
    }
    names(tps_group_char) <- tps_group$name
    tps_rpl <- character()
    if ("replicate" %in% colnames(tps)) {
      tps_rpl <- tps[, c("name", "replicate")]
      tps_rpl <- tps_rpl[, .(replicate = paste(unique(replicate[!is.na(replicate)]), collapse = "<br>")), by = "name"]
      tps_rpl_char <- tps_rpl$replicate
    } else {
      tps_rpl_char <- rep("", nrow(tps))
    }
    names(tps_rpl_char) <- tps_rpl$name
    if ("group" %in% colnames(parents_info)) {
      parents_group <- unique(parents_info$group)
      parents_group <- parents_group[!is.na(parents_group)]
      if (length(parents_group) > 0) {
        parents_group <- paste(parents_group, collapse = "<br>")
      }
    } else {
      parents_group <- ""
    }
    if ("replicate" %in% colnames(parents_info)) {
      parents_rpl <- unique(parents_info$replicate)
      parents_rpl <- parents_rpl[!is.na(parents_rpl)]
      if (length(parents_rpl) > 0) {
        parents_rpl <- paste(parents_rpl, collapse = "<br>")
      }
    } else {
      parents_rpl <- ""
    }
    parent_image <- NULL
    if ("SMILES" %in% colnames(parents_info) && !is.na(parents_info$SMILES[1])) {
      parent_image <- create_structure_image(parents_info$SMILES[1], parent_name)
    }
    parent_node <- data.table::data.table(
      id = parent_name,
      label = parent_name,
      group = "Parent",
      value = 20,
      shape = "circle",
      title = paste0(
        "<p><b>Parent: ", parent_name, "</b><br>",
        "Replicate: <br>", parents_rpl, "<br>",
        "Group: <br>", parents_group, "<br>",
        "Formula: ", parents_info$formula[1], "<br>",
        "Mass: ", round(parents_info$mass[1], 4), "<br>",
        if (!is.null(parent_image)) paste0("<img src='", parent_image, "' width='400' height='400' style='display:block; margin:10px;'><br>") else "",
        "</p>"
      ),
      color = "lightgreen",
      shadow = TRUE,
      font.size = 14,
      stringsAsFactors = FALSE
    )

    if ("analysis" %in% colnames(parents_info)) {
      if (all(is.na(parents_info$analysis))) {
        parent_node$color <- "lightgray"
      }
    }

    # Create structure images for transformation products if SMILES is available
    tp_images <- rep(NA_character_, nrow(tps))
    if ("SMILES" %in% names(tps)) {
      for (i in seq_len(nrow(tps))) {
        if (!is.na(tps$SMILES[i])) {
          tp_img <- create_structure_image(tps$SMILES[i], tps$name[i])
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
        "Replicate: <br>", tps_rpl_char[tps$name], "<br>",
        "Group: <br>", tps_group_char[tps$name], "<br>",
        "Formula: ", tps$formula, "<br>",
        "Mass: ", round(tps$mass, 4), "<br>",
        "Generation: ", tps$generation, "<br>",
        "Transformation: ", tps$transformation, "<br>",
        ifelse(!is.na(tp_images), paste0("<img src='", tp_images, "' width='400' height='400' style='display:block; margin:10px;'><br>"), ""),
        "</p>"
      ),
      color = tps$color,
      shadow = FALSE,
      font.size = 10,
      stringsAsFactors = FALSE
    )

    tp_nodes <- unique(tp_nodes)
    tp_nodes <- tp_nodes[!duplicated(tp_nodes$id), ]
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
