# NON EXPORTED FUNCTIONS -------------------------------------------------------

processXmlGraph <- function (graph) {
  # Non-exported function to read cost matrices from XML files in TSPLib format
  # Args:
  #   graph: XML object
  # Returns:
  #   The cost matrix read from the XML file
  #
  
  # Get the size of the problem and build a matrix with 0's
  n <- xmlSize(graph)
  cost.matrix <- matrix(rep(0, n^2), ncol=n)
  colnames(cost.matrix) <- paste("C", 1:n, sep="")
  rownames(cost.matrix) <- colnames(cost.matrix)
  
  # Function to process the list of edges in a node. It creates pairs 
  # (destination node, cost)
  processEdge <- function(j, edges) {
    edge <- edges[[j]]
    res <- c(as.numeric(xmlValue(edge)) + 1, 
             as.numeric(xmlGetAttr(edge, "cost")))
    return(res)
  }
  
  # Function to process the list of nodes in the graph. To the pair (destination node, cost) it adds the origin node information
  processVertex <- function(i, graph) {
    edges <- graph[[i]]
    aux <- lapply (1:(n - 1), FUN=processEdge, edges=edges)
    return(cbind(i , do.call(rbind, aux)))
  }
  
  # Create a matrix with the triplets i, j, value to replace in the matrix 
  aux <- lapply (1:n, FUN=processVertex, graph=graph)
  replacement <- do.call(rbind, aux)
  
  # Do the replacement
  cost.matrix[replacement[, 1:2]] <- replacement[, 3]
  return(cost.matrix)
}

#' Function to read cost matrices from TSPlib
#' 
#' This function reads XML files (compressed or not) from TSPlib and builds the cost matrix associated
#' 
#' @family parsers
#' @param file It can be a local file or a URL corresponding to an XML file or compressed (zip) XML file
#' @return A matrix containing the cost associated to each pair of cities
#' @seealso \code{\link{tspProblem}}
#' 
tsplibParser <- function (file) {
  require("XML")
  td <- tempdir()
  # If the file is a URL, download it
  if (length(grep("http", file)) > 0) {
    tf <- tempfile(tmpdir=td, fileext=".zip")
    download.file(file, tf)
  } else {
    tf <- file
  }
  
  # If the file is compressed, uncompress it
  if (length(grep(".zip$", tf) > 0)) {
    fname <- unzip(tf, list=TRUE)$Name[1]  
    unzip(tf, files=fname, exdir=td, overwrite=TRUE)
    fpath <- file.path(td, fname)    
  } else if (length(grep(".xml$", tf) > 0)) {
    fpath <- tf
  } else {
    stop ("File extension unknown. It should be either xml or zip")
  }
  # Finally, read the file
  xml <- xmlParse(fpath)
  
  # Get the top. It should be only one element of type travellingSalesmanProblemInstance
  xml.top <- xmlRoot(xml)
  if (xmlName(xml.top) != "travellingSalesmanProblemInstance") {
    stop ("This is not a valid XML file. The root of the file should be a ",
          "'travellingSalesmanProblemInstance'")
  }
  xml.children <- xmlChildren(xml.top)
  
  message("Processing file corresponding to instance ", xmlValue(xml.children$name),  
          ": ", xmlValue(xml.children$description))
  cost.matrix <- processXmlGraph (xml.children$graph)
  return(cost.matrix)
}