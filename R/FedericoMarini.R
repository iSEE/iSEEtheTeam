.photoFile <- "PhotoFile"

collated <- character(0)
collated[.photoFile] <- "character"

#' @importClassesFrom iSEE Panel
#' @export
setClass("FedericoMarini", contains="Panel", slots=collated)

#' Federico Marini
#' 
#' @name FedericoMarini-class
#' 
#' @aliases 
#' initialize,FedericoMarini-method
#' .refineParameters,FedericoMarini-method
#' .panelColor,FedericoMarini-method
#' .fullName,FedericoMarini-method
#' .defineOutput,FedericoMarini-method
#' .renderOutput,FedericoMarini-method
NULL

#' Federico Marini
#' 
#' @export
#' @examples
#' library(SummarizedExperiment)
#' library(iSEE)
#' 
#' se <- SummarizedExperiment()
#' 
#' iSEE(se, initial = list(FedericoMarini()))
FedericoMarini <- function(...) {
    new("FedericoMarini", ...)
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "FedericoMarini", function(.Object, ...) {
    args <- list(...)
    
    args <- .emptyDefault(args, .photoFile, system.file(package = "iSEEtheTeam", "images", "federico-marini.png"))

    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "FedericoMarini", function(x, se) {
    x <- callNextMethod()
    
    if (is.null(x)) {
        return(NULL)
    }
    
    x
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".panelColor", "FedericoMarini", function(x) {
    "#28a745"
})

#' @export
setMethod(".fullName", "FedericoMarini", function(x) "Federico Marini")

#' @importFrom shiny imageOutput
#' @importFrom shinyWidgets addSpinner
#' @export
setMethod(".defineOutput", "FedericoMarini", function(x) {
    plot_name <- .getEncodedName(x)
    addSpinner(
        imageOutput(plot_name, height=paste0(slot(x, .organizationHeight), "px")),
        color=.panelColor(x)
    )
})

#' @export
#' @importFrom shiny renderImage tagList
setMethod(".renderOutput", "FedericoMarini", function(x, se, output, pObjects, rObjects) {
    plot_name <- .getEncodedName(x)
    force(se) # defensive programming to avoid difficult bugs due to delayed evaluation.
    
    # width  <- session$clientData[[paste0("output_", plot_name, "_width")]]
    # height <- session$clientData[[paste0("output_", plot_name, "_height")]]

    # nocov start
    output[[plot_name]] <- renderImage({
        list(src = x[[.photoFile]],
            width = "100%",
            # height = "100%",
            alt = "Federico Marini")
    }, deleteFile = FALSE)
    # nocov end

    callNextMethod()
})
