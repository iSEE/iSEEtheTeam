.photoFile <- "PhotoFile"
.affiliationBoxOpen <- "AffiliationBoxOpen"

collated <- character(0)
collated[.photoFile] <- "character"
collated[.affiliationBoxOpen] <- "logical"

#' @importClassesFrom iSEE Panel
#' @export
setClass("CharlotteSoneson", contains="Panel", slots=collated)

#' Charlotte Soneson
#' 
#' @name CharlotteSoneson-class
#' 
#' @aliases 
#' initialize,CharlotteSoneson-method
#' .refineParameters,CharlotteSoneson-method
#' .panelColor,CharlotteSoneson-method
#' .fullName,CharlotteSoneson-method
#' .defineOutput,CharlotteSoneson-method
#' .renderOutput,CharlotteSoneson-method
NULL

#' Charlotte Soneson
#' 
#' @export
#' @examples
#' library(SummarizedExperiment)
#' library(iSEE)
#' 
#' se <- SummarizedExperiment()
#' 
#' iSEE(se, initial = list(CharlotteSoneson()))
CharlotteSoneson <- function(...) {
    new("CharlotteSoneson", ...)
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "CharlotteSoneson", function(.Object, ...) {
    args <- list(...)
    
    args <- .emptyDefault(args, .photoFile, system.file(package = "iSEEtheTeam", "images", "charlotte-soneson.png"))
    args <- .emptyDefault(args, .affiliationBoxOpen, FALSE)

    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "CharlotteSoneson", function(x, se) {
    x <- callNextMethod()
    
    if (is.null(x)) {
        return(NULL)
    }
    
    x
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".panelColor", "CharlotteSoneson", function(x) {
    "#ffc107"
})

#' @export
setMethod(".fullName", "CharlotteSoneson", function(x) "Charlotte Soneson")

#' @importFrom shiny imageOutput
#' @importFrom shinyWidgets addSpinner
#' @export
setMethod(".defineOutput", "CharlotteSoneson", function(x) {
    plot_name <- .getEncodedName(x)
    addSpinner(
        imageOutput(plot_name, height=paste0(slot(x, .organizationHeight), "px")),
        color=.panelColor(x)
    )
})

#' @export
#' @importFrom shiny renderImage tagList
setMethod(".renderOutput", "CharlotteSoneson", function(x, se, output, pObjects, rObjects) {
    plot_name <- .getEncodedName(x)
    force(se) # defensive programming to avoid difficult bugs due to delayed evaluation.
    
    # width  <- session$clientData[[paste0("output_", plot_name, "_width")]]
    # height <- session$clientData[[paste0("output_", plot_name, "_height")]]

    # nocov start
    output[[plot_name]] <- renderImage({
        list(src = x[[.photoFile]],
            width = "100%",
            # height = "100%",
            alt = "Charlotte Soneson")
    }, deleteFile = FALSE)
    # nocov end

    callNextMethod()
})

#' @export
setMethod(".defineInterface", "CharlotteSoneson", function(x, se, select_info) {
    out <- callNextMethod()
    plot_name <- .getEncodedName(x)
    this_box <- collapseBox(
        id=paste0(plot_name, "_", .affiliationBoxOpen),
        title="Affiliation",
        open=slot(x, .affiliationBoxOpen),
        p(
            "Friedrich Miescher Institute for Biomedical Research, Basel, Switzerland."
        )
    )
    list(
        this_box
    )
})
