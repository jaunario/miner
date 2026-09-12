#' Determine block type and style at some position
#'
#' Determine the type of the block at position x (north / south),
#' y (height), z (east / west). By default, the block's style is
#' also given, although the style can be excluded from the output
#' using the `include_style` parameter.
#'
#' @param x A numeric string with north/south position
#' @param y A numeric string with height
#' @param z A numeric string with east/west position
#' @param include_style A logical value of whether the block's
#'    style should also be included in the output (defaults to TRUE).
#'
#' @return A numeric or character vector of length one or two with the type ID/name
#'    and style, if `include_style` is `TRUE`, of the block
#'    at position (x, y, z). You can use [find_item()] to
#'    find the name of the block type based on this returned ID.
#'
#' @examples
#' \dontrun{
#' mc_connect()
#' h <- getHeight(0,0)
#' b_type <- getBlock(0,h,0)
#' b_type
#'
#' find_item(id = b_type[1])
#' find_item(id = b_type[1], style = b_type[2])
#'
#' getBlock(0,h,0, include_style = FALSE)
#' }
#'
#' @importFrom stats setNames
#' @export

getBlock <- function(x,y,z, include_style = TRUE)
{
    x <- floor(as.numeric(x))
    y <- floor(as.numeric(y))
    z <- floor(as.numeric(z))
    result <- mc_sendreceive(merge_data("world.getBlockWithData", x, y, z))

    parts <- strsplit(result, ",")[[1]]
    num_parts <- suppressWarnings(as.numeric(parts))
    if (!any(is.na(num_parts))) {
        parts <- num_parts
    }

    # If world.getBlockWithData returned air (0 or "0" or "AIR"), check world.getBlock
    # because modern Minecraft (1.13+) blocks without legacy IDs return 0 in getBlockWithData
    if (length(parts) > 0 && (parts[1] == 0 || tolower(as.character(parts[1])) %in% c("0", "air"))) {
        block_type <- mc_sendreceive(merge_data("world.getBlock", x, y, z))
        if (!is.null(block_type) && block_type != "" && block_type != "0" && tolower(block_type) != "air") {
            bt_parts <- strsplit(block_type, ",")[[1]]
            num_bt_parts <- suppressWarnings(as.numeric(bt_parts))
            if (!any(is.na(num_bt_parts))) {
                bt_parts <- num_bt_parts
            }
            if (length(bt_parts) >= 2) {
                parts <- bt_parts
            } else {
                parts[1] <- bt_parts[1]
                if (length(parts) < 2) {
                    parts <- c(parts[1], "0")
                }
            }
        }
    }

    if (length(parts) >= 2) {
        out <- stats::setNames(parts[1:2], c("typeID", "style"))
    } else {
        out <- stats::setNames(parts[1], "typeID")
    }

    if(include_style && length(out) >= 2){
      return(out)
    } else {
      return(out[1])
    }

}


#' Determine block types in a cuboid
#'
#' Determine block types in a cuboid for which one corner is at
#' the position (x0, y0, z0) and the opposite corner is at the position
#' (x1, y1, z1).
#'
#' @param x0 A numeric value giving the starting north / south position
#'    of the cuboid.
#' @param y0 A numeric value giving the starting height
#'    of the cuboid.
#' @param z0 A numeric value giving the starting east / west position
#'    of the cuboid.
#' @param x1 A numeric value giving the north / south position of
#'    the opposite corner of the cuboid.
#' @param y1 A numeric value giving the ending height of
#'    the opposite corner of the cuboid.
#' @param z1 A numeric value giving the ending east / west position of
#'    the opposite corner of the cuboid.
#'
#' @return A 3-D array of integers or character strings where each element gives the ID or name of the
#'    type of a block in the cuboid.
#'
#' @examples
#' \dontrun{
#' mc_connect()
#' h <- getHeight(0,0)
#' block_types <- getBlocks(0, h, 0, 1, h + 3, 2)
#' block_types
#'
#' find_item(id = block_types[1, 1, 1])
#' }
#'
#' @export

getBlocks <- function(x0,y0,z0, x1,y1,z1)
{
    x0 <- floor(as.numeric(x0))
    y0 <- floor(as.numeric(y0))
    z0 <- floor(as.numeric(z0))

    x1 <- floor(as.numeric(x1))
    y1 <- floor(as.numeric(y1))
    z1 <- floor(as.numeric(z1))

    # reorder the cuboid values
    if(x0 > x1) { tmp <- x1; x1 <- x0; x0 <- tmp }
    if(y0 > y1) { tmp <- y1; y1 <- y0; y0 <- tmp }
    if(z0 > z1) { tmp <- z1; z1 <- z0; z0 <- tmp }

    result <- mc_sendreceive(merge_data("world.getBlocks", x0, y0, z0, x1, y1, z1))

    # blocks come back as a vector with values separated by commas
    parts <- strsplit(result, ",")[[1]]
    num_parts <- suppressWarnings(as.numeric(parts))
    if (!any(is.na(num_parts))) {
        parts <- num_parts
    }

    # the order of things is a bit tricky
    res_array <- array(parts, dim=c(z1-z0+1, x1-x0+1, y1-y0+1))
    res_array <- aperm(res_array, c(2,3,1))

    dimnames(res_array) <- list(x0:x1, y0:y1, z0:z1)
    res_array
}
