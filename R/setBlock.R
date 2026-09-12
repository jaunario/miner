#' Place a block
#'
#' Place a block at position (x,y,z) by block type ID or block name
#'
#' @inheritParams find_item
#' @inheritParams getBlock
#'
#' @return None.
#'
#' @examples
#' \dontrun{
#' mc_connect()
#' h <- getHeight(0, 0)
#' setBlock(0, h, 0, 46)
#' setBlock(0, h, 0, "stone")
#' }
#'
#' @export

setBlock <- function(x, y, z, id, style=0)
{
    x <- floor(as.numeric(x))
    y <- floor(as.numeric(y))
    z <- floor(as.numeric(z))

    if (is.data.frame(id) || is.list(id)) {
        if ("style" %in% names(id) && (missing(style) || is.null(style) || style == 0)) {
            style <- id$style[1]
        }
        if ("id" %in% names(id)) {
            id <- id$id[1]
        } else if ("name" %in% names(id)) {
            id <- id$name[1]
        }
    }

    num_id <- suppressWarnings(as.numeric(id))
    if (!is.na(num_id)) {
        id <- floor(num_id)
        num_style <- suppressWarnings(as.numeric(style))
        if (!is.na(num_style)) {
            style <- floor(num_style)
        }
        if (is.null(style) || is.na(style)) style <- 0
        mc_send(merge_data("world.setBlock", x, y, z, id, style))
    } else {
        # Character block identifier (e.g. "stone" or "minecraft:stone")
        mc_send(merge_data("world.setBlock", x, y, z, as.character(id)))
    }
}


#' Place blocks in a cuboid
#'
#' Place blocks of a single type (specified by `id`) in the cuboid
#' with opposite corners at the positions (x0, y0, z0) and (x1, y1, z1).
#'
#' @inheritParams getBlocks
#' @inheritParams find_item
#'
#' @return None.
#'
#' @examples
#' \dontrun{
#' mc_connect()
#'
#' ice <- find_item(name = "Ice")
#'
#' h <- getHeight(0,0)
#' setBlocks(0, h, 0, 1, h + 3, 2, id = ice$id)
#' setBlocks(0, h, 0, 1, h + 3, 2, id = "ice")
#' }
#'
#' @export

setBlocks <- function(x0,y0,z0, x1,y1,z1,  id)
{
    x0 <- floor(as.numeric(x0))
    y0 <- floor(as.numeric(y0))
    z0 <- floor(as.numeric(z0))
    x1 <- floor(as.numeric(x1))
    y1 <- floor(as.numeric(y1))
    z1 <- floor(as.numeric(z1))

    if (is.data.frame(id) || is.list(id)) {
        if ("id" %in% names(id)) {
            id <- id$id[1]
        } else if ("name" %in% names(id)) {
            id <- id$name[1]
        }
    }

    num_id <- suppressWarnings(as.numeric(id))
    if (!is.na(num_id)) {
        id <- floor(num_id)
    } else {
        id <- as.character(id)
    }

    mc_send(merge_data("world.setBlocks", x0, y0, z0, x1, y1, z1, id))

}
