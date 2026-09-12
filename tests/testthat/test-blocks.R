context("block functions")

test_that("setBlock and setBlocks work with numeric and character ids", {
    sent_msgs <- c()
    # Mock mc_send for testing
    mock_send <- function(text) {
        sent_msgs <<- c(sent_msgs, text)
    }

    # Test numeric ID
    with_mock(
        mc_send = mock_send,
        {
            setBlock(10, 20, 30, 1, 0)
            expect_equal(tail(sent_msgs, 1), "world.setBlock(10,20,30,1,0)")

            setBlock(10, 20, 30, "1", "0")
            expect_equal(tail(sent_msgs, 1), "world.setBlock(10,20,30,1,0)")

            setBlocks(0, 0, 0, 1, 1, 1, 5)
            expect_equal(tail(sent_msgs, 1), "world.setBlocks(0,0,0,1,1,1,5)")
        }
    )

    # Test character ID (string block name)
    sent_msgs <- c()
    with_mock(
        mc_send = mock_send,
        {
            setBlock(10, 20, 30, "stone")
            expect_equal(tail(sent_msgs, 1), "world.setBlock(10,20,30,stone)")

            setBlock(10, 20, 30, "minecraft:stone")
            expect_equal(tail(sent_msgs, 1), "world.setBlock(10,20,30,minecraft:stone)")

            setBlocks(0, 0, 0, 1, 1, 1, "diamond_block")
            expect_equal(tail(sent_msgs, 1), "world.setBlocks(0,0,0,1,1,1,diamond_block)")

            # Test passing item data frame from find_item
            copper_item <- data.frame(name = "Block of Copper", id = "copper_block", style = 0, stringsAsFactors = FALSE)
            setBlock(10, 20, 30, copper_item)
            expect_equal(tail(sent_msgs, 1), "world.setBlock(10,20,30,copper_block)")

            setBlocks(0, 0, 0, 1, 1, 1, copper_item)
            expect_equal(tail(sent_msgs, 1), "world.setBlocks(0,0,0,1,1,1,copper_block)")
        }
    )
})

test_that("getBlock and getBlocks work with numeric and string responses", {
    # Test numeric response from server
    with_mock(
        mc_sendreceive = function(cmd) "1,0",
        {
            res <- getBlock(10, 20, 30)
            expect_equal(res, c(typeID = 1, style = 0))
            expect_type(res, "double")

            res_nostyle <- getBlock(10, 20, 30, include_style = FALSE)
            expect_equal(res_nostyle, c(typeID = 1))
        }
    )

    # Test character response from server (modern Minecraft block names)
    with_mock(
        mc_sendreceive = function(cmd) "stone,0",
        {
            res <- getBlock(10, 20, 30)
            expect_equal(res, c(typeID = "stone", style = "0"))
            expect_type(res, "character")

            res_nostyle <- getBlock(10, 20, 30, include_style = FALSE)
            expect_equal(res_nostyle, c(typeID = "stone"))
        }
    )

    # Test single item string response (no style)
    with_mock(
        mc_sendreceive = function(cmd) "stone",
        {
            res <- getBlock(10, 20, 30)
            expect_equal(res, c(typeID = "stone"))
        }
    )

    # Test fallback to world.getBlock when world.getBlockWithData returns 0 for modern blocks
    mock_sendreceive <- function(cmd) {
        if (startsWith(cmd, "world.getBlockWithData")) {
            return("0,0")
        } else if (startsWith(cmd, "world.getBlock")) {
            return("copper_block")
        }
        return("0")
    }
    with_mock(
        mc_sendreceive = mock_sendreceive,
        {
            res <- getBlock(10, 20, 30)
            expect_equal(res, c(typeID = "copper_block", style = "0"))

            res_nostyle <- getBlock(10, 20, 30, include_style = FALSE)
            expect_equal(res_nostyle, c(typeID = "copper_block"))
        }
    )

    # Test getBlocks with numeric response
    with_mock(
        mc_sendreceive = function(cmd) "1,1,1,1",
        {
            res <- getBlocks(0, 0, 0, 1, 0, 1)
            expect_equal(dim(res), c(2, 1, 2))
            expect_equal(res[1, 1, 1], 1)
        }
    )

    # Test getBlocks with character response
    with_mock(
        mc_sendreceive = function(cmd) "stone,stone,dirt,dirt",
        {
            res <- getBlocks(0, 0, 0, 1, 0, 1)
            expect_equal(dim(res), c(2, 1, 2))
            expect_equal(res[1, 1, 1], "stone")
            expect_equal(res[2, 1, 2], "dirt")
        }
    )
})
