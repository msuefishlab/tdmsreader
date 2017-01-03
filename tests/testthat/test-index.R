library(tdmsreader)

test_that("index file is present", {
    f = file('file.tdms_index', 'rb')
    main = TdmsIndexFile$new(f)
    expect_equal(length(main$segments), 1)
    close(f)
})
