library(tdmsreader)

test_that("index file is present", {
    f = file('file.tdms', 'rb')
    i = file('file.tdms_index', 'rb')
    main = TdmsFile$new(f, i)
    expect_equal(length(main$segments), 1)
    close(f)
    close(i)
})

