library(tdmsreader)

test_that("get properties from tdms", {
    f = file('file.tdms', 'rb')
    main = TdmsFile$new(f)
    expect_equal(length(main$segments), 2)
    expect_equal(length(main$objects), 3)

    ret = main$objects[[ "/"]]$properties
    expect_equal(ret$name, "BBRACH_scn4aasg2_18dpf_A_2016_10_07_17_51_42")


    ret = main$objects[[ "/'Untitled'/'Dev1/ai0'"]]$properties
    expect_equal(ret$wf_increment, 1e-5)
    expect_equal(ret$wf_samples, 1e4)

    close(f)
})

test_that("get data from tdms", {
    f = file('file.tdms', 'rb')
    main = TdmsFile$new(f)

    main$read_data(f, 0, 1)
    r = main$objects[[ "/'Untitled'/'Dev1/ai0'"]]
    t = r$time_track(start = 0, end = 1)
    s = r$data
    png('test.png')
    print(length(t))
    print(length(s))
    plot(t, s, type='l')
    dev.off()


    close(f)
})

