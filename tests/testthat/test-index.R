library(tdmsreader)

test_that("get properties from tdms", {
    f = file('file.tdms', 'rb')
    main = TdmsFile$new(f)
    expect_equal(length(main$segments), 2)
    expect_equal(length(main$objects), 3)

    ret = main$objects[["/"]]$properties
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
    expect_equal(t[1:5], c(1e-5, 2e-5, 3e-5, 4e-5, 5e-5))
    expect_equal(s[1:5], c(0.01800260, 0.01964717, 0.01668694, 0.01833151, 0.01701586))

    close(f)
})

test_that("get data from tdms partial", {
  f <- file("file.tdms", "rb")
  main <- TdmsFile$new(f)

  main$read_data(f, 0.5, 1)
  r <- main$objects[["/'Untitled'/'Dev1/ai0'"]]
  t <- r$time_track(start = 0.5, end = 1)
  s <- r$data
  expect_equal(t[1:5], c(0.50001, 0.50002, 0.50003, 0.50004, 0.50005))
  expect_equal(s[1:5], c(0.002214717, 0.006490602, -0.003047911, 0.003859288, 0.004846030))


  close(f)
})

test_that("get data from tdms partial", {
  ## randomly guessed, or can be set randomly if it is not relevant to your data
  sample_rate = 10000
  f <- file("Data_sample_file_VTMS_testbed.tdms", "rb")
  main <- TdmsFile$new(f)
  main$read_data(f, 0, 1, sample_rate = sample_rate)

  ## example to list objects
  print(ls(main$objects))

  ## choose an entry from this list
  r = main$objects[[ "/'Signals'/'AC_Battery_Outputs_BT_1502[](Tair_testcham_return_degC)'"]]

  ## manually specify sample rate
  t = r$time_track(start = 0, end = 1, sample_rate=sample_rate)
  s = r$data
  png('out.png')
  plot(t, s, xlab = 'Time', ylab = 'Degrees')
  title('TDMS reader')
  dev.off()
  close(f)
})
