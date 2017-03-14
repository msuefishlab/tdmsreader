# tdmsreader

[![Build Status](https://travis-ci.org/msuefishlab/tdmsreader.svg?branch=master)](https://travis-ci.org/msuefishlab/tdmsreader)

Read LabVIEW TDMS files using R


## Install

    install.packages('devtools')
    devtools::install_github('msuefishlab/tdmsreader')

## Example usage

    library(tdmsreader)
    f = file('tests/testthat/file.tdms', 'rb')
    main = TdmsFile$new(f)
    main$read_data(f, 0, 1)
    r = main$objects[[ "/'Untitled'/'Dev1/ai0'"]]
    t = r$time_track(start = 0, end = 1)
    s = r$data
    png('out.png')
    plot(t, s, xlab = 'Time', ylab = 'Volts')
    title('TDMS reader')
    dev.off()
    close(f)
   

## Screenshot

![](img/test.png)

## Notes

See https://github.com/msuefishlab/tdmsviewer for example usage

This only supports only a subset of the TDMS spec and has been tested on single channel 32bit float data

Feedback welcome!

## Credit

This app is largely a port of the python npTDMS package into R and shares the same LGPL license https://github.com/adamreeve/npTDMS

