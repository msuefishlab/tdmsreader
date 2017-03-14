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


## References

python npTDMS package https://github.com/adamreeve/npTDMS

also see https://github.com/msuefishlab/tdmsviewer for example usage
