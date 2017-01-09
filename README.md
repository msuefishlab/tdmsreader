# tdmsreader

[![Build Status](https://travis-ci.org/msuefishlab/tdmsreader.svg?branch=master)](https://travis-ci.org/msuefishlab/tdmsreader)

Read LabVIEW TDMS files using R


## Install

It is not an official package on CRAN, so you can use devtools to install

    devtools::install_github('msuefishlab/tdmsreader')

## Usage

A command line script named is also installed. It might not be installed to a normal bin directory, so you can locate it with

    Rscript -e "system.file('scripts', 'tdmsreader', package='tdmsreader')"

The usage of the script is

    tdmsreader input.tdms output.png

The command line script is a simple example of usage of the `tdmsreader` library


## Screenshot

![](img/test.png)


## References

python npTDMS package https://github.com/adamreeve/npTDMS

