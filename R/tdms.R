#!/usr/bin/env Rscript

library(futile.logger)
library(R6)

fl = flog.debug


tdsDataType <- list(
    list(length = 0, id = 0, name = "tdsTypeVoid"),
    list(length = 1, id = 1, name = "tdsTypeI8"),
    list(length = 2, id = 2, name = "tdsTypeI16"),
    list(length = 4, id = 3, name = "tdsTypeI32"),
    list(length = 8, id = 4, name = "tdsTypeI64"),
    list(length = 1, id = 5, name = "tdsTypeU8"),
    list(length = 2, id = 6, name = "tdsTypeU16"),
    list(length = 4, id = 7, name = "tdsTypeU32"),
    list(length = 8, id = 8, name = "tdsTypeU64"),
    list(length = 4, id = 9, name = "tdsTypeSingleFloat"),
    list(length = 8, id = 10, name = "tdsTypeDoubleFloat"),
    list(length = 16, id = 68, name = "tdsTypeTimeStamp")
)

get_type <- function(id) {
    for (elt in tdsDataType) {
        if (elt$id == id) {
            return(elt)
        }
    }
    return(NULL)
}

read_string <- function(f) {
    s = readBin(f, integer(), size = 4)
    readChar(f, s)
}


read_type <- function(f, type) {
    s = 0
    if (type == 12 || type == 10) {
        s = readBin(f, numeric(), size = 8)
    } else if (type == 25 || type == 9) {
        s = readBin(f, numeric(), size = 4)
    } else if (type == 4 || type == 8) {
        s = readBin(f, integer(), size = 8)
    } else if (type == 3 || type == 7) {
        s = readBin(f, integer(), size = 4)
    } else if (type == 2 || type == 6) {
        s = readBin(f, integer(), size = 2)
    } else if (type == 1 || type == 5) {
        s = readBin(f, integer(), size = 1)
    } else if (type == 68) {
        s_frac = readBin(f, integer(), size = 8)
        s_t = readBin(f, integer(), size = 8)
        s = s_t + s_frac / 2 ^ 64
    }
    return(s)
}

#' TdmsFile class
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
#' @export
TdmsFile <- R6Class("TdmsFile",
    public = list(
        objects = new.env(parent = emptyenv()),
        segments = list(),
        initialize = function(file) {
            self$segments = list()
            self$objects = new.env(parent = emptyenv())
            self$read_segments(file)
        },
        read_segments = function(file) {
            i = 0
            previous_segment = NULL
            while (TRUE) {
                fl("PASS %d %d", i, seek(file))
                segment = TdmsSegment$new(file)
                if (segment$eof == 1) {
                    break
                }
                segment$read_metadata(file, self$objects, previous_segment)
                self$segments[[length(self$segments) + 1]] = segment
                previous_segment = segment
                if (is.null(segment$next_segment_pos)) {
                    break
                } else {
                    seek(file, segment$next_segment_pos)
                }
                i = i + 1
            }
        },
        read_data = function(file, start = NULL, end = NULL) {
            for (elt in ls(self$objects)) {
                obj = self$objects[[elt]]
                if(obj$has_data) {
                    obj$initialize_data(start, end)
                }
            }

            for (segment in self$segments) {
                segment$read_raw_data(file, start, end)
            }
        }
    )
)

#' TdmsSegment class
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
#' @export
TdmsSegment <- R6Class("TdmsSegment",
    public = list(
        position = 0,
        version = '',
        eof = 0,
        ordered_objects = list(),
        final_chunk_proportion = 1.0,
        kTocMetaData = 0,
        kTocRawData = 0,
        kTocInterleavedData = 0,
        kTocDAQmxRawData = 0,
        kTocBigEndian = 0,
        kTocNewObjList = 0,
        next_segment_offset = 0,
        raw_data_offset = 0,
        data_position = 0,
        next_segment_pos = 0,
        num_chunks = 0,

        initialize = function(f) {
            self$position = 0
            self$version = ''
            self$eof = 0
            self$ordered_objects = list()
            self$final_chunk_proportion = 1.0

            self$next_segment_offset = 0
            self$raw_data_offset = 0
            self$data_position = 0
            self$next_segment_pos = 0
            self$num_chunks = 0


            self$position = seek(f)
            self$version = readChar(f, 4)
            if (length(self$version) == 0) {
                self$eof = 1
                return (0)
            }

            if (self$version != "TDSh" && self$version != "TDSm") {
                stop("File format error (file)")
            }

            kTocType = readBin(f, integer(), size = 4)
            fl('kToc %d %d', kTocType, seek(f))
            self$kTocMetaData = bitwAnd(kTocType, bitwShiftL(1, 1))
            self$kTocRawData = bitwAnd(kTocType, bitwShiftL(1, 3))
            self$kTocDAQmxRawData = bitwAnd(kTocType, bitwShiftL(1, 7))
            self$kTocInterleavedData = bitwAnd(kTocType, bitwShiftL(1, 5))
            self$kTocBigEndian = bitwAnd(kTocType, bitwShiftL(1, 6))
            self$kTocNewObjList = bitwAnd(kTocType, bitwShiftL(1, 2))


            self$version = readBin(f, integer(), size = 4)
            fl("version %d", self$version)
            self$next_segment_offset = readBin(f, 'int', size = 8, endian = 'little')
            self$raw_data_offset = readBin(f, 'int', size = 8, endian = 'little')


            lead_size = 7 * 4
            fl("raw_data_offset %d next_segment_offset %d", self$raw_data_offset, self$next_segment_offset)
            fl("raw_data_offset %016x next_segment_offset %016x", self$raw_data_offset, self$next_segment_offset)
            self$data_position = self$position + lead_size + self$raw_data_offset
            self$next_segment_pos = self$position + self$next_segment_offset + lead_size
        },

        calculate_chunks = function() {
            ds = lapply(self$ordered_objects, function(elt) {
                return(elt$data_size)
            })
            data_size = sum(unlist(ds))


            total_data_size = self$next_segment_offset - self$raw_data_offset
            fl('total size %d data_size %d', total_data_size, data_size)

            if (data_size < 0 || total_data_size < 0) {
                stop("Negative data size")
            } else if (data_size == 0) {
                if (total_data_size != data_size) {
                    stop("Zero channel data size but non-zero data length based on segment offset.")
                }
                self$num_chunks = 0
                return
            }
            chunk_remainder = total_data_size %% data_size
            if (chunk_remainder == 0) {
                self$num_chunks = total_data_size %/% data_size
                for (o in self$ordered_objects) {
                    if (o$has_data) {
                        o$tdms_object$number_values = o$tdms_object$number_values + o$number_values * self$num_chunks
                    }
                }
            } else {
                flog.error("Data size %d is not a multiple of the chunk size %d", total_data_size, data_size)
                self$num_chunks = 1 + total_data_size %/% data_size
                self$final_chunk_proportion = chunk_remainder / data_size

                for (obj in self$ordered_objects) {
                    if (obj$has_data) {
                        obj$tdms_object$number_values = obj$tdms_object$number_values +
                          (obj$number_values * (self$num_chunks - 1) +
                          (obj$number_values * self$final_chunk_proportion))
                    }
                }
            }
            fl("num_chunks %d", self$num_chunks)
        },

        read_metadata = function(f, objects, previous_segment=NULL) {
            if (!self$kTocMetaData) {
                if (!is.null(previous_segment)) {
                    self$ordered_objects = previous_segment$ordered_objects
                }
                else {
                    stop("Error no metadata from previous")
                }
                self$calculate_chunks()
                return
            }
            if (!self$kTocNewObjList) {
                if (!is.null(previous_segment)) {
                    for (elt in ls(previous_segment$ordered_objects)) {
                        self$ordered_objects[[elt]] = previous_segment$ordered_objects[[elt]]$clone()
                    }
                }
                else {
                    stop("Error no previous objects")
                }
            }

            num_objects = readBin(f, integer(), size = 4)
            if (num_objects > 0) {
                for (j in 1:num_objects) {
                    object_path = read_string(f)
                    if (object_path %in% ls(objects)) {
                        obj = objects[[object_path]]
                    } else {
                        obj = TdmsObject$new(object_path)
                        objects[[object_path]] = obj
                    }
                    updating_existing = FALSE
                    if (!self$kTocNewObjList) {
                        obj_index = -1
                        for (i in 1:length(self$ordered_objects)) {
                            if (self$ordered_objects[[i]] == obj) {
                                obj_index = i
                            }
                        }
                        if (obj_index != -1) {
                            updating_existing = TRUE
                            fl("Updating object in segment list")
                            segment_obj = self$ordered_objects[[i]]
                        }
                    }
                    if (!updating_existing) {
                        if (!is.null(obj$previous_segment_object)) {
                            fl("Copying previous segment object")
                            segment_obj = obj$previous_segment_object$clone()
                        } else {
                            fl("Creating new segment object")
                            segment_obj = TdmsSegmentObject$new(obj)
                        }
                        self$ordered_objects[[length(self$ordered_objects) + 1]] = segment_obj
                    }
                    segment_obj$read_metadata(f)
                    obj$previous_segment_object = segment_obj
                }
            }

            self$calculate_chunks()
        },

        read_raw_data = function(f, start = NULL, end = NULL) {
            if (!self$kTocRawData) {
                fl("No raw data in segment")
                return
            }
            seek(f, self$data_position)
            total_data_size = self$next_segment_offset - self$raw_data_offset
            num_elts = total_data_size / 8

            flag = FALSE
            tol = 1e-5

            if (self$num_chunks > 0) {
                for (i in 1:self$num_chunks) {
                    for (obj in self$ordered_objects) {
                        if (obj$has_data) {
                            n = obj$number_values
                            inc = obj$tdms_object$properties[['wf_increment']]
                            tr = obj$tdms_object$read_so_far
                            s = 1
                            e = n

                            if((tr + n*inc) < start) {
                                flog.error("skipping s %f e %f", tr, tr + n*inc)
                                obj$read_values(f, n)
                                obj$tdms_object$read_so_far = tr + n*inc
                                break
                            }
                            if(tr > end) {
                                flag = 1
                                break
                            }

                            if((tr + n*inc) > start && tr < start) {
                                s = n - as.integer((tr + n*inc - start) / inc)
                                flog.error("setting s %f tr %f int %f end %f mod %f", s, tr, n*inc, start, tr + n*inc - start)
                            }
                            if((tr + n*inc) > end && tr < end && (tr + n*inc - end) > tol) {
                                e = n - as.integer((tr + n*inc - end) / inc)
                                flog.error("setting e %f tr %f int %f end %f mod %f", e, tr, n*inc, end, tr + n*inc - end)
                            }
                            flog.error("reading time %f e %f", tr, tr + n*inc)
                            vals = obj$read_values(f, n)
                            flog.error("reading vals pos %d:%d", s, e)
                            vals = vals[s:e]
                            flog.error("len vals %d", length(vals))

                            obj$tdms_object$update_data(vals)
                            obj$tdms_object$read_so_far = tr + length(vals)*inc
                        }
                    }
                    if (flag) {
                        break
                    }
                }
            }
            else {
                flog.error('No chunks')
            }
        }
    )
)

#' TdmsObject class
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
#' @export
TdmsObject <- R6Class("TdmsObject",
    lock_object = FALSE,
    public = list(
        path = NULL,
        data = NULL,
        properties = new.env(parent = emptyenv()),
        dimension = 1,
        data_type = NULL,
        has_data = FALSE,
        read_so_far = 0,
        number_values = 0,
        data_insert_position = 1,
        previous_segment_object = NULL,
        initialize = function(path) {
            self$path = path
            self$data = NULL
            self$properties = new.env(parent = emptyenv())
            self$dimension = 1
            self$read_so_far = 0
            self$data_type = NULL
            self$has_data = FALSE
            self$number_values = 0
            self$data_insert_position = 1
            self$previous_segment_object = NULL
        },
        update_data = function(d) {
            p = self$data_insert_position
            s = p + length(d) - 1
            self$data[p:s] = d
            self$data_insert_position = self$data_insert_position + length(d)
        },
        time_track = function(absolute_time = FALSE, accuracy = 'ns', start = NULL, end = NULL) {
            increment = self$properties[['wf_increment']]
            offset = self$properties[['wf_start_offset']]
            len = length(self$data)
            num_vals = (end - start) / self$properties[['wf_increment']]
            ret = (1:num_vals * increment) + offset + start
            return (ret[1:length(self$data)])
        },
        initialize_data = function(start = NULL, end = NULL) {
            if (self$number_values == 0) {
                # non-data or metadata segment
            }
            num_vals = (end - start) / self$properties[['wf_increment']]
            if(num_vals > self$number_values) {
                flog.error("Start/end bigger than specified data")
                num_vals = self$number_values
            }
            self$data = numeric(num_vals)
        }
    )
)

#' TdmsSegmentObject class
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
#' @export
TdmsSegmentObject <- R6Class("TdmsSegmentObject",
    public = list(
        number_values = 0,
        data_size = 0,
        has_data = TRUE,
        dimension = 1,
        data_type = NULL,
        tdms_object = NULL,
        prop_type = NULL,
        initialize = function(object) {
            self$has_data = TRUE
            self$data_size = 0
            self$number_values = 0
            self$data_type = NULL
            self$prop_type = NULL
            self$dimension = 1
            self$tdms_object = object
        },
        read_metadata = function(f) {
            raw_data_index = readBin(f, integer(), size = 4)
            if (raw_data_index == -1) {
                self$has_data = FALSE
            } else if (raw_data_index == 0) {
                fl("Object has same data structure as in previous segment")
                self$has_data = TRUE
            } else {
                self$has_data = TRUE
                self$tdms_object$has_data = TRUE

                s = readBin(f, integer(), size = 4)
                self$data_type = get_type(s)
                self$dimension = readBin(f, integer(), size = 4)
                self$number_values = readBin(f, integer(), size = 8)

                if (self$data_type$name == "tdsTypeString") {
                    self$data_size = readBin(f, integer(), size = 8)
                } else {
                    self$data_size = self$number_values * self$data_type$len * self$dimension
                }

            }
            fl("num_val %d data_size %d", self$number_values, self$data_size)

            num_properties = readBin(f, integer(), size = 4)
            if (num_properties > 0) {
                for (i in 1:num_properties) {
                    prop_name = read_string(f)
                    self$prop_type = readBin(f, integer(), size = 4)

                    if (self$prop_type == 32) {
                        s = read_string(f)
                        fl("%s = %s", prop_name, s)
                        self$tdms_object$properties[[prop_name]] = s
                    } else {
                        s = read_type(f, self$prop_type)
                        fl("%s = %f %d [ns]", prop_name, s, self$prop_type)
                        self$tdms_object$properties[[prop_name]] = s
                    }
                }
            }
        },
        read_values = function(f, n) {
            readBin(f, numeric(), n, size = 8)
        }
    )
)
