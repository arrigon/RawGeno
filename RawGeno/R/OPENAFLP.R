OPENAFLP <-
function (listfiles, dyecol = "B", format = "pksc") {
    if (any(which(ls() == "AFLP"))) {
        rm(AFLP)
        polyy <<- NULL
    }
    if (format == 'GS') {
        listfiles = sort(listfiles)
        namescol = c("DyeSample_peak", "Minutes", "Size", "Peak_height", 
            "Peak_area", "Data_point")
        dat.in = c(0, 0, 0, 0, 0, 0, 0)
        for (i in 1:length(listfiles)) {
            dat = cbind(i, try(read.delim(listfiles[i], header = F)[, 
                c(1, 3, 4)], silent = T))
            COL = substr(as.character(dat[, 2]), 0, 1)
            dat = dat[COL == dyecol, ]
            dat.in = rbind(dat.in, dat)
        }
        all.dat = dat.in[-1, -2]
        colnames(all.dat) = c("sample.id", "Size", "Peak_height")
        all.dat = all.dat[all.dat[, 2] != 0, ]
        all.dat = all.dat[order(all.dat[, 2], all.dat[, 1]), 
            ]
        files = sort(basename(listfiles))
        pops = substr(files, 0, 4)
        check = substr(levels(factor(all.dat$sample.id))[-1], 
            0, 4)
        AFLP = list(all.dat = all.dat, pops = pops, samples.names = basename(listfiles))
      } 
    if(format == 'pksc'){
        data = read.delim(listfiles, header = T)
        dye = data$Dye.Sample.Peak
        dye = substr(as.character(dye), 0, 1)
        data = data[dye == dyecol, ]
        if (is.na(match("Area.in.BP", colnames(data)))) {
            cat("Areas and Widths of AFLP peaks are absent: scoring can be processed,\nbut vizualisation of binning will be impossible.\nYou can correct this by tuning export options in PeakScanner.\n")
            all.dat = data.frame(sample.id = data$Sample.File.Name, 
            Size = data$Size, Peak_height = data$Height)
        } else {
            if (any(is.na(match(c("Well", "Plate.Name"), colnames(data))))) {
                all.dat = data.frame(sample.id = data$Sample.File.Name, 
                  Size = data$Size, Peak_height = data$Height, 
                  Peak_area = data$Area.in.BP, Peak_width = data$Width.in.BP)
            } else {
                all.dat = data.frame(sample.id = data$Sample.File.Name, 
                Size = data$Size, Peak_height = data$Height, 
                Peak_area = data$Area.in.BP, Peak_width = data$Width.in.BP, 
                Plate = data$Plate.Name, Well = data$Well)
            }
        }
        all.dat = all.dat[is.na(all.dat[, 2]) == F, ]
        all.dat = all.dat[all.dat[, 2] != 0, ]
        files = levels(as.factor(as.character(all.dat$sample.id)))
        all.dat[, 1] = as.numeric(as.factor(as.character(all.dat$sample.id)))
        files = sort(files)
        files = gsub(".fsa", "", files)
        all.dat = all.dat[order(all.dat[, 2], all.dat[, 1]), ]
        pops = substr(files, 0, 4)
        AFLP = list(all.dat = all.dat, pops = pops, samples.names = files)
    }
    if(format == 'GeneMarker'){
      data = read.delim(listfiles, header = T, skip=12)
      dye = data$Dye
      dye = substr(as.character(dye), 0, 1)
      data = data[dye == dyecol, ]
      if (is.na(match("Area", colnames(data)))) {
	  cat("Areas and Widths of AFLP peaks are absent: scoring can be processed,\nbut vizualisation of binning will be impossible.\n")
	  all.dat = data.frame(sample.id = data$Sample, 
				Size = data$Size, 
				Peak_height = data$Height)
        } else {
	  all.dat = data.frame(sample.id = data$Sample, 
			       Size = data$Size, 
			       Peak_height = data$Height, 
			       Peak_area = data$Area, 
			       Peak_width = abs(data$End - data$Start))
        }
	all.dat = all.dat[is.na(all.dat[, 2]) == F, ]
	all.dat = all.dat[all.dat[, 2] != 0, ]
	files = levels(as.factor(as.character(all.dat$sample.id)))
	all.dat[, 1] = as.numeric(as.factor(as.character(all.dat$sample.id)))
	files = sort(files)
	files = gsub(".fsa", "", files)
	all.dat = all.dat[order(all.dat[, 2], all.dat[, 1]), ]
	pops = substr(files, 0, 4)
	AFLP = list(all.dat = all.dat, pops = pops, samples.names = files)
      }
    assign("AFLP", AFLP, env = .GlobalEnv)
}
