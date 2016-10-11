AllOK <-
function (...) {                                                                                                                                                                                                                                                                                                                                    
    if (modified == 0) {                                                                                                                                                           
        tkdestroy(tt)                                                                                                                                                              
    } else {                                                                                                                                                                         
        peaks.info = data.binary$peaks.info                                                                                                                                        
	xx <<- xx[order(xx[, 1]), ]
        binNR=rep(0,nrow(peaks.info))                                                                                                                                                        
        myfun = function(i, ...) {                                                                                                                                                 
            binNR[peaks.info[, 1] >= xx[i, 1] & peaks.info[, 1] <= xx[i, 2]] = i                                                                                                                                             
            binNR <<- binNR                                                                                                                                              
        }
	for(i in 1:nrow(xx)){
	  myfun(i)
	  }
	peaks.info[,4]=binNR
	peaks.info[,6]=binNR
	peaks.info[peaks.info[, 6] > 0, 6] = 1
	peaks.info[, 6] = 1 - peaks.info[, 6]
        peaks.infoB = peaks.info                                                                                                                                                   
        peaks.infoB = peaks.infoB[peaks.infoB[, 4] > 0, ]                                                                                                                          
        smps = as.numeric(names(table(peaks.infoB[, 2])))                                                                                                                          
        samples.kept = AFLP$samples.names[smps]                                                                                                                                    
        b.homopl = tapply(abs(peaks.infoB[, 5]), data.frame(as.factor(peaks.infoB[, 4]), as.factor(peaks.infoB[, 2])), length)                                                                                                                             
        b.size = tapply(abs(peaks.infoB[, 1]), data.frame(as.factor(peaks.infoB[, 4]), as.factor(peaks.infoB[, 2])), mean)                                                                                                                               
        b.height = tapply(abs(peaks.infoB[, 5]), data.frame(as.factor(peaks.infoB[, 4]), as.factor(peaks.infoB[, 2])), mean)                                                                                                                                                                                                                                                         
        b.height = b.height[order(nrow(b.size):1), ]                                                                                                                               
        b.size = b.size[order(nrow(b.size):1), ]                                                                                                                                   
        b.homopl = b.homopl[order(nrow(b.size):1), ]                                                                                                                               
        b.height.raw = b.height 
        sizes.out = round(rowMeans(b.size, na.rm = T), 2)                                                                                                                                                   
        set = b.height                                                                                                                                                             
        sums = apply(set, 2, sum, na.rm = T)                                                                                                                                       
        med = median(sums)                                                                                                                                                         
        norm.factor = med/sums                                                                                                                                                     
        b.height = b.height * norm.factor                                                                                                                                          
        colnames(b.height) = samples.kept                                                                                                                                          
        colnames(b.size) = samples.kept                                                                                                                                            
        colnames(b.homopl) = samples.kept                                                                                                                                          
        rownames(b.height) = sizes.out                                                                                                                                             
        rownames(b.size) = sizes.out                                                                                                                                               
        rownames(b.homopl) = sizes.out                                                                                                                                             
        b.size[is.na(b.size)] = 0                                                                                                                                                  
        b.homopl[is.na(b.homopl)] = 0                                                                                                                                              
        b.height[is.na(b.height)] = 0                                                                                                                                              
        data.out = b.size                                                                                                                                                          
        data.out[data.out > 0] = 1                                                                                                                                                 
        ErrRate = NA                                                                                                                                                               
        if (data.binary$table.tech[6] != "ReplicateID") {                                                                                                                          
            who = data.binary$table.tech[6]                                                                                                                                        
            EBO = function(j) {                                                                                                                                                    
                d.test = dat.d[match(target[j], indivs), match(repl[j],                                                                                                            
                  indivs)]                                                                                                                                                         
                sum(d.test)/nloc                                                                                                                                                   
            }                                                                                                                                                                      
            dat.d = as.matrix(dist(t(data.out), "manhattan"))                                                                                                                      
            indivs = rownames(dat.d)                                                                                                                                               
            repl = indivs[grep(who, indivs)]                                                                                                                                       
            target = gsub(who, "", repl)                                                                                                                                           
            nloc = nrow(data.out)                                                                                                                                                  
            ErrRate = median(sapply(1:length(target), EBO), na.rm = T)                                                                                                             
        }                                                                                                                                                                          
        IBIN = function(j, nloc) {                                                                                                                                                 
            d.glob = dat.d[j, -j]                                                                                                                                                  
            mean(d.glob)/nloc                                                                                                                                                      
        }                                                                                                                                                                          
        dat.d = as.matrix(dist(t(data.out), "manhattan"))                                                                                                                          
        indivs = rownames(dat.d)                                                                                                                                                   
        nloc = nrow(data.out)                                                                                                                                                      
        Ibin = median(sapply(1:length(indivs), IBIN, nloc))                                                                                                                        
        reviewed.stats = matrix(NA, 4, 2)                                                                                                                                          
        reviewed.stats[1, ] = c(nrow(data.binary$data.binary),                                                                                                                     
            100)                                                                                                                                                                   
        reviewed.stats[2, ] = c(nrow(data.out), 100 * round(nrow(data.out)/reviewed.stats[1,                                                                                       
            1], 2))                                                                                                                                                                
        reviewed.stats[3, ] = c(ErrRate, 0)                                                                                                                                        
        reviewed.stats[4, ] = c(Ibin, 0)                                                                                                                                           
        colnames(reviewed.stats) = c("Counts", "PercentsOfTotal")                                                                                                                  
        rownames(reviewed.stats) = c("InitialBinNr", "FinalBinNr",                                                                                                                 
            "ErrorRateBo", "Ibin")                                                                                                                                                 
        table.stats = list()                                                                                                                                                       
        table.stats$InitialBinning = data.binary$table.stats[[1]]
        table.stats$ReviewedBinning = reviewed.stats
        data.binary = list(data.binary = data.out, peaks.info = peaks.info,
            data.hom = b.homopl, data.size = b.size, data.height = b.height,
            data.height.raw = b.height.raw, table.stats = table.stats,
            table.tech = data.binary$table.tech)
        assign("data.binary", data.binary, env = .GlobalEnv)
        tkdestroy(tt)
        print(data.binary$table.stats)
    }
}
