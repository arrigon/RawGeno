EXTRACTAFLP <-
function (all.dat, samples.names, TOL = 0.97, MAXBIN = max(all.dat$Size),                                             
    MINBIN = 0.2, probs = c(5e-04), freq = 1, who = "ReplicateID",                                                    
    thresh = 0, keep = T, RMIN = 50, RMAX = max(all.dat$Size),                                                        
    cutRFU = 30) {

    autoscoring<<-1

    MS = cbind(all.dat$Size, all.dat$sample.id, all.dat$Peak_height)                                                  
    MS = MS[order(MS[, 1], MS[, 2]), ]                                                                                
    MS = MS[MS[, 1] >= RMIN, ]                                                                                        
    MS = MS[MS[, 1] <= RMAX, ]                                                                                        

    bin = geno.clust(diff(MS[, 1]), MS[, 2], BinSize = c(MINBIN, MAXBIN), tol = TOL, verbose = F)                                                                              
    peaks.info = cbind(MS[, 1], MS[, 2], bin, cumsum(as.numeric(bin)), MS[, 3],rep(0,nrow(MS)))                                                                                                      

    colnames(peaks.info) = c("Size", "sample.id", "bin", "peak.nr", "height","filter")                                                                                                     
    n.bands.tot = max(peaks.info[, 4])                                                                                
    samples.tot = as.numeric(row.names(table(peaks.info[, 2])))                                                       
    samples.kept = samples.names[samples.tot]                                                                         
    lostsamples = samples.names[which(is.na(match(samples.names, samples.kept)))]                                                                                              

    b.height = tapply(abs(peaks.info[, 5]), data.frame(as.factor(peaks.info[, 4]), as.factor(peaks.info[, 2])), mean)                                                                       
    b.size = tapply(abs(peaks.info[, 1]), data.frame(as.factor(peaks.info[, 4]), as.factor(peaks.info[, 2])), mean)                                                                       
    b.homopl = tapply(abs(peaks.info[, 5]), data.frame(as.factor(peaks.info[, 4]), as.factor(peaks.info[, 2])), length)                                                                     
                                                             
    colnames(b.height) = samples.kept                                                                                 
    colnames(b.size) = samples.kept                                                                                   
    colnames(b.homopl) = samples.kept
                                                                                   
    n.bands.tot = nrow(b.size)                                                                                        
    n.peaks.tot = sum(as.numeric(b.size > 0), na.rm = T) 
                                                             
    b.height.raw = b.height                                                                                           
    set = b.height                                                                                                    
    sums = apply(set, 2, sum, na.rm = T)                                                                              
    med = median(sums)                                                                                                
    norm.factor = med/sums                                                                                            
    b.height = b.height * norm.factor
                                                                                 
    chck = apply(b.height, 1, mean, na.rm = T)                                                                        
    bin.means = apply(b.height, 1, mean, na.rm = T)                                                                   
    n.bands.RFU = nrow(b.size)                                                                                        
    b.size = b.size[chck >= cutRFU, ]                                                                                 
    b.height = b.height[chck >= cutRFU, ]                                                                             
    b.height.raw = b.height.raw[chck >= cutRFU, ]                                                                     
    b.homopl = b.homopl[chck >= cutRFU, ]                                                                             
    n.bands.RFU = n.bands.RFU - nrow(b.size)                                                                          
    torem = as.numeric(names(chck)[chck < cutRFU])
    peaks.info[!is.na(match(peaks.info[, 4], torem)), 6] = 1                                                                             
    peaks.info[!is.na(match(peaks.info[, 4], torem)), 4] = 0  

                                                        
    n.bands.repl = 0                                                                                                  
    if (who != "ReplicateID") {                                                                                       
        data.bin.test = b.size                                                                                        
        data.bin.test[data.bin.test > 0] = 1                                                                          
        data.bin.test[is.na(data.bin.test)] = 0                                                                       
        repl = samples.names[grep(who, samples.kept)]                                                                 
        target = gsub(who, "", repl)                                                                                  
        test = is.na(match(target, samples.kept))                                                                     
        target = target[test == FALSE]                                                                                
        repl = repl[test == FALSE]                                                                                    
        conserv = matrix(0, ncol = length(repl), nrow = nrow(data.bin.test))                                          
        glob = matrix(0, ncol = length(repl), nrow = nrow(data.bin.test))
	rownames(glob) = rownames(data.bin.test)
	rownames(conserv) = rownames(data.bin.test)
	if(ncol(glob)>0){
	  for (i in 1:length(target)) {                                                                                 
	      couple = cbind(data.bin.test[, samples.kept == target[i]], data.bin.test[, samples.kept == repl[i]])                                                             
	      out = apply(couple, 1, var)                                                                               
	      test = apply(couple, 1, sum)                                                                              
	      conserv[, i] = out                                                                                        
	      glob[, i] = test                                                                                          
	  }                                                                                                             
	  mism = 2 * conserv                                                                                            
	  mism2 = mism                                                                                                  
	  ErrRate = colMeans(mism2, na.rm = T)                                                                          
	  mism = mism[, ErrRate < 0.25]                                                                                 
	  conserv = 100 - 100 * rowMeans(mism, na.rm = T)                                                               
	  n.bands.repl = nrow(b.size)                                                                                   
	  b.size = b.size[which(conserv >= thresh), ]                                                                   
	  b.height = b.height[which(conserv >= thresh), ]                                                               
	  b.height.raw = b.height.raw[which(conserv >= thresh), ]                                                                                                         
	  b.homopl = b.homopl[which(conserv >= thresh), ]                                                               
	  tested = rowSums(glob[which(conserv >= thresh), ])                                                            
	  torem = as.numeric(names(which(conserv < thresh)))
	  peaks.info[!is.na(match(peaks.info[, 4], torem)), 6] = 2                                                                            
	  peaks.info[!is.na(match(peaks.info[, 4], torem)), 4] = 0
						  
	  if (keep == F) {                                                                                              
	      b.size = b.size[which(tested > 1), ]                                                                      
	      b.height = b.height[which(tested > 1), ]                                                                  
	      b.homopl = b.homopl[which(tested > 1), ]                                                                  
	      torem = as.numeric(names(which(tested <= 1)))
	      peaks.info[!is.na(match(peaks.info[, 4], torem)), 6] = 2                                                                        
	      peaks.info[!is.na(match(peaks.info[, 4], torem)), 4] = 0                                                                              
	  }                                                                                                             
	  n.bands.repl = n.bands.repl - nrow(b.size)
	}
    }                                                                                                                 
    n.bands.freq = 0                                                                                                  
    if (freq != 0) {                                                                                                  
        data.bin.test = b.size                                                                                        
        data.bin.test[data.bin.test > 0] = 1                                                                          
        data.bin.test[is.na(data.bin.test)] = 0                                                                       
        criterion = rowSums(data.bin.test)                                                                            
        nr.samples = ncol(b.size)                                                                                     
        n.bands.freq = nrow(b.size)                                                                                   
        data.bin.test = b.size                                                                                        
        data.bin.test[data.bin.test > 0] = 1                                                                          
        data.bin.test[is.na(data.bin.test)] = 0                                                                       
        criterion = rowSums(data.bin.test)
	test=criterion >= freq & criterion <= (nr.samples - freq)

        b.size = b.size[test, ]                                                                                                  
        b.height = b.height[test, ]                                                                                    
        b.height.raw = b.height.raw[test, ]                                                                                    
        b.homopl = b.homopl[test, ]                                                                                    
        torem = as.numeric(names(which(test==F)))
        peaks.info[!is.na(match(peaks.info[, 4], torem)), 6] = 3      
        peaks.info[!is.na(match(peaks.info[, 4], torem)), 4] = 0                                    
        n.bands.freq = n.bands.freq - nrow(b.size)                                                                    
    }                                                                                                                 
    samples.names = colnames(b.size)    
    sizes.out = round(rowMeans(b.size, na.rm = T), 2)                                                                           
    rownames(b.height) = sizes.out                                                                                    
    rownames(b.size) = sizes.out                                                                                      
    rownames(b.homopl) = sizes.out 
                                                                              
    b.size[is.na(b.size)] = 0                                                                                         
    b.homopl[is.na(b.homopl)] = 0                                                                                     
    b.height[is.na(b.height)] = 0                                                                                     
    b.height.raw[is.na(b.height.raw)] = 0                                                                             
    data.out = b.size                                                                                                 
    data.out[data.out > 0] = 1                                                                                        
    ErrRate = NA                                                                                                      
    if (who != "ReplicateID") {                                                                                       
        EBO = function(j) {                                                                                           
            d.test = dat.d[match(target[j], indivs), match(repl[j], indivs)]                                                                                              
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
    table.stats = cbind(c(n.bands.tot, nrow(data.out), 
			  n.bands.RFU, n.bands.repl, 
			  n.bands.freq, round(ErrRate, 4), 
			  round(Ibin, 4)), 
			c(100, round(100 * nrow(data.out)/n.bands.tot, 2), 
			  round(100 * n.bands.RFU/n.bands.tot, 2), 
			  round(100 * n.bands.repl/n.bands.tot, 2), 
			  round(100 * n.bands.freq/n.bands.tot, 2), 0, 0))                                                                                                    
    colnames(table.stats) = c("Counts", "PercentsOfTotal")                                                            
    rownames(table.stats) = c("InitialBinNr", "FinalBinNr", "RemvdLowIntensBins",                                     
        "RemvdNonReplicBins", "RemvdRareFrequentBins", "ErrorRateBo",                                                 
        "Ibin")                                                                                                       
    table.tech = data.frame(MaxBin = MAXBIN, MinBin = MINBIN,                                                         
        `LowIntensPeaks(%)` = probs, `LowFreqBins(NbInds)` = freq,                                                    
        `Reproducibility(%)` = thresh, ReplicateID = who, KeepUntestedBins = keep,                                    
        ScoringRangeMin = RMIN, ScoringRangeMax = RMAX)                                                               
    rownames(table.tech) = "Value"                                                                                    
    data.out = data.out[order(nrow(data.out):1), ]                                                                    
    b.size = b.size[order(nrow(b.size):1), ]                                                                          
    data.binary = list(data.binary = data.out, peaks.info = peaks.info,                                               
        data.hom = b.homopl, data.size = b.size, data.height = b.height,                                              
        data.height.raw = b.height.raw, table.stats = table.stats,
        table.tech = t(table.tech))
    assign("data.binary", data.binary, env = .GlobalEnv)
    if (length(samples.tot) < length(AFLP$samples.names)) {
        cat("Some samples were lost due to poor AFLP profiles. See in working directory for a list of lost samples\n")
        cat("Samples lost during scoring step:\n", file = "LostSamplesScoring.txt",
            append = T)
        write.table(lostsamples, "LostSamplesScoring.txt", sep = "\t",
            append = T, col.names = F, quote = F)
    }
}
