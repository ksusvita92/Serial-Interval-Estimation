# Estimate serial interval from simulated outbreaks


require(dplyr)
require(igraph)
require(adegenet)
require(ape)



simOutbreak <- function(initial_sus = 200,
                        genint_params, # vector in mean, sigma
                        outbreak_duration = 100,
                        R0 = 2,
                        mutation_rate = 1e-4,
                        seq_length = 1e4,
                        minimum_case = 10,
                        .plot = FALSE){
  ##FROM OUTBREAKER
  theoutbreak <- function(R0, infec.curve, n.hosts=200, duration=50,
                          seq.length, mu.transi=1e-4, mu.transv=mu.transi/2,
                          rate.import.case=0.01, diverg.import=10, group.freq=1,
                          stop.once.cleared=TRUE){
    ## HANDLE ARGUMENTS ##
    ## handle group sizes
    if(any(group.freq<0)) stop("negative group frequencies provided")
    group.freq <- group.freq/sum(group.freq)
    K <- length(group.freq)
    ## host.group <- sample(1:K, size=n.hosts, prob=group.freq, replace=TRUE)
    R0 <- rep(R0, length=K) # recycle R0
    
    ## normalize gen.time
    infec.curve <- infec.curve/sum(infec.curve)
    infec.curve <- c(infec.curve, rep(0, duration)) # make sure dates go all the way
    t.clear <- which(diff(infec.curve<1e-10)==1) # time at which infection is cleared
    
    ## GENETIC FUNCTIONS ##
    NUCL <- as.DNAbin(c("a","t","c","g"))
    TRANSISET <- list('a'=as.DNAbin('g'), 'g'=as.DNAbin('a'), 'c'=as.DNAbin('t'), 't'=as.DNAbin('c'))
    TRANSVSET <- list('a'=as.DNAbin(c('c','t')), 'g'=as.DNAbin(c('c','t')), 'c'=as.DNAbin(c('a','g')), 't'=as.DNAbin(c('a','g')))
    
    
    ## AUXILIARY FUNCTIONS ##
    ## generate sequence from scratch
    seq.gen <- function(){
      ##res <- list(sample(NUCL, size=seq.length, replace=TRUE)) # DNAbin are no longer lists by default
      res <- sample(NUCL, size=seq.length, replace=TRUE)
      class(res) <- "DNAbin"
      return(res)
    }
    
    ## create substitutions for defined SNPs - no longer used
    substi <- function(snp){
      res <- sapply(1:length(snp), function(i) sample(setdiff(NUCL,snp[i]),1)) # ! sapply does not work on DNAbin vectors directly
      class(res) <- "DNAbin"
      return(res)
    }
    
    ## create transitions for defined SNPs
    transi <- function(snp){
      res <- unlist(TRANSISET[as.character(snp)])
      class(res) <- "DNAbin"
      return(res)
    }
    
    ## create transversions for defined SNPs
    transv <- function(snp){
      res <- sapply(TRANSVSET[as.character(snp)],sample,1)
      class(res) <- "DNAbin"
      return(res)
    }
    
    ## duplicate a sequence (including possible mutations)
    seq.dupli <- function(seq, T){ # T is the number of time units between ancestor and decendent
      ## transitions ##
      n.transi <- rbinom(n=1, size=seq.length*T, prob=mu.transi) # total number of transitions
      if(n.transi>0) {
        idx <- sample(1:seq.length, size=n.transi, replace=FALSE)
        seq[idx] <- transi(seq[idx])
      }
      
      ## transversions ##
      n.transv <- rbinom(n=1, size=seq.length*T, prob=mu.transv) # total number of transitions
      if(n.transv>0) {
        idx <- sample(1:seq.length, size=n.transv, replace=FALSE)
        seq[idx] <- transv(seq[idx])
      }
      return(seq)
    }
    
    ## define the group of 'n' hosts
    choose.group <- function(n){
      out <- sample(1:K, size=n, prob=group.freq, replace=TRUE)
      return(out)
    }
    
    
    ## MAIN FUNCTION ##
    ## initialize results ##
    dynam <- data.frame(nsus=integer(duration+1), ninf=integer(duration+1), nrec=integer(duration+1))
    rownames(dynam) <- 0:duration
    res <- list(n=1, dna=NULL, onset=NULL, id=NULL, ances=NULL, dynam=dynam)
    res$dynam$nsus[1] <- n.hosts-1
    res$dynam$ninf[1] <- 1
    res$onset[1] <- 0
    res$id <- 1 # id of infected individuals
    res$ances <- NA
    res$group <- choose.group(1)
    EVE <- seq.gen()
    res$dna <- matrix(seq.dupli(EVE, diverg.import),nrow=1)
    class(res$dna) <- "DNAbin"
    res$status <- c("I", rep("S", n.hosts-1)) # will be I, S, or R
    
    
    ## run outbreak ##
    for(t in 1:duration){
      ## DETERMINE NEW INTERNAL INFECTIONS ##
      ## individual force of infection - purely based on symptom onset
      indivForce <- infec.curve[t-res$onset+1]
      
      ## temporal (spatial) force of infection * R0
      indivForce <- indivForce * R0[res$group]
      
      ## global force of infection (R0 \sum_j I_t^j / N)
      N <- res$dynam$nrec[t] + res$dynam$ninf[t] + res$dynam$nsus[t] # this may change because of imports
      globForce <- sum(indivForce)/N
      
      ## stop if no ongoing infection in the population
      if(stop.once.cleared && (globForce < 1e-12)) break;
      
      ## compute proba of infection for each susceptible
      p <- 1-exp(-globForce)
      
      ## number of new infections
      nbNewInf <- rbinom(1, size=res$dynam$nsus[t], prob=p)
      
      
      ## HANDLE NEW INTERNAL INFECTIONS ##
      if(nbNewInf>0){
        ## dates of new infections ##
        res$onset <- c(res$onset, rep(t,nbNewInf))
        
        ## identify the infectors of the new cases ##
        newAnces <- sample(res$id, size=nbNewInf, replace=TRUE, prob=indivForce)
        res$ances <- c(res$ances,newAnces)
        
        ## find the groups of the new cases ##
        newGroup <- choose.group(nbNewInf)
        res$group <- c(res$group,newGroup)
        
        ## id of the new cases ##
        areSus <- which(res$status=="S") # IDs of susceptibles
        newId <- sample(areSus, size=nbNewInf, replace=FALSE)
        res$id <- c(res$id, newId)
        res$status[newId] <- "I"
        
        ## dna sequences of the new cases ##
        ## molecular clock / generation
        ## newSeq <- t(sapply(match(newAnces, res$id), function(i) seq.dupli(res$dna[i,], 1)))
        ## molecular clock / time unit
        newSeq <- t(sapply(match(newAnces, res$id), function(i) seq.dupli(res$dna[i,], t-res$onset[match(newAnces, res$id)])))
        res$dna <- rbind(res$dna, newSeq)
      }
      
      
      ## IMPORTED CASES ##
      ## number of imported cases
      nbImpCases <- rpois(1, rate.import.case)
      if(nbImpCases>0){
        ## dates of imported cases
        res$onset <- c(res$onset, rep(t, nbImpCases))
        
        ## ancestries of the imported cases
        res$ances <- c(res$ances, rep(NA, nbImpCases))
        
        ## id of the imported cases
        newId <- seq(N+1, by=1, length=nbImpCases)
        res$id <- c(res$id, newId)
        
        ## status of new cases
        res$status[newId] <- "I"
        
        ## group of the imported cases
        res$group <- c(res$group, choose.group(nbImpCases))
        
        ## dna sequences of the new infections
        newSeq <- t(sapply(1:nbImpCases, function(i) seq.dupli(EVE, diverg.import)))
        res$dna <- rbind(res$dna, newSeq)
      }
      
      
      ## set recovered status ##
      res$status[res$id[(t-res$onset) >= t.clear]] <- "R"
      
      ## update nb of infected, recovered, etc.
      res$dynam$nrec[t+1] <- sum(res$status=="R")
      res$dynam$ninf[t+1] <- sum(res$status=="I")
      res$dynam$nsus[t+1] <- sum(res$status=="S")
    } # end for
    
    
    ## SHAPE AND RETURN OUTPUT ##
    ## data need to be reordered so that res$id is 1:res$n
    res$n <- nrow(res$dna)
    res$ances <- match(res$ances, res$id)
    res$id <- 1:res$n
    res$xy <- res$inf.xy # don't keep entire distribution, not right order anymore anyway
    res$inf.xy <- NULL # simpler to just call coords 'xy'
    res$status <- NULL # we don't need this
    res$recover <- t.clear+res$onset
    
    findNmut <- function(i){
      if(!is.na(res$ances[i]) && res$ances[i]>0){
        out <- dist.dna(res$dna[c(res$id[i],res$ances[i]),], model="raw")*ncol(res$dna)
      } else {
        out <- NA
      }
      return(out)
    }
    
    ##res$nmut <- sapply(1:res$n, function(i) dist.dna(res$dna[c(res$id[i],res$ances[i]),], model="raw"))*ncol(res$dna)
    res$nmut <- sapply(1:res$n, function(i) findNmut(i))
    res$ngen <- rep(1, length(res$ances)) # number of generations
    res$call <- match.call()
    ## if(tree){
    ##     res$tree <- fastme.ols(dist.dna(res$dna, model="TN93"))
    ##     res$tree <- root(res$tree,"1")
    ## }
    
    
    class(res) <- "simOutbreak"
    return(res)
    
  } # end simOutbreak
  as.igraph.simOutbreak <- function(x, edge.col="black", col.edge.by="dist", vertex.col="orange",
                                    edge.col.pal=NULL, annot=c("dist","n.gen"), sep="/", ...){
    ## if(!require(igraph)) stop("package igraph is required for this operation")
    ## if(!require(ape)) stop("package ape is required for this operation")
    if(!inherits(x,"simOutbreak")) stop("x is not a tTree object")
    ## if(!require(adegenet)) stop("adegenet is required")
    if(!col.edge.by %in% c("dist","n.gen","prob")) stop("unknown col.edge.by specified")
    
    ## GET DAG ##
    from <- as.character(x$ances)
    to <- as.character(x$id)
    isNotNA <- !is.na(from) & !is.na(to)
    vnames <- unique(c(from,to))
    vnames <- vnames[!is.na(vnames)]
    dat <- data.frame(from,to,stringsAsFactors=FALSE)[isNotNA,,drop=FALSE]
    out <- graph.data.frame(dat, directed=TRUE, vertices=data.frame(names=vnames))
    
    ## from <- as.character(x$ances)
    ## to <- as.character(x$id)
    ## dat <- data.frame(from,to,stringsAsFactors=FALSE)[!is.na(x$ances),]
    ## out <- graph.data.frame(dat, directed=TRUE)
    
    ## SET VERTICE INFO ##
    ## labels
    V(out)$label <- V(out)$name
    
    ## dates
    names(x$onset) <- x$id
    V(out)$date <- x$onset[V(out)$name]
    
    ## ## groups
    ## names(x$group) <- x$id
    ## V(out)$group <- x$group[V(out)$name]
    
    ## colors
    V(out)$color <- vertex.col
    ## V(out)$color <- fac2col(factor(V(out)$group), col.pal=vertex.col.pal)
    
    
    ## SET EDGE INFO ##
    ## genetic distances to ancestor
    E(out)$dist <- x$nmut[!is.na(x$ances)]
    
    ## number of generations to ancestor
    E(out)$ngen <- x$ngen[!is.na(x$ances)]
    
    ## colors
    if(is.null(edge.col.pal)){
      edge.col.pal <- function(n){
        return(grey(seq(0.75,0,length=n)))
      }
    }
    if(col.edge.by=="dist") edge.col <- num2col(E(out)$dist, col.pal=edge.col.pal, x.min=0, x.max=1)
    
    ## labels
    n.annot <- sum(annot %in% c("dist","n.gen"))
    lab <- ""
    if(!is.null(annot) && n.annot>0){
      if("dist" %in% annot) lab <- E(out)$dist
      if("n.gen" %in% annot) lab <- paste(lab, x$ngen[!is.na(x$ances)], sep=sep)
    }
    lab <- sub(paste("^",sep,sep=""),"",lab)
    E(out)$label <- lab
    
    ## SET LAYOUT ##
    attr(out, "layout") <- layout.fruchterman.reingold(out, params=list(minx=V(out)$date, maxx=V(out)$date))
    
    return(out)
  } # end as.igraph.simOutbreak
  
  
  ##GENERATE OUTBREAK
  #repeat outbreak if doesn't require minimum cases
  n <- 0; i <- 1
  w <- EpiEstim::discr_si(k = seq(0, outbreak_duration),
                          mu = genint_params[1],
                          sigma = genint_params[2])
  while(n < minimum_case){
    myoutbreak <- tryCatch(theoutbreak(R0 = R0,
                                       infec.curve = w,
                                       n.hosts = initial_sus,
                                       duration = outbreak_duration,
                                       seq.length = seq_length,
                                       mu.transi = mutation_rate),
                           error = function(e){
                             list(n = -1, msg = conditionMessage(e))
                           })
    n <- myoutbreak$n
    if(n < minimum_case){
      if(n == -1) cat("Attempt", i, ":", "error detected.", myoutbreak$msg, "\n")
      else cat("Attempt", i, ":", "Outbreak doesn't exceed minimum cases.", "\n")
      cat("Repeating simulation.", "\n")
    } else cat("Attempt", i, ":", "Generate outbreak with ncases =", n, "\n")
    cat("\n")
    i <- i+1
  }
  
  ##GET THE OUTPUT: EPIDATA, WIWDATA, DNA
  epidata <- data.frame(inf.ID = paste("ID", myoutbreak$id, sep = "_"),
                        inf.times = myoutbreak$onset,
                        rec.times = myoutbreak$recover,
                        inf.source = paste("ID", ifelse(is.na(myoutbreak$ances), 0, myoutbreak$ances), sep = "_"),
                        nmut = myoutbreak$nmut)
  
  wiwdata <- epidata %>%
    select(inf.source) %>%
    left_join((epidata %>% select(inf.ID, inf.times)), by = c("inf.source" = "inf.ID")) %>%
    rename(source.times = inf.times) %>%
    bind_cols((epidata %>% select(-c("inf.source")))) %>%
    mutate(si = inf.times-source.times) %>%
    select(inf.source, inf.ID, si)
  epidata <- epidata %>%
    left_join(wiwdata, by = c("inf.ID", "inf.source"))
  
  aligndata <- myoutbreak$dna
  row.names(aligndata) <- epidata$inf.ID
  aligndata <- as.list(aligndata)
  
  ##PLOT THE OUTBREAK
  if(.plot){
    gg <- as.igraph.simOutbreak(myoutbreak)
    par(mfrow = c(1,2), mar=c(0,0,0,0))
    plot(gg, layout=layout.reingold.tilford(gg), edge.arrow.size = .1,
         edge.arrow.width = .5, vertex.label = NA, edge.label.cex = .5,
         vertex.size = 2, edge.label = E(gg)$dist)
    plot(gg, layout=layout_with_fr(gg), edge.arrow.size = .1,
         edge.arrow.width = .5, vertex.label.cex = .3, edge.label.cex = .5,
         vertex.size = 3, vertex.label = NA, edge.label = wiwdata$si[!is.na(wiwdata$si)])
    par(mfrow = c(1,1))
  }
  
  ##GET THE OUTPUT
  output <- list(epidata = epidata, aligndata = aligndata, dynam = myoutbreak$dynam)
  class(output) <- "outbreak"
  return(output)
}