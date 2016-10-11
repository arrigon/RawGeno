SAVE.GUI <-
function(...){
  if(exists('AFLP')){
    all.dat=AFLP$all.dat
    } else {
    all.dat=0
    }
  if(exists('data.binary')){  
    peaks.info=data.binary$peaks.info
    assign("data.out",data.binary$data.binary,env=.GlobalEnv)
    } else {
    peaks.info=0
    data.binary=0
    }
           
  assign('stock',sort(as.character(colnames(matinfo))),env=.GlobalEnv)
  assign('states','No field selected',env=.GlobalEnv)
  assign('toexport',-9,env=.GlobalEnv)

##### Functions
### Selecting Individuals
    InsertField=function(...){
        assign("toexport",-9,env=.GlobalEnv)
        for(i in 1:nrow(matinfo)){
          tkdelete(tl,0)
          }
        toFld=as.integer(tkcurselection(tcol))+1
        st=sort(levels(as.factor(matinfo[,colnames(matinfo)==stock[toFld]])))
        assign("statexport",stock[toFld],env=.GlobalEnv)
        assign("states",st,env=.GlobalEnv)
        for (i in (1:length(st))){
          tkinsert(tl,"end",st[i])
          } 
        }

    InsertSelection=function(stexp,...){
        for(i in 1:nrow(matinfo)){
          tkdelete(tl2,0)
          }
        toInsrt=as.integer(tkcurselection(tl))+1
        xprt=states[toInsrt]
        xprt=c(toexport,xprt)
        xprt=levels(as.factor(xprt))
        xprt=xprt[xprt!=-9]
        for(i in 1:length(xprt)){
          tkinsert(tl2,"end",xprt[i])
          }  
        assign("toexport",xprt,env=.GlobalEnv)
        }

    DeleteSelection=function(...){
        toDel=as.integer(tkcurselection(tl2))
        tkdelete(tl2,toDel)
        DELET(toDel)
        }
           
### Exporting functions
### Selecting Individuals   
    options.apply=function(...){
      if(exists("mergedTable")){
      origin=tclvalue(rbValue) 
      if(origin=='databinary') data.origin=t(data.binary$data.binary)
      if(origin=='mergedTable') data.origin=mergedTable
      } else {
      data.origin=t(data.binary$data.binary)
      }
      
      if(is.data.frame(matinfo)==F){
        cat('No Population Informations added')
        data.out=data.origin
        } else {  
        ### Join binary table and Info table
        data.bin=data.origin
        popsA=rownames(data.bin)
        popsB=matinfo$Tag
        inters.A=data.bin[match(intersect(popsA,popsB),popsA),]
        inters.B=matinfo[match(intersect(popsA,popsB),popsB),]
        x=cbind(inters.B,inters.A)
        rownames(x)=x$Tag

        ### Select individuals to display    
        if(toexport==-9){
          data.show=x
          cat('Whole dataset selected')
          } else {   
          stexp=as.vector(x[,match(statexport,colnames(x))])
          data.show=x[which(stexp==toexport[1]),]
          if(length(toexport)>1){     
            for(i in 2:length(toexport)){
              temp=x[which(stexp==toexport[i]),]
              data.show=rbind(data.show,temp)
              }
            }
          }
          
        data.out=data.show[,(ncol(inters.B)+1):ncol(data.show)]
        data.out=data.out[,colSums(data.out)>1]
        }  
      data.out=t(data.out)
      
      ## Define Spliting for pops
      toSplt=as.integer(tkcurselection(tlsplit))+1
      if(sum(toSplt)==0){
        pops=as.vector(data.frame(pops=rep(1,ncol(data.out))))
        cat('!! No Population Informations Added !! \n Take care to select the defining population field!')
        } else {
        pops=data.frame(pops=data.show[,colnames(data.show)==stock[toSplt]])[,1]
        cat(paste("Field ",stock[toSplt]," selected for defining populations.",sep=''))      
        }  
      
      path=tclvalue(dir.GUI)
      if(tclvalue(tree.dum)==1) Treecon(t(data.out),path)
      if(tclvalue(tree.dum2)==1) Treecon.inv(t(data.out),path)
      if(tclvalue(strD.dum)==1) Structure.D(t(data.out),path)
      if(tclvalue(strDh.dum)==1) Structure.Dhapl(t(data.out),path)
      if(tclvalue(strma.dum)==1) Structurama(t(data.out),path)
      if(tclvalue(baps.dum)==1) Baps(t(data.out),path)
      if(tclvalue(nexP.dum)==1) Nexus.Paup(t(data.out),path)
      if(tclvalue(nexMB.dum)==1) Nexus.MrBayes(t(data.out),path)
      if(tclvalue(spagind.dum)==1) Spagedi.Ind(t(data.out),path)

      if(tclvalue(hic.dum)==1) Hickory(t(data.out),pops,path)
      if(tclvalue(arl.dum)==1) Arlequin(t(data.out),pops,path)
      if(tclvalue(popgen.dum)==1) Popgene(t(data.out),pops,path)
      if(tclvalue(aflpsurv.dum)==1) AFLPsurv(t(data.out),pops,path)
      if(tclvalue(strpop.dum)==1) Structure.popsD(t(data.out),pops,path)
      if(tclvalue(mltr.dum)==1) Mltr(t(data.out),pops,path)
      if(tclvalue(spagpop.dum)==1) Spagedi.Pop(t(data.out),pops,path)
      if(tclvalue(dfdist.dum)==1) Dfdist(t(data.out),pops,path)
      
      if(tclvalue(tech.dum)==1){
      write.table(all.dat,
      file=paste(tclvalue(dir.GUI),.Platform$file.sep,'NativeDatasetColumn.txt',sep='')
      ,sep='\t')

      write.table(peaks.info,
      file=paste(tclvalue(dir.GUI),.Platform$file.sep,'PeaksInfos.txt',sep='')
      ,sep='\t')
      
      write.table(data.binary$table.tech,
      file=paste(tclvalue(dir.GUI),.Platform$file.sep,paste('ScorParam',Sys.Date(),".txt",sep=''),sep=''),sep='\t')
            
      save(AFLP,data.binary
          ,file=paste(tclvalue(dir.GUI),.Platform$file.sep,paste('TechnicalFiles_',Sys.Date(),'.Rdata',sep=''),sep=''))
      }

      if(tclvalue(join.dum)==1){
        popsA=matinfo$Tag
        popsB=colnames(data.out)
        inters.A=matinfo[match(intersect(popsA,popsB),popsA),] 
        inters.B=data.out[,match(intersect(popsA,popsB),popsB)] 
        data.out=t(data.frame(inters.A,'Tag.fsa'=colnames(inters.B),t(inters.B)))
        }

      if(tclvalue(bin.dum)==1){
        write.table(data.out,
        file=paste(tclvalue(dir.GUI),.Platform$file.sep,'DataBinary.txt',sep='')
        ,sep='\t')
        }
        
      if(tclvalue(bint.dum)==1){
        write.table(t(data.out),
        file=paste(tclvalue(dir.GUI),.Platform$file.sep,'DataBinaryT.txt',sep='')
        ,sep='\t')
        }   
          
      tkdestroy(tt)
      cat(paste('Files Successfully Produced in ',tclvalue(dir.GUI),'\n',sep=''))
      }

    choose.apply=function(...){
      assign("dir2.GUI",tkchooseDirectory(),env=.GlobalEnv)
      tkdestroy(tt)
      SAVE.GUI(matinfo)
      }

    choose.pop=function(...){
      tkdestroy(tt)
      pop2.vect=tkgetOpenFile(title='Select samples additionnal information file')
      assign("pop2.vect",pop2.vect,env=.GlobalEnv)
      assign("matinfo",read.delim(tclvalue(pop2.vect),header=T),env=.GlobalEnv)
      SAVE.GUI(matinfo)
      }

    quit.apply=function(...){
      tkdestroy(tt)
      }

####### GUI
    tt=tktoplevel()
    tktitle(tt)="Dataset Manager"

    ##### Importing Informations
    frame1 <- tkframe(tt, relief = "groove", borderwidth = 2)
    labelInfotitle=tklabel(frame1,text="Samples Information and Selection",font = "Arial 14",justify='left')
    
    if(exists("mergedTable")){
      rb1=tkradiobutton(frame1)
      rb2=tkradiobutton(frame1)
      rbValue=tclVar("mergedTable")
      tkconfigure(rb1,variable=rbValue,value="databinary")
      tkconfigure(rb2,variable=rbValue,value="mergedTable")
      tkgrid(tklabel(frame1,text="Choose data origin",font = "Arial 14",justify='left'),sticky='w')
      tkgrid(tklabel(frame1,text="Scored Dataset "),rb1,sticky='w')
      tkgrid(tklabel(frame1,text="Imported / Merged table "),rb2,sticky='w')
      tkpack(frame1, fill = "x")
      }
      
    frame1.main <- tklabel(frame1, text = "Choose Populations Informations",font = "arial 10", justify = "left")
    frame1.entry1 <- tkentry(frame1, textvariable = pop2.vect, width = 60, justify = "left")
    browse.pop <- tkbutton(frame1, text = "Browse and Use", command = choose.pop,padx = 20, font = "arial 10")
    tkgrid(labelInfotitle, sticky = "w")
    tkgrid(frame1.main, columnspan = 1, sticky = "w")
    tkgrid(frame1.entry1,browse.pop,sticky = "w")
    tkpack(frame1,fill = "x")

    ##### Selecting individuals
    frameA=tkframe(tt, relief = "groove", borderwidth = 3)
      label1=tklabel(frameA,text="Select Variable                            Select States                                          Your Selection")
      tkgrid(label1)
      tkpack(frameA,fill = "x")

    frameB=tkframe(tt, relief = "groove", borderwidth = 2)
      tcol=tklistbox(frameB,height=10,selectmode="single",yscrollcommand=function(...)tkset(scrol,...),background="white")
      scrol=tkscrollbar(frameB, repeatinterval=5,command=function(...)tkyview(tcol,...))
      for (i in (1:length(stock))){
        tkinsert(tcol,"end",stock[i])
        }

      tl<-tklistbox(frameB,height=10,selectmode="multiple",yscrollcommand=function(...)tkset(scr,...),background="white")
      scr=tkscrollbar(frameB, repeatinterval=5,command=function(...)tkyview(tl,...))
      for (i in (1:length(states))){
        tkinsert(tl,"end",states[i])
        }
      tkselection.set(tl,0)

      tl2<-tklistbox(frameB,height=10,selectmode="single",yscrollcommand=function(...)tkset(scr2,...),background="grey")
      scr2=tkscrollbar(frameB, repeatinterval=5,command=function(...)tkyview(tl2,...))
      tkgrid(tcol,scrol,tl,scr,tl2,scr2,sticky='sw')
      tkgrid.configure(scrol,rowspan=4,sticky="nsw")
      tkgrid.configure(scr,rowspan=4,sticky="nsw")
      tkgrid.configure(scr2,rowspan=4,sticky="nsw")
      tkpack(frameB, fill = "x")

    frameC=tkframe(tt, relief = "groove", borderwidth = 2)
      OK.but=tkbutton(frameC,text="Cancel",command=quit.apply )
      InsertField.but=tkbutton(frameC,padx = 30,text="Select Field ->",command=InsertField)
      DeleteSelection.but=tkbutton(frameC,padx = 30,text="<- Delete From selection",command=DeleteSelection)
      InsertSelection.but=tkbutton(frameC,padx = 30,text="Insert State->",command=InsertSelection)
      tkgrid(InsertField.but,InsertSelection.but,DeleteSelection.but,sticky='we')
      tkpack(frameC, fill = "x")

##### Exporting Options
    if(is.numeric(dir2.GUI)==T){
      dir.GUI <- tclVar(getwd())
      } else {
      dir.GUI <- tclVar(dir2.GUI)
      }

    if(is.numeric(pop2.vect)==T){
      pop.vect=tclVar(getwd())
      } else {
      pop.vect=tclVar(pop2.vect)
      }

    bin.dum=tclVar(init="2")
    bint.dum=tclVar(init="2")
    tech.dum=tclVar(init="2")
    join.dum=tclVar(init="2")
    
    tree.dum=tclVar(init="2")
    tree.dum2=tclVar(init="2")
    strD.dum=tclVar(init="2")
    strDh.dum=tclVar(init="2")
    baps.dum=tclVar(init="2")
    nexP.dum=tclVar(init="2")
    nexMB.dum=tclVar(init="2")
    strma.dum=tclVar(init="2")
    spagind.dum=tclVar(init="2")

    arl.dum=tclVar(init="2")
    hic.dum=tclVar(init="2")
    popgen.dum=tclVar(init="2")
    aflpsurv.dum=tclVar(init="2")
    strpop.dum=tclVar(init="2")
    mltr.dum=tclVar(init="2")
    spagpop.dum=tclVar(init="2")
    dfdist.dum=tclVar(init="2")
    
    frame2 <- tkframe(tt, relief = "groove", borderwidth = 2)
    frame2.main <- tklabel(frame2, text = "Individual-oriented analyses", font = "Arial 14",justify='left')
    button.Join <- tkcheckbutton(frame2, text="Join Population Informations  \n with Binary Table", 'onvalue'=1, 'offvalue'=0, 'variable'=join.dum,justify='left')
    button.Bin <- tkcheckbutton(frame2, text="Binary Table \n (individuals = columns)", 'onvalue'=1, 'offvalue'=0, 'variable'=bin.dum,justify='left')
    button.Bint <- tkcheckbutton(frame2, text="Transposed Binary Table \n (individuals = rows)", 'onvalue'=1, 'offvalue'=0, 'variable'=bint.dum,justify='left')
    button.Tech <- tkcheckbutton(frame2, text="Technical Files", 'onvalue'=1, 'offvalue'=0, 'variable'=tech.dum)
    button.Tree <- tkcheckbutton(frame2, text="Treecon", 'onvalue'=1, 'offvalue'=0, 'variable'=tree.dum)
    button.TreeInv <- tkcheckbutton(frame2, text="Treecon inv", 'onvalue'=1, 'offvalue'=0, 'variable'=tree.dum2)
    button.StructD <- tkcheckbutton(frame2, text="Structure_2.2", 'onvalue'=1, 'offvalue'=0, 'variable'=strD.dum)
    button.StructDhapl <- tkcheckbutton(frame2, text="Structure_2.2_hapl", 'onvalue'=1, 'offvalue'=0, 'variable'=strDh.dum)
    button.Strma <- tkcheckbutton(frame2, text="Structurama", 'onvalue'=1, 'offvalue'=0, 'variable'=strma.dum)
    button.Baps <- tkcheckbutton(frame2, text="Baps", 'onvalue'=1, 'offvalue'=0, 'variable'=baps.dum)
    button.NexusP <- tkcheckbutton(frame2, text="Nexus (PAUP)", 'onvalue'=1, 'offvalue'=0, 'variable'=nexP.dum)
    button.NexusMB <- tkcheckbutton(frame2, text="Nexus (MrBayes)", 'onvalue'=1, 'offvalue'=0, 'variable'=nexMB.dum)
    button.Spagind <- tkcheckbutton(frame2, text="Spagedi.ind", 'onvalue'=1, 'offvalue'=0, 'variable'=spagind.dum)
    tkgrid(frame2.main, columnspan = 1, sticky = "w")
    tkgrid(button.Bin,button.Bint,button.Join, sticky = "w")
    tkgrid(button.Tree,button.TreeInv,button.Spagind,sticky = "w")
    tkgrid(button.Baps,button.StructD,button.StructDhapl,sticky = "w")
    tkgrid(button.Strma,button.NexusP,button.NexusMB,sticky = "w")
    tkgrid(button.Tech,sticky = "w")
    tkpack(frame2,fill='x',side='right')
    
    framePoptitle=tkframe(tt, relief = "groove", borderwidth = 2)
    labelPoptitle=tklabel(framePoptitle,text="Population-oriented analyses",font = "Arial 14",justify='left')
    tkgrid(labelPoptitle,sticky='we')
      
    frame2a.main=tklabel(framePoptitle, text = "Define populations according to variable:",justify='left')
    tlsplit=tklistbox(framePoptitle,height=4,selectmode="single",yscrollcommand=function(...)tkset(scrsplt,...),background="white")
    scrsplt=tkscrollbar(framePoptitle, repeatinterval=2,command=function(...)tkyview(tlsplit,...))
    for (i in (1:length(stock))){
      tkinsert(tlsplit,"end",stock[i])
      }
    tkgrid(frame2a.main, columnspan = 2, sticky = "nw")
    tkgrid(tlsplit,scrsplt,sticky='w')
    tkgrid.configure(scrsplt,rowspan=4,sticky="nsw")
    tkpack(framePoptitle,side='left')
    
    frame2b=tkframe(tt, relief = "groove", borderwidth = 2)     
    button.Arl=tkcheckbutton(frame2b, text="Arlequin", 'onvalue'=1, 'offvalue'=0, 'variable'=arl.dum)
    button.Hickory=tkcheckbutton(frame2b, text="Hickory", 'onvalue'=1, 'offvalue'=0, 'variable'=hic.dum)
    button.Popgen=tkcheckbutton(frame2b, text="Popgen", 'onvalue'=1, 'offvalue'=0, 'variable'=popgen.dum)
    button.Aflpsurv=tkcheckbutton(frame2b, text="AFLPsurv", 'onvalue'=1, 'offvalue'=0, 'variable'=aflpsurv.dum)
    button.StructPop=tkcheckbutton(frame2b, text="Structure2.2 Pops", 'onvalue'=1, 'offvalue'=0, 'variable'=strpop.dum)
    button.Mltr=tkcheckbutton(frame2b, text="Mltr", 'onvalue'=1, 'offvalue'=0, 'variable'=mltr.dum)
    button.Spagpop=tkcheckbutton(frame2b, text="Spagedi.Pops", 'onvalue'=1, 'offvalue'=0, 'variable'=spagpop.dum)
    button.Dfdist=tkcheckbutton(frame2b, text="Dfdist", 'onvalue'=1, 'offvalue'=0, 'variable'=dfdist.dum)
    tkgrid(button.Arl,button.Hickory, sticky = "w")
    tkgrid(button.Popgen,button.Aflpsurv, sticky = "w")
    tkgrid(button.StructPop,button.Mltr, sticky = "w")
    tkgrid(button.Spagpop,button.Dfdist, sticky = "w")
    tkpack(frame2b,side='left',fill='x')
    
    ## Buttons
    frame3.main <- tklabel(frame2, text = "Choose Export Directory",font = "arial 10", justify = "left")
    frame3.entry1 <- tkentry(frame2, textvariable = dir.GUI, width = 40, justify = "left")
    browse.but <- tkbutton(frame2, text = "Browse", command = choose.apply,padx = 20, font = "arial 10")
    tkgrid(frame3.main, columnspan = 1, sticky = "w")

    save.but <- tkbutton(frame2, text = "Save", command = options.apply,padx = 20, font = "arial 10")
    quit.but <- tkbutton(frame2, text = "Quit", command = quit.apply,padx = 20, font = "arial 10")
    tkgrid(frame3.entry1,browse.but,save.but,quit.but,sticky = "w")
    tkpack(frame2, fill = "x",side='bottom')
    }
