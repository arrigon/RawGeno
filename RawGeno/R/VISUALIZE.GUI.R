VISUALIZE.GUI <-
function(...){
  assign('stock',sort(as.character(colnames(matinfo))),env=.GlobalEnv)
  assign('states','No field selected',env=.GlobalEnv)
  assign("toexport", -9, env = .GlobalEnv)
  
##### Functions
### Selecting Individuals
    InsertField=function(...){
        assign("toexport", -9, env = .GlobalEnv)
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
      assign("diagno",tclvalue(rbValue),env=.GlobalEnv)
      if(matinfo!=0){
        if(length(as.integer(tkcurselection(tlsplit)))==0){
          assign('columnDiagno',0,pos=1)
          } else {
            assign('columnDiagno',stock[as.integer(tkcurselection(tlsplit))+1],pos=1)
            }
        } else {
        assign('columnDiagno',0,pos=1)
        }
      VISU(diagno,columnDiagno)
      }
      
    options2.apply=function(...){ 
      assign("diagno",tclvalue(rbValue),env=.GlobalEnv)
      if(matinfo!=0){
        if(length(as.integer(tkcurselection(tlsplit)))==0){
          assign('columnDiagno',0,pos=1)
          } else {
            assign('columnDiagno',stock[as.integer(tkcurselection(tlsplit))+1],pos=1)
            }
        } else {
        assign('columnDiagno',0,pos=1)
        }
      VISU.ORD(diagno,columnDiagno)
      }  

    choose.apply=function(...){
      assign("dir2.GUI",tkchooseDirectory(),env=.GlobalEnv)
      tkdestroy(tt)
      VISUALIZE.GUI(matinfo)
      }

    choose.pop=function(...){
      tkdestroy(tt)
      pop2.vect=tkgetOpenFile(title='Select samples additionnal information file')
      assign("pop2.vect",pop2.vect,env=.GlobalEnv)
      assign("matinfo",read.delim(tclvalue(pop2.vect),header=T),env=.GlobalEnv)
      VISUALIZE.GUI(matinfo)
      }

    quit.apply=function(...){
      tkdestroy(tt)
      }

####### GUI
    tt=tktoplevel()
    tktitle(tt)="Visualize Samples"

    ##### Importing Informations
    frame1 <- tkframe(tt, relief = "groove", borderwidth = 2)
    labelInfotitle=tklabel(frame1,text="Samples Information and Selection",font = "Arial 14",justify='left')
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

    ##### Visualization Options
    if(is.numeric(pop2.vect)==T){
      pop.vect=tclVar(getwd())
      } else {
      pop.vect=tclVar(pop2.vect)
      }
    
    frame2=tkframe(tt, relief = "groove", borderwidth = 2)
    rb1=tkradiobutton(frame2)
    rb2=tkradiobutton(frame2)
    rb3=tkradiobutton(frame2)
    rb4=tkradiobutton(frame2)
    rb5=tkradiobutton(frame2)
    
    rbValue=tclVar("NormFreq")
    tkconfigure(rb1,variable=rbValue,value="NumBins")
    tkconfigure(rb2,variable=rbValue,value="NormFreq")
    tkconfigure(rb3,variable=rbValue,value="HeightVar")
    tkconfigure(rb4,variable=rbValue,value="HeightMean")
    tkconfigure(rb5,variable=rbValue,value="External")
          
    tkgrid(tklabel(frame2,text="Choose display information",font = "Arial 14",justify='left'),sticky='w')
    tkgrid(tklabel(frame2,text="Number of peaks / individual "),rb1,sticky='w')
    tkgrid(tklabel(frame2,text="Regularity frequency index / individual  "),rb2,sticky='w')
    tkgrid(tklabel(frame2,text="Standard deviation peaks height  / individual "),rb3,sticky='w')
    tkgrid(tklabel(frame2,text="Mean peaks height / individual "),rb4,sticky='w')
    tkgrid(tklabel(frame2,text="External display value "),rb5,sticky='w')
    tkpack(frame2, fill = "x")
   
    tlsplit=tklistbox(frame2,height=4,selectmode="single",yscrollcommand=function(...)tkset(scrsplt,...),background="white")
    scrsplt=tkscrollbar(frame2, repeatinterval=2,command=function(...)tkyview(tlsplit,...))
    for (i in (1:length(stock))){
      tkinsert(tlsplit,"end",stock[i])
      }
    tkgrid(tlsplit,scrsplt,sticky='w')
    tkgrid.configure(scrsplt,rowspan=4,sticky="nsw")
    tkpack(frame2,side='left')
    
    
    ## Buttons
    show.but=tkbutton(frame2, text = "Show Binary Matrix !", command = options.apply,padx = 20, font = "arial 10")
    show2.but=tkbutton(frame2, text = "Show PCoA !", command = options2.apply,padx = 20, font = "arial 10")
    quit.but=tkbutton(frame2, text = "Quit", command = quit.apply,padx = 20, font = "arial 10")
    if(exists("vegdist")){
      tkgrid(show.but,show2.but,quit.but,sticky = "w")
      } else {
      tkgrid(show.but,quit.but,sticky = "w")
      }
    
    tkpack(frame2, fill = "x",side='bottom')
    tkfocus(tt)
    }
