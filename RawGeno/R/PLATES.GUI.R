PLATES.GUI <-
function(...){ 
### Selecting Individuals
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
      vard=tclvalue(rbValue)
      p=as.numeric(toexport)
      if(tclvalue(log.dum)==0) logv=F
      if(tclvalue(log.dum)==1) logv=T
      if(tclvalue(sam.dum)==0) txt='Stats'
      if(tclvalue(sam.dum)==1) txt='Tag'
      if(tclvalue(pdf.dum)==0) pathpdf=0
      if(tclvalue(pdf.dum)==1) pathpdf=tclvalue(dir.GUI)
      for(i in p){
        SHOWPLATES(vard,p=i,txt,pathpdf,logv)
        }
      }
      
    choose.apply=function(...){
      assign("dir2.GUI",tkchooseDirectory(),env=.GlobalEnv)
      tkdestroy(tt)
      PLATES.GUI(matinfo)
      }

    choose.pop=function(...){
      tkdestroy(tt)
      pop2.vect=tkgetOpenFile(title='Select samples additionnal information file')
      assign("pop2.vect",pop2.vect,env=.GlobalEnv)
      assign("matinfo",read.delim(tclvalue(pop2.vect),header=T),env=.GlobalEnv)
      states=as.numeric(levels(as.factor(matinfo$Plate)))
      assign('states',states,env=.GlobalEnv)
      PLATES.GUI(matinfo)
      }

    quit.apply=function(...){
      tkdestroy(tt)
      }

####### GUI
    tt=tktoplevel()
    tktitle(tt)="Visualize PCR Plates"

    ##### Importing Informations
    frame1 <- tkframe(tt, relief = "groove", borderwidth = 2)
    labelInfotitle=tklabel(frame1,text="Samples Information and Selection",font = "Arial 14",justify='left')
    frame1.main <- tklabel(frame1, text = "Choose Populations Informations",font = "arial 10", justify = "left")
    frame1.entry1 <- tkentry(frame1, textvariable = pop2.vect, width = 50, justify = "left")
    browse.pop <- tkbutton(frame1, text = "Browse and Use", command = choose.pop,padx = 20, font = "arial 10")
    tkgrid(labelInfotitle, sticky = "w")
    tkgrid(frame1.main, columnspan = 1, sticky = "w")
    tkgrid(frame1.entry1,browse.pop,sticky = "w")
    tkpack(frame1,fill = "x")

    ##### Selecting plates
    frameA=tkframe(tt, relief = "groove", borderwidth = 3)
      label1=tklabel(frameA,text="Select PCR Plates                  Your Selection")
      tkgrid(label1)
      tkpack(frameA,fill = "x")

    frameB=tkframe(tt, relief = "groove", borderwidth = 2)
      tl<-tklistbox(frameB,height=10,selectmode="multiple",yscrollcommand=function(...)tkset(scr,...),background="white")
      scr=tkscrollbar(frameB, repeatinterval=5,command=function(...)tkyview(tl,...))
      for (i in (1:length(states))){
        tkinsert(tl,"end",states[i])
        }
      tkselection.set(tl,0)

      tl2<-tklistbox(frameB,height=10,selectmode="single",yscrollcommand=function(...)tkset(scr2,...),background="grey")
      scr2=tkscrollbar(frameB, repeatinterval=5,command=function(...)tkyview(tl2,...))
      tkgrid(tl,scr,tl2,scr2,sticky='sw')
      tkgrid.configure(scr,rowspan=4,sticky="nsw")
      tkgrid.configure(scr2,rowspan=4,sticky="nsw")
      tkpack(frameB, fill = "x")

    frameC=tkframe(tt, relief = "groove", borderwidth = 2)
      OK.but=tkbutton(frameC,text="Cancel",command=quit.apply )
      DeleteSelection.but=tkbutton(frameC,padx = 30,text="<- Delete From selection",command=DeleteSelection)
      InsertSelection.but=tkbutton(frameC,padx = 30,text="Insert Plate->",command=InsertSelection)
      tkgrid(InsertSelection.but,DeleteSelection.but,sticky='we')
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
    
    rbValue=tclVar("NumBins")
    tkconfigure(rb1,variable=rbValue,value="NumBins")
    tkconfigure(rb2,variable=rbValue,value="NormFreq")
    tkconfigure(rb3,variable=rbValue,value="HeightVar")
    tkconfigure(rb4,variable=rbValue,value="HeightMean")
          
    tkgrid(tklabel(frame2,text="Choose display information",font = "Arial 14",justify='left'),sticky='w')
    tkgrid(tklabel(frame2,text="Number of Bins / individual "),rb1,sticky='w')
    tkgrid(tklabel(frame2,text="Regularity frequency index / individual  "),rb2,sticky='w')
    tkgrid(tklabel(frame2,text="Standard deviation peaks height  / individual "),rb3,sticky='w')
    tkgrid(tklabel(frame2,text="Mean peaks height / individual "),rb4,sticky='w')
    tkpack(frame2, fill = "x")    
    
    log.dum=tclVar(init="0")
    sam.dum=tclVar(init="0")
    pdf.dum=tclVar(init="0")
    button.log=tkcheckbutton(frame2, text="Log transform", 'onvalue'=1, 'offvalue'=0, 'variable'=log.dum,justify='left')
    button.sam=tkcheckbutton(frame2, text="Display Samples names", 'onvalue'=1, 'offvalue'=0, 'variable'=sam.dum,justify='left')
    button.pdf=tkcheckbutton(frame2, text="Save plots (pdf)", 'onvalue'=1, 'offvalue'=0, 'variable'=pdf.dum,justify='left')    
    tkgrid(button.sam,sticky = "w")
    tkgrid(button.log,button.pdf,sticky = "w")

    ## Export   
    if(is.numeric(dir2.GUI)==T){
      dir.GUI <- tclVar(getwd())
      } else {
      dir.GUI <- tclVar(dir2.GUI)
      }   
    frame3=tkframe(tt, relief = "groove", borderwidth = 2)
    frame3.main=tklabel(frame2, text = "Choose Export Directory",font = "arial 10", justify = "left")
    frame3.entry1=tkentry(frame2, textvariable = dir.GUI, width = 40, justify = "left")
    browse.but=tkbutton(frame2, text = "Browse", command = choose.apply,padx = 20, font = "arial 10")
    tkgrid(frame3.main, columnspan = 1, sticky = "w")
    tkgrid(frame3.entry1,browse.but,sticky = "w")
    tkpack(frame3, fill = "x",side='bottom')
        
    ## Buttons
    frame4=tkframe(tt, relief = "groove", borderwidth = 2)
    show.but=tkbutton(frame4, text = "Show PCR Plates !", command = options.apply,padx = 20, font = "arial 10")
    quit.but=tkbutton(frame4, text = "Quit", command = quit.apply,padx = 20, font = "arial 10")
    tkgrid(show.but,quit.but,sticky = "w") 
    tkpack(frame4, fill = "x",side='bottom')
    
    tkfocus(tt)
    }
