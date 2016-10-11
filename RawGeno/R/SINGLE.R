SINGLE <-
function(){
  listfiles=list.single
  assign("toimport",-9,env=.GlobalEnv)
  path=dirname(list.single)[1]
   
  go.apply=function(){
    assign("list.single",toimport,env=.GlobalEnv)
    tkdestroy(tt)
    diagno=tclvalue(rbValue)
    if(diagno=="indRows") assign("mergedTable", read.delim(list.single[1], row.names = 1,header = T), pos = 1)
    if(diagno=="indColumns") assign("mergedTable", t(read.delim(list.single[1], row.names = 1,header = T)), pos = 1)
    cat('Done:',basename(list.single[1]),'was successfully imported!')
    tkdestroy(tt)
    }

   quit.apply=function(){
    tkdestroy(tt)
    stop()
    }
    ### Selecting Individuals
      ImportAll=function(){
      for(i in 1:length(listfiles)){
            tkdelete(tl,0)
            }
          mprt=levels(as.factor(listfiles))
          mprt=mprt[mprt!=-9]
          for(i in 1:length(mprt)){
            tkinsert(tl,"end",basename(mprt[i]))
            }  
          assign("toimport",mprt,env=.GlobalEnv)
         }
         
      InsertSelection=function(...){
          for(i in 1:length(listfiles)){
            tkdelete(tl,0)
            }
          toInsrt=as.integer(tkcurselection(tcol))+1
          mprt=listfiles[toInsrt]
          mprt=levels(as.factor(c(toimport,mprt)))
          mprt=mprt[mprt!=-9]
          for(i in 1:length(mprt)){
            tkinsert(tl,"end",basename(mprt[i]))
            }  
          assign("toimport",mprt,env=.GlobalEnv)
          }
  
      DeleteSelection=function(...){
          toDel=as.integer(tkcurselection(tl))
          tkdelete(tl,toDel)
          DELETOP(toDel)
          }    
    
    ### GUI
    tt=tktoplevel()
    tktitle(tt)="Import a single Binary Matrix"
    
    ##### Selecting individuals
    frameA=tkframe(tt, relief = "groove", borderwidth = 3)
      label1=tklabel(frameA,text="Available Files                            Selected Files")
      tkgrid(label1)
      tkpack(frameA,fill = "x")

    frameB=tkframe(tt, relief = "groove", borderwidth = 2)
      tcol=tklistbox(frameB,height=20,selectmode="multiple",yscrollcommand=function(...)tkset(scrol,...),background="white")
      scrol=tkscrollbar(frameB, repeatinterval=5,command=function(...)tkyview(tcol,...))
      for (i in (1:length(listfiles))){
        tkinsert(tcol,"end",basename(listfiles)[i])
        }

      tl<-tklistbox(frameB,height=20,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
      scr=tkscrollbar(frameB, repeatinterval=5,command=function(...)tkyview(tl,...))
      tkselection.set(tl,0)

      tkgrid(tcol,scrol,tl,scr,sticky='sw')
      tkgrid.configure(scrol,rowspan=4,sticky="nsw")
      tkgrid.configure(scr,rowspan=4,sticky="nsw")
      tkpack(frameB, fill = "x")

    frameC=tkframe(tt, relief = "groove", borderwidth = 2)
      OK.but=tkbutton(frameC,text="Cancel",command=quit.apply )
      InsertAll.but=tkbutton(frameC,padx = 30,text="Import All files",command=ImportAll)
      DeleteSelection.but=tkbutton(frameC,padx = 30,text="<- Delete From selection",command=DeleteSelection)
      InsertSelection.but=tkbutton(frameC,padx = 30,text="Insert File->",command=InsertSelection)
      tkgrid(InsertAll.but,InsertSelection.but,DeleteSelection.but,sticky='we')
      tkpack(frameC, fill = "x")
    
    frameD=tkframe(tt, relief = "groove", borderwidth = 2)
    rb1=tkradiobutton(frameD)
    rb2=tkradiobutton(frameD)
    rb3=tkradiobutton(frameD)
    rb4=tkradiobutton(frameD)
    rb5=tkradiobutton(frameD)
    
    rbValue=tclVar("indRows")
    tkconfigure(rb1,variable=rbValue,value="indRows")
    tkconfigure(rb2,variable=rbValue,value="indColumns")
          
    tkgrid(tklabel(frameD,text="Matrix orientation",font = "Arial 12",justify='left'),sticky='w')
    tkgrid(tklabel(frameD,text="Individuals = rows"),rb1,sticky='w')
    tkgrid(tklabel(frameD,text="Individuals = columns"),rb2,sticky='w')
    tkpack(frameD, fill = "x")
     
    frameE=tkframe(tt, relief = "groove", borderwidth = 2)  
      save.but=tkbutton(frameE, text = "Continue", command = go.apply,padx = 20, font = "arial 10")
      quit.but=tkbutton(frameE, text = "Quit", command = quit.apply,padx = 20, font = "arial 10")
      tkgrid(save.but,quit.but,sticky = "w") 
      tkpack(frameE, fill = "x")
    }
