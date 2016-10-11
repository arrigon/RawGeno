err.mes <-
function(){
    choose.pop=function(...){
      tkdestroy(tt)
      pop2.vect=tkgetOpenFile(title='Select samples additionnal information file')
      assign("pop2.vect",pop2.vect,env=.GlobalEnv)
      matinfo=read.delim(tclvalue(pop2.vect),header=T)
      assign("matinfo",matinfo,env=.GlobalEnv)
      START.PLATES()
      }
    quit.apply=function(...){
      tkdestroy(tt)
      }      
  tt=tktoplevel()
  tktitle(tt)="Option unavailable"
  frame1=tkframe(tt, relief = "groove", borderwidth = 2)
  labelInfotitle=tklabel(frame1,text="
  Please provide additional informations about samples.
(refer to the documentation of RawGeno for examples)
               
The table must include at least the three following columns:
- Tag = the name of samples
- Plate = the plate number
- Pos = the position of samples within the plate 
        (96 wells PCR plates)",font = "Arial 9",justify='left')
  tkgrid(labelInfotitle, sticky = "w")
  tkpack(frame1, fill = "x",side='top')
  
  frame2=tkframe(tt, relief = "groove", borderwidth = 2)
  browse.pop=tkbutton(frame2, text = "Browse and Use", command = choose.pop,padx = 20, font = "arial 10")
  quit.but=tkbutton(frame2, text = "Quit", command = quit.apply,padx = 20, font = "arial 10")
  tkgrid(browse.pop,quit.but,sticky = "w")
  tkpack(frame2, fill = "x",side='bottom')
  }
