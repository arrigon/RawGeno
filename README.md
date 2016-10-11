# RawGeno V.2.0-1 Quick Readme


Installing RawGeno:

Windows users (tested on XP, expected to work on >Vista but contact me for troubleshoot): install RawGeno from the binary file
0. Download RawGeno_2.0-1.zip

1. Start R from where you have downloaded RawGeno

2. make sure that the working directory of R is where you have downloaded RawGeno

setwd("MyPathToRawGenoInstallFile")

3. Install the package using the following command:

utils:::menuInstallLocal() # and select the RawGeno_2.0-1.zip file. R should then take care of the rest.

4. Install vegan and tkrplot
install.packages("vegan") # optional, but will let you explore your results with the PCoA display.
install.packages("tkrplot") # optional, but RawGeno will kindly recall you that you should install it to benefit from all the available options ;)

5. load the package and have fun!
require(RawGeno)
require(vegan)
RawGeno()



Linux users: install RawGeno from the source file (this version runs with R2.14):
0. Download RawGeno_2.0-1.tar.gz

1. Start R from where you have downloaded RawGeno

2. make sure that the working directory of R is where you have downloaded RawGeno
setwd("MyPathToRawGenoInstallFile")

3. Install the package using the following command:
install.packages("RawGeno_2.0-1.tar.gz", repos = NULL, type = "source")
install.packages("vegan") # optional, but will let you explore your results with the PCoA display.
install.packages("tkrplot") # optional, but RawGeno will kindly recall you that you should install it to benefit from all the available options ;)

4. load the package and have fun!
require(RawGeno)
require(vegan)
RawGeno()


The tkrplot fun part for Linux users...
Linux users; if you have troubles for installing tkrplot,
note that precompiled versions are available in the usual Ubuntu directories.

If you still experience troubles, make sure that you have the following libraries already installed:
libx11-dev
tcl8.5-dev
tk8.5-dev

and retry to install it. This solved my issues actually.



Mac users: please contact me. The linux version seems to work for you, but you may get stuck into problems with TclTk.
I am unfortunately not familiar with the Apple environment and would be really happy to get any feedbacks about the way you got it at work.

You can find informations about how to solve this in the R CRAN FAQs:
http://cran.r-project.org/bin/macosx/RMacOSX-FAQ.html#TclTk-issues

Alternatively, you could use the Windows version with an emulator, such as Parallels Desktop# RawGeno
