Delphi ASIO & VST Project - ReadMe.txt
--------------------------------------

The Delphi ASIO & VST Project is an open source solution for Delphi & Lazarus 
to make ASIO and VST plugin development easy. With its VST plugin wizard and
several test tools it is easy not only to create a VST plugin, but also to test
it in several ways. The ASIO interface and an additional VST host also allows 
to build full featured stand alone applications such as the MiniHost. Also it 
comes with many useful examples showing the wide range of possibilities.

The Delphi ASIO & VST Project is based on a translation of the ASIO & VST 
headers by Frederic Vanmol (http://www.axiworld.be) and a VST plugin template 
by Tobias Fleischer (aka TobyBear, http://www.tobybear.de). The host code (for 
ASIO and VST hosting) and a VST plugin wizard in addition with a new template 
was written by Christian-W. Budde, the main developer of this project. He also 
added several useful DSP units and examples to the project demonstrating the 
wide range of possibilities. Furthermore GUI components and SynthEdit modules 
were added later.

Furthermore several other developers (such as Maik Menz in particular) helped 
over the years to improve this project. An incomplete list of contributors can 
be found at the end of this text file.  


Installation:
-------------

In order to make use of this project you need to install the packages. You 
should find dedicated packages for all major Delphi versions in the .\Packages\ 
sub-directory.

You need to install them in the following order:
DAV_Common_D?.dpk
DAV_DSP_D?.dpk
DAV_ASIOHost_D?.dpk (*)
DAV_VSTPlugin_D?.dpk (*)
DAV_VSTHost_D?.dpk (*)
DAV_SEHost_D?.dpk (*)
DAV_GUI_D?.dpk (*)
DAV_Modular_D?.dpk (**)

(*) not necessary for all applications
(**) yet unfinished and thus not mandatory

To install these packages, just doubleclick the .dpk file (or open it manually
in Delphi), then click on "compile", then on "install". After installation, 
a message should pop up notifying you that a new component has been added to 
the palette.

To verify all the packages have been installed correctly, you may want to open 
one of the countless examples. However, as some of these examples rely on other 
examples and resource files created by other projects it is recomended to use 
the following examples:

.\Examples\Standalone\ASIO-Host Demo\ASIODemo.dpr (for ASIO hosting)
.\Examples\Standalone\VST Plugin Scanner\VSTPluginScanner.dpr (for VST hosting)
.\Examples\Plugins\Simple Sample Delay\SimpleSampleDelay.dpr (for a VST plugin)
.\Examples\SE Modules\Butterworth\SEButterworth.dpr (for an SE module)


Exploring the project
---------------------

Once you got everything working without any problem you can start explore all 
the other examples. Keep in mind, that it might become necessary to compile 
some resources prior to compiling the project. In case this is necessary you 
can find a batch file in the directory to simplify this cumbersome work.

Also it is sometimes necessary to render some of the knobs. They were done in 
KnobMan (see http://www.g200kg.com/en/software/knobman.html). It's worth to 
learn how this software works as it will save you plenty of time in the design
stage.


License
-------

The license of this project is a dual license of either LGPL or MPL. In order
to work with Lazarus the LGPL that holds here is the same as the (modified) 
LGPL used for Lazarus.


Comments
--------

Comments, suggestions and extensions are *very* welcome! Feel free to 
contribute this project or suggest changes. It also helps just reporting bugs 
and other oddities. 


Contributors (incomplete list, let me know if your name is missing)
------------

Christian-W. Budde (Christian@savioursofsoul.de)
Tobias Fleischer (tobybear@web.de)
Chris Horton
Frederic Vanmol
Maik Menz (myco@gmx.net)
Tobias Erichsen
Norbert Stellberg
Marco Spies
Benjamin Rosseaux (http://bero.0ok.de)
Salih Sertkaya
Daniel Terhell


Special Thanks
--------------

This project contains a lot of work & ideas of others who kindly shared their 
knowledge (in form of source code or articles).

First of all many thanks to 'Laurent de Soras' here as the pascal 
translation of some of his libraries were taken as base for some units. 
Make sure to visit his homepage (http://ldesoras.free.fr/) for all of his work.

Also many thanks to Benjamin Rosseaux, who wrote the ASIO interface to access 
ASIO without the need of a helper DLL. Furthermore it was his idea to 
statically link DLLs.  

Furthermore many thanks to Olli Parviainen for making a dynamic link library 
available to link his SoundTouch library.   


Copyrights:
-----------

VST & ASIO are trademarks of:
Steinberg Media GmbH (http://www.steinberg.net)

SoundTouch written by Olli Parviainen

SynthEdit by Jef McClintock (http://www.synthedit.com)

WinAmp written by Nullsoft (http://www.nullsoft.com), copyright AOL
