# DelphiASIOVST-64-bit-Examples-2018
Some 64 bit VSTs (Crumar DS2, Modular, Syntorchestra, Mellotron, Waveplayer) using DelphiASIOVST and Delphi Community Edition.
This is part of my hobby project. See www.ermers.org.

Here you find a few examples of 64 bit VSTs written
- with ASIOVST package
- Delphi Community Edition.

I am very happy Embarcadero has released a community edition with which it is possible to write 64 bit applications.
I have been using the ASIOVST package for a long, long time and I am sorry to see that it isn't used often anymore.
It is a shame because this package is miraculous! And yes, even in 2018 you can build 64 bit VSTs (2.4) with it!
I have made a few adjustments in the original package, some are bugfixing, some are workarounds.
Since I am using this for years I am not exactly sure why the changes are needed. I know there were a few bugs
in the wave Filehandling and reading Parameters. 

Moreover, this is part of my hobby projects where I also have build a dedicated MIDI routing system. 
Therefor there are a few overlapping things, shared in the RMCShared directory. 
Some plugins use Delay, Reverb from other (free) plugins. These are not part of this source and should be put
in your VST folder. If you get that far, I can be of help to get things going.
But in all fairness, I've made these sources available so you can create new ones yourself more quick (and you
can leave out the effects easily). 

A quick overview:
- The Crumar DS2 Plugin is a faithfull reconstruction of the DS2, with almost exactly the same possibilities.
- The Modular plugin is a 4 oscillator synth which I use in my setup for sequencing. Under the hood it is almost the
same synth as the DS2, except for another UI and two more oscillators. 
- The Rompler VST was just a quick study for a mellotron VST. You will need the samples to run it, but since these are 
free I can send them if you want them.
- The syntorchestra is an emulation for a Farfisa Syntorchestra. It can use samples OR waveforms. I don't have the 
correct samples for the mono section so I only did some effort to recreate the poly section. However, in my setup
I use another samplebased plugin. 
- The waveplayer is a very basic waveplayer. It can play .WAV and .MP3 and uses Bass (not included). I use it 
in my setup, and by sending the correct MIDI information you can command it to play any wavefile.

The VST are available in 64 bit VST format, but there are also standalone versions included.
It is very nice to see how, with some effort, the ASIOVST makes it possible to have a large simultaneous codebase 
to realize this. 

Note that these plugins have only be tested with the master of all DAWs, Reaper! 
They run smooth there, and you can even debug them using Delphi debugger. 
As with many open source projects, and due to the complexity of VST software, you will be puzzled at first,
but hopefully you can get it going. If not, let me help. I would really like to see Delphi and ASIOVST to 
become as popular as they were one day. They deserve it!
 

