////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Delphi ASIO & VST Package - Compiler Switches                             //
//  ---------------------------------------------                             //
//                                                                            //
//  With this include file you can finetune some settings. Not all of them    //
//  are documented in full length, but at least a hint is given here as well. //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{ Enable this option, in order to use the native types for arrays as defined
  in the types.pas unit. Advantages are native assigns to dynamic arrays.
  However, if you experience random IDE crashes this might be the reason. To
  solve those conflicts you can either disable this option or uninstall the
  packages, delete all *.dcu and *.dcp files and reinstall the packages.
  Since the types.pas is not available for older Delphi versions (Delphi 5),
  this option is disabled by default for those IDEs.
  The default is enabled. }
{$DEFINE UseNativeTypes}

{ Enable this option, in order to use the native float to string convertsion
  The default is enabled. }
{$DEFINE UseNativeFloatToStringConversion}

{ Disable this option if you want to use range/overflow checks of the compiler
  The default is enabled. }
{-$DEFINE ZeroArray}

{ Enable this option for platform independent testing. No assembler code is
  used if this is checked}
{.$DEFINE PUREPASCAL}

{ Misc. undocumented compiler switches}
{$DEFINE CheckCRC}
{-$DEFINE ValidateEveryReadOperation}
{$DEFINE UseAlignedMemory}


//////////////////////////////
//  ASIO Compiler Switches  //
//////////////////////////////

{ In the official ASIO specifications there is only one ASIO driver allowed
  per application. Based on this requirement the callback can only address one
  single driver (globally). Though it might work with multiple drivers. To test
  this you can disable the error by allowing multiple ASIO hosts.
  The default is disabled.}
{$DEFINE AllowMultipleAsioHosts}

{ After uninstalling some ASIO driver, empty strings or false settings might
  remain in the registry leading to non existing ASIO drivers. These non-
  functional drivers can be ignored or an exception can be raised to warn the
  user.
  The default is enabled.}
{$DEFINE IgnoreBrokenAsioList}

{ Since the interface to ASIO can't be handled natively by pascal/delphi a
  special wrapper for the calling convention is mandatory. This can either be
  the build-in wrapper by BeRo or via the OpenASIO.DLL by Martin Fay. If you
  prefer the second option you need to enable this switch (needs OpenASIO.dll).
  The default is disabled.}
{.$DEFINE OpenASIO}

{ In the very early days a dedicated ASIO mixer was included with this
  component. It has not been used since ages, but if you long for testing it,
  you can enable this switch and experiment with the code. Not recommended!
  The default is disabled.}
{.$DEFINE ASIOMixer}

{ In the very early days it was possible to call the control panel from the
  application (right click -> "control panel"). This was handy in case some
  settings needed to be tweaked from within the IDE. The current state is
  unknown, but it might work (only Delphi 5 though).}
{.$DEFINE D5CP}


////////////////////////////////////
//  VST Plugin Compiler Switches  //
////////////////////////////////////

{ By default, a VST plugin is wrapped around a descendant of a TDataModule. It
  is responsible to handle the properties easily. If you prefer the old style
  of assigning property values by code, you may want to disable this option.
  In the examples you should find some examples that needs to have this option
  disabled.
  The default is enabled.}
{$DEFINE UseDelphi}

{ This switch was introduces by "the-real-myco". As far as I remember it
  handles automatic conversion of VST process calls to dynamic arrays. It slows
  down processing, but the handling is easier.
  The default is enabled.}
{$DEFINE CONVERT_TO_DYNARRAY}

{ To identify VST Plugin datamodules a reference pointer ist stored into the
  basic TVSTEffect record. The reference an either be stored as 'AudioEffectPtr'
  or as 'User' pointer. While the original 'AudioEffectPtr' is reserved to
  store an AudioEffect class instance, the user pointer can be anything.
  By default the user pointer is used.}
{-$DEFINE UseAudioEffectPtr}

{ Enable this option to create a special "debug.log". While it is not useful
  to be enabled for productional use, it offers some debug possibilities for
  hosts that do not allow any debuggers. The "debug.log" contains information
  about the dispatcher and some internal states.
  The default is disabled.}
{.$DEFINE DebugLog}

  { By default every opcodes called by the host is logged separately. However,
    this can blow the log file by a factor of 2. If this switch is enabled,
    the opcodes will not be logged.
    The default is disabled.}
  {.$DEFINE DebugLogSkipOpcodeLogging}


//////////////////////////////////
//  VST Host Compiler Switches  //
//////////////////////////////////

{ Enable this option if the VST host unit should contain code to display any
  GUI for the plugin. If only audio processing is favoured, the application
  size can be shrinked by disabling this switch
  The default is enabled.}
{$DEFINE VstHostGUI}

{ Enable this option if the VST host unit should initialize the VST plugins
  similar to Cubase 4. Some opcodes are dispached before the plugin is open.
  The default is disable.}
{.$DEFINE VstHostCubase4}

{ Enable this option if the jBridge should be used to bridge 64bit plugins if
  available. jBridge is available at: http://jstuff.wordpress.com/jbridge
  The default is disable.}
{.$DEFINE jBridge}

{ Enable this option if the VST host should use FlatScrollBars instead of the
  3-D like scrollbars. It is disabled by default, because it hasn't been tested
  in detail.
  The default is disabled.}
{.$DEFINE FlatSrcollBar}

{ To distinguish VST plugins and hosts, in the VST Host dispatcher, pointers
  are stored in the effect record. Unfortunately these information are not
  available for some dispatcher calls (especially those before instantiation).
  If this switch is disabled (default) only generic informations are returned
  by the host (e.g. the VendorString (audioMasterGetVendorString) is hard coded
  to "Delphi ASIO & VST Project". This needs to be completely adjustable enable
  this compiler switch. A known issues with this switch is the fact that in a
  multithread environment with several TVSTHost components containing several
  TVstPlugin items, the search might fail and point to another object instead.
  It probably won't crash, but the returned information is wrong.
  The default is disabled.}
{.$DEFINE SearchPluginAndHost}

{ The compiler option below enables the automatic exception handling. This
  will catch any exceptions caused by the VST plugin and discards them.
  The exception handling is up to the host application then. If it isn't
  present there, all exceptions will be raised. Thus the application might
  appear less stable to the user, hence it is most probably caused by the
  hosted VST plugin}
{.$DEFINE VstHostExceptionHandling}


/////////////////////////////
//  DSP Compiler Switches  //
/////////////////////////////

{$DEFINE HandleDenormals}
{ If this compiler switch is enabled, denormals are handled on the lowest level
  possible. Although it costs some extra CPU cycles per sample, it avoids the
  penalty caused by denormal calculations (around factor 3)
  The default is enabled.}

{-$DEFINE Use_IPPS}
{ If this compiler switch is enabled the Intel Performance Primitives are used.
  Right now there only exists a unit (not part of the package) to integrate the
  FFT found in the IPP. You need to purchase the IPP separately from Intel in
  order to use this.
  The default is disabled.}

{-$DEFINE Use_CUDA}
{ If this compiler switch is enabled CUDA can be used.
  Right now there exists only a wrapper to use the 32-bit precision FFT from
  the CUDA library, but further options might be implemented in the future as
  well. The unit for interfacing can be found in the repository, but it is
  mandatory to install CUDA first. Also you need to have a nVidia graphic
  adapter in order to use this. Otherwise only an emulation will be used. This
  emulation is very slow!
  The default is disabled.}

{$DEFINE UseFastMove}
{ If this compiler switch is enabled, the FastMove library is used for all
  Move(...) calls. It used to be significantly faster, especially on older
  systems and IDEs. In later versions of Delphi its code is built in or at
  least on par in terms of performance.
  The FastMove library can be found at: http://fastcode.sourceforge.net/
  The default is enabled.}

{$DEFINE UseMadExcept}
{ If this compiler switch is enabled, the madExcept library is used to handle
  exceptions. The library can be downloaded for free at http://www.madshi.net/
  However, a license need to be purchased in case it is used in a commercial
  product.
  The default is enabled.}

