program VSTPluginUnitTest;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  GUITestRunner,
  DAV_TestVSTHost in 'DAV_TestVSTHost.pas',
  SplashScreen in 'SplashScreen.pas' {FmSplashScreen};

{$R *.res}

begin
  Application.Title := 'VST Plugin Unit Test';
  Application.Initialize;
  InitializeVstPluginTests;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

