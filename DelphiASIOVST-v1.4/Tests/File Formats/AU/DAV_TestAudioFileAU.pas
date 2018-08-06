unit DAV_TestAudioFileAU;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, DAV_ChunkClasses, Classes, DAV_Common, Contnrs,
  SysUtils, DAV_ChannelDataCoder, DAV_AudioFileAU, DAV_AudioFile;

type
  // Test methods for class TAudioFileAU
  TestAudioFileAU = class(TTestCase)
  strict private
    FAudioFileAU: TAudioFileAU;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestScanning;
    procedure TestBasicWriting;
  end;

implementation

uses
  Dialogs;

procedure TestAudioFileAU.SetUp;
begin
  FAudioFileAU := TAudioFileAU.Create;
end;

procedure TestAudioFileAU.TearDown;
begin
 FreeAndNil(FAudioFileAU);
end;

procedure TestAudioFileAU.TestScanning;
var
  SR      : TSearchRec;
  succeed : Boolean;
begin
 if FindFirst('*.au', faAnyFile, SR) = 0 then
  try
   repeat
    succeed := True;
    try
     FAudioFileAU.LoadFromFile(SR.Name)
    except
     on e: EAUError do MessageDlg(e.Message, mtError, [mbOK], 0);
     else succeed := False;
    end;
    Check(succeed, 'Error loading file: ' + SR.Name);
   until FindNext(SR) <> 0;
  finally
   // Must free up resources used by these successful finds
   FindClose(SR);
  end;
end;

procedure TestAudioFileAU.TestBasicWriting;
var
  TempStream : TMemoryStream;
begin
 TempStream := TMemoryStream.Create;
 with TempStream do
  try
//   FAudioFileAU.Name := 'Test';
   FAudioFileAU.SampleFrames := 100;
   FAudioFileAU.SampleRate := 44100;
   FAudioFileAU.ChannelCount := 1;
   FAudioFileAU.SaveToStream(TempStream);
   TempStream.Position := 0;
   FAudioFileAU.LoadFromStream(TempStream);
  finally
   Free;
  end;
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestAudioFileAU.Suite);
  
end.

