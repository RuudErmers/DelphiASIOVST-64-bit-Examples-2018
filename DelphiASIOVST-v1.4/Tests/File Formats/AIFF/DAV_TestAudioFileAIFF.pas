unit DAV_TestAudioFileAIFF;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, DAV_ChunkClasses, DAV_ChunkAiffBasic, Classes, DAV_Common, Contnrs,
  SysUtils, DAV_ChannelDataCoder, DAV_AudioFileAIFF, DAV_AudioFile;

type
  // Test methods for class TAudioFileAIFF
  TestAudioFileAIFF = class(TTestCase)
  strict private
    FAudioFileAIFF : TAudioFileAIFF;
    FAudioData     : TDAVArrayOfSingleFixedArray;
  protected
    procedure DecodeHandler(Sender: TObject; const Coder: TCustomChannelDataCoder; var Position: Cardinal);
    procedure DecodeSimpleHandler(Sender: TObject; const Coder: TCustomChannelDataCoder; var Position: Cardinal);
    procedure EncodeHandler(Sender: TObject; const Coder: TCustomChannelDataCoder; var Position: Cardinal);
    procedure EncodeSimpleHandler(Sender: TObject; const Coder: TCustomChannelDataCoder; var Position: Cardinal);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestScanning;
    procedure TestBasicWriting;
    procedure TestAdvancedWriting;
  end;

implementation

uses
  Dialogs;

resourcestring
  RCStrTestName = 'Test Name';
  RCStrTestAuthor = 'Test Author';
  RCStrTestCopyright = 'Test Copyright';

procedure TestAudioFileAIFF.SetUp;
begin
  FAudioFileAIFF := TAudioFileAIFF.Create;
end;

procedure TestAudioFileAIFF.TearDown;
begin
 FreeAndNil(FAudioFileAIFF);
end;

procedure TestAudioFileAIFF.TestScanning;
var
  SR      : TSearchRec;
  succeed : Boolean;
begin
 if FindFirst('*.aif*', faAnyFile, SR) = 0 then
  try
   repeat
    succeed := True;
    try
     FAudioFileAIFF.LoadFromFile(SR.Name)
    except
     on e: EAIFFError do MessageDlg(e.Message, mtError, [mbOK], 0);
     else succeed := False;
    end;
    Check(succeed, 'Error loading file: ' + SR.Name);
   until FindNext(SR) <> 0;
  finally
   // Must free up resources used by these successful finds
   FindClose(SR);
  end;
end;

procedure TestAudioFileAIFF.TestBasicWriting;
var
  TempStream : TMemoryStream;
begin
 TempStream := TMemoryStream.Create;
 with TempStream do
  try
   with FAudioFileAIFF do
    begin
     Name := RCStrTestName;
     Author := RCStrTestAuthor;
     Copyright := RCStrTestCopyright;
     SampleFrames := 100;
     SaveToStream(TempStream);
     Position := 0;
     LoadFromStream(TempStream);

     CheckTrue(Name = RCStrTestName);
     CheckTrue(Author = RCStrTestAuthor);
     CheckTrue(Copyright = RCStrTestCopyright);
    end;
  finally
   Free;
  end;
end;

procedure TestAudioFileAIFF.DecodeSimpleHandler(Sender: TObject;
  const Coder: TCustomChannelDataCoder; var Position: Cardinal);
var
  Channel  : Cardinal;
begin
 assert(Coder is TCustomChannel32DataCoder);
 with TCustomChannel32DataCoder(Coder) do
  for Channel := 0 to ChannelCount - 1
   do FillChar(ChannelPointer[Channel]^[0], SampleFrames * SizeOf(Single), 0);
end;

procedure TestAudioFileAIFF.DecodeHandler(Sender: TObject;
  const Coder: TCustomChannelDataCoder; var Position: Cardinal);
var
  Channel  : Cardinal;
begin
 assert(Coder is TCustomChannel32DataCoder);
 with TCustomChannel32DataCoder(Coder) do
  for Channel := 0 to ChannelCount - 1
   do Move(ChannelPointer[Channel]^[0], FAudioData[Channel]^[Position],
        SampleFrames * SizeOf(Single));
 // Position := Position + Coder.SampleFrames; // not necessary, incremented by caller!
end;

procedure TestAudioFileAIFF.EncodeSimpleHandler(Sender: TObject;
  const Coder: TCustomChannelDataCoder; var Position: Cardinal);
var
  Channel  : Cardinal;
begin
 assert(Coder is TCustomChannel32DataCoder);
 with TCustomChannel32DataCoder(Coder) do
  for Channel := 0 to ChannelCount - 1
   do FillChar(ChannelPointer[Channel]^[0], SampleFrames * SizeOf(Single), 0);
end;

procedure TestAudioFileAIFF.EncodeHandler(Sender: TObject;
  const Coder: TCustomChannelDataCoder; var Position: Cardinal);
var
  Channel  : Cardinal;
begin
 assert(Coder is TCustomChannel32DataCoder);
 with TCustomChannel32DataCoder(Coder) do
  for Channel := 0 to ChannelCount - 1
   do Move(FAudioData[Channel]^[Position], ChannelPointer[Channel]^[0],
        SampleFrames * SizeOf(Single));
 // Position := Position + Coder.SampleFrames; // not necessary, incremented by caller!
end;

procedure TestAudioFileAIFF.TestAdvancedWriting;
var
  TempStream  : TMemoryStream;
  TestEpsilon : Single;

  procedure ChannelLoop;
  var
    TestChannel : Integer;
    Channel     : Integer;
    Sample      : Integer;
  begin
   for TestChannel := 1 to 8 do
  with FAudioFileAIFF do
   begin
    // setup wave file
    SampleFrames := 100;
    ChannelCount := TestChannel;

    // store data
    SetLength(FAudioData, ChannelCount);
    for Channel := 0 to ChannelCount - 1 do
     begin
      GetMem(FAudioData[Channel], SampleFrames * SizeOf(Single));
      for Sample := 0 to SampleFrames - 1
       do FAudioData[Channel]^[Sample] := (Channel + 1) / ChannelCount;
     end;

    // reset stream and write to stream
    TempStream.Clear;
    SaveToStream(TempStream);
(*
    SaveToFile('Test WAVE - Channels ' + IntToStr(TestChannel) + ' ' +
      'Bits ' + IntToStr(BitsPerSample) + '.wav');
*)

    // load from stream
    TempStream.Position := 0;
    LoadFromStream(TempStream);

    // check data
    for Channel := 0 to ChannelCount - 1 do
     for Sample := 0 to SampleFrames - 1 do
      begin
       CheckTrue(abs(FAudioData[Channel]^[Sample] * ChannelCount - (Channel + 1)) < TestEpsilon * ChannelCount,
         'Expected: ' + FloatToStr((Channel + 1) / ChannelCount) + ', ' +
         'but was: ' + FloatToStr(FAudioData[Channel]^[Sample]) + ' ' +
         '(Channel: ' + IntToStr(Channel) + ' Sample: ' + IntToStr(Sample) + ')');
      end;
   end;
   end;

begin
 TempStream := TMemoryStream.Create;
 with TempStream do
  try
   with FAudioFileAIFF do
    begin
     // write 100 samples of zeroes and discard
     SampleFrames := 100;
     OnEncode := EncodeSimpleHandler;
     OnDecode := DecodeSimpleHandler;
     SaveToStream(TempStream);
     TempStream.Position := 0;
     LoadFromStream(TempStream);

     OnEncode := EncodeHandler;
     OnDecode := DecodeHandler;

     // integer encoding
     Encoding := aeInteger;
     BitsPerSample := 8;
     TestEpsilon := 5E-1;
     ChannelLoop;

     BitsPerSample := 16;
     TestEpsilon := 5E-4;
     ChannelLoop;

     BitsPerSample := 20;
     TestEpsilon := 1E-5;
     ChannelLoop;

     BitsPerSample := 24;
     TestEpsilon := 5E-7;
     ChannelLoop;

     BitsPerSample := 32;
     TestEpsilon := 5E-8;
     ChannelLoop;

     // float encoding
     Encoding := aeFloat;
     BitsPerSample := 32;
     ChannelLoop;

(*
     BitsPerSample := 64;
     TestEpsilon := 1E-7;
     ChannelLoop;

     BitsPerSample := 16;
     TestEpsilon := 5E-3;
     ChannelLoop;
*)

     // alaw encoding
     Encoding := aeALaw;
     TestEpsilon := 5E-2;
     ChannelLoop;

     // mulaw encoding
     Encoding := aeMuLaw;
     TestEpsilon := 5E-2;
     ChannelLoop;
    end;
  finally
   Free;
  end;
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestAudioFileAIFF.Suite);

end.

