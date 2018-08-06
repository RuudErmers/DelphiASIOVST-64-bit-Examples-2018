unit DAV_TestAudioFileWav;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der 
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, Classes, Contnrs, SysUtils, DAV_Common, DAV_ChunkClasses,
  DAV_ChunkWaveBasic, DAV_AudioFile, DAV_AudioFileWav, DAV_ChannelDataCoder;

type
  // Test methods for class TAudioFileWav
  TestAudioFileWav = class(TTestCase)
  strict private
    FAudioFileWav : TAudioFileWav;
    FAudioData    : TDAVArrayOfSingleFixedArray;
  protected
    procedure EncodeSimpleHandler(Sender: TObject; const Coder: TCustomChannelDataCoder; var Position: Cardinal);
    procedure DecodeSimpleHandler(Sender: TObject; const Coder: TCustomChannelDataCoder; var Position: Cardinal);
    procedure EncodeHandler(Sender: TObject; const Coder: TCustomChannelDataCoder; var Position: Cardinal);
    procedure DecodeHandler(Sender: TObject; const Coder: TCustomChannelDataCoder; var Position: Cardinal);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestScanning;
    procedure TestCreationDummy;
    procedure TestCreationEmpty;
    procedure TestBasicWriting;
    procedure TestAdvancedWriting;
    procedure TestRandomAccess;
  end;

implementation

uses
  Dialogs;

resourcestring
  RCStrTestTitle = 'Test Title';
  RCStrTestArtist = 'Test Artist';
  RCStrTestCutID = 'Test Cut ID';
  RCStrTestClientID = 'Test Client ID';
  RCStrTestCategory = 'Test Category';
  RCStrTestClassification = 'Test Classification';
  RCStrTestOutCue = 'Test Out Cue';
  RCStrTestStartDate = 'Test Start Date';
  RCStrTestEndDate = 'Test End Date';
  RCStrTestStartTime = 'Test Start Time';
  RCStrTestEndTime = 'Test End Time';
  RCStrTestProducerAppID = 'Test Producer App ID';
  RCStrTestProducerApp = 'Test Producer App Version';
  RCStrTestUserDef = 'Test User Def';
  RCStrTestDescription = 'Test Description';
  RCStrTestOriginator = 'Test Originator';
  RCStrTestOriginatorRef = 'Test Originator Ref';
  RCStrTestOriginationDate = 'Test Origination Date';
  RCStrTestOriginationTime = 'Test Origination Time';
  RCStrLabelChunk = 'Label Chunk';
  RCStrNoteChunk = 'Note Chunk';

procedure TestAudioFileWav.SetUp;
begin
  FAudioFileWav := TAudioFileWav.Create;
end;

procedure TestAudioFileWav.TearDown;
begin
 FreeAndNil(FAudioFileWav);
end;

procedure TestAudioFileWav.TestScanning;
var
  SR      : TSearchRec;
  succeed : Boolean;
begin
 if FindFirst('*.wav*', faAnyFile, SR) = 0 then
  try
   repeat
    Succeed := True;
    try
     FAudioFileWav.LoadFromFile(SR.Name)
    except
     on e: EWavError do MessageDlg(SR.Name + ': ' + e.Message, mtError, [mbOK], 0);
     else Succeed := False;
    end;
    Check(Succeed, 'Error loading file: ' + SR.Name);
   until FindNext(SR) <> 0;
  finally
   // Must free up resources used by these successful finds
   FindClose(SR);
  end;
end;

procedure TestAudioFileWav.TestCreationEmpty;
var
  TempStream : TMemoryStream;
begin
 TempStream := TMemoryStream.Create;
 try
  // create an empty file on the temp stream
  with TAudioFileWAV.Create(TempStream) do
   try
   finally
    Free;
   end;

  // reset stream
  TempStream.Position := 0;

  // load file from temp stream
  FAudioFileWav.LoadFromStream(TempStream);
 finally
  FreeAndNil(TempStream);
 end;
end;

procedure TestAudioFileWav.TestCreationDummy;
var
  TempStream : TMemoryStream;
begin
 TempStream := TMemoryStream.Create;
 try
  // create an empty file on the temp stream
  with TAudioFileWAV.Create(TempStream) do
   try
    SampleFrames := 1024;
    OnEncode := EncodeSimpleHandler;

    // write data in reversed order...
    Encode(1024, 1024);
    Encode(0, 1024);
   finally
    Free;
   end;

  // reset stream
  TempStream.Position := 0;

  // load file from temp stream
  FAudioFileWav.LoadFromStream(TempStream);
 finally
  FreeAndNil(TempStream);
 end;
end;

procedure TestAudioFileWav.TestBasicWriting;
var
  TempStream : TMemoryStream;
  Chunk      : TCustomChunk;
  I          : Integer;
begin
 TempStream := TMemoryStream.Create;
 with TempStream do
  try
(*
   FAudioFileWAV.Name := 'Test';
   FAudioFileWAV.Author := 'That''s me';
   FAudioFileWAV.Copyright := 'That''s also me';
*)
   with FAudioFileWAV do
    begin
     ChannelCount := 1;
     SampleFrames := 100;
     BitsPerSample := 16;
     Encoding := aeInteger;

     // junk chunk
     Chunk := TJunkChunk.Create;
     AddSubChunk(Chunk);

     // silent chunk
     Chunk := TSilentChunk.Create;
     TSilentChunk(Chunk).NumberOfSilentSamples := 17;
     AddSubChunk(Chunk);

     // pad chunk
     Chunk := TPadChunk.Create;
     AddSubChunk(Chunk);

     // instrument chunk
     Chunk := TInstrumentChunk.Create;
     with TInstrumentChunk(Chunk) do
      begin
       UnshiftedNote := 1;
       FineTune      := 1;
       Gain_dB       := 1;
       LowNote       := 1;
       HighNote      := 1;
       LowVelocity   := 1;
       HighVelocity  := 1;
      end;
     AddSubChunk(Chunk);

(*
     // sampler chunk
     Chunk := TSamplerChunk.Create;
     with TSamplerChunk(Chunk) do
      begin
       Manufacturer := mmSequentialCircuits;
       Product := 1;
       SamplePeriod := 1;
       MIDIUnityNote := 1;
       MIDIPitchFraction := 1;
       SMPTEFormat := so30Drop;
       SMPTEOffset := 1;
       with TLoopItem(LoopCollection.Add) do
        begin
         CuePointID := 0;
         LoopType   := 0;
         LoopStart  := 0;
         LoopEnd    := 0;
         Fraction   := 0;
         PlayCount  := 0;
        end;
      end;
     AddSubChunk(Chunk);
* )

     // note chunk
     Chunk := TNoteChunk.Create;
     TNoteChunk(Chunk).Note := RCStrNoteChunk;
     AddSubChunk(Chunk);
*)

     // label chunk
     Chunk := TLabelChunk.Create;
     TLabelChunk(Chunk).Text := RCStrLabelChunk;
     AddSubChunk(Chunk);

     // cue chunk
     Chunk := TCueChunk.Create;
     AddSubChunk(Chunk);

     // playlist chunk
     Chunk := TPlaylistChunk.Create;
     AddSubChunk(Chunk);

     // data list chunk
     Chunk := TAssociatedDataListChunk.Create;
     AddSubChunk(Chunk);

     // CART Chunk
     CartVersion := 1000;
     Title := RCStrTestTitle;
     Artist := RCStrTestArtist;
     CutID := RCStrTestCutID;
     ClientID := RCStrTestClientID;
     Category := RCStrTestCategory;
     Classification := RCStrTestClassification;
     OutCue := RCStrTestOutCue;
     StartDate := RCStrTestStartDate + ': ' + DateToStr(Now);
     EndDate := RCStrTestEndDate + ': ' + DateToStr(Now);
     StartTime := RCStrTestStartTime + ': ' + TimeToStr(Now);
     EndTime := RCStrTestEndTime + ': ' + TimeToStr(Now);
     ProducerAppID := RCStrTestProducerAppID;
     ProducerAppVersion := RCStrTestProducerApp;
     UserDef := RCStrTestUserDef;
     dbLevelReference := 10;

     // BEXT Chunk
     BextVersion := 1000;
     BextDescription := RCStrTestDescription;
     Originator := RCStrTestOriginator;
     OriginatorRef := RCStrTestOriginatorRef;
     OriginationDate := DateToStr(Now);
     OriginationTime := TimeToStr(Now);
     TimeRefLow := 10;
     TimeRefHigh := 20;

     // Save/Load stream!
     TempStream.Clear;
     SaveToStream(TempStream);
     TempStream.Position := 0;
     LoadFromStream(TempStream);

     // CART Chunk
     CheckTrue(CartVersion = 1000, 'Wrong Version');
     CheckTrue(Title = RCStrTestTitle, 'Expected: ' + RCStrTestTitle + ', but was: ' + Title);
     CheckTrue(Artist = RCStrTestArtist, 'Expected: ' + RCStrTestArtist + ', but was: ' + Artist);
     CheckTrue(CutID = RCStrTestCutID, 'Expected: ' + RCStrTestCutID + ', but was: ' + CutID);
     CheckTrue(ClientID = RCStrTestClientID, 'Expected: ' + RCStrTestClientID + ', but was: ' + ClientID);
     CheckTrue(Category = RCStrTestCategory, 'Expected: ' + RCStrTestCategory + ', but was: ' + Category);
     CheckTrue(Classification = RCStrTestClassification, 'Expected: ' + RCStrTestClassification + ', but was: ' + Classification);
     CheckTrue(OutCue = RCStrTestOutCue, 'Expected: ' + RCStrTestOutCue + ', but was: ' + OutCue);
     CheckTrue(ProducerAppID = RCStrTestProducerAppID, 'Expected: ' + RCStrTestProducerAppID + ', but was: ' + ProducerAppID);
     CheckTrue(ProducerAppVersion = RCStrTestProducerApp, 'Expected: ' + RCStrTestProducerApp + ', but was: ' + ProducerAppVersion);
     CheckTrue(UserDef = RCStrTestUserDef, 'Expected: ' + RCStrTestUserDef + ', but was: ' + UserDef);
     CheckTrue(dbLevelReference = 10, 'Expected: 10, but was: ' + IntToStr(dbLevelReference));

     // BEXT Chunk
     CheckTrue(BextVersion = 1000, 'Wrong Version');
     CheckTrue(BextDescription = RCStrTestDescription, 'Expected: ' + RCStrTestDescription + ', but was: ' + BextDescription);
     CheckTrue(Originator = RCStrTestOriginator, 'Expected: ' + RCStrTestOriginator + ', but was: ' + Originator);
     CheckTrue(OriginatorRef = RCStrTestOriginatorRef, 'Expected: ' + RCStrTestOriginatorRef + ', but was: ' + OriginatorRef);
     CheckTrue(TimeRefLow = 10, 'Expected: 10, but was: ' + IntToStr(TimeRefLow));
     CheckTrue(TimeRefHigh = 20, 'Expected: 20, but was: ' + IntToStr(TimeRefHigh));

     for I := 0 to SubChunkCount - 1 do
      if SubChunk[i] is TLabelChunk then
       with TLabelChunk(SubChunk[i]) do CheckTrue(Text = RCStrLabelChunk, 'Expected: ' + RCStrLabelChunk + ', but was: ' + Text) else
      if SubChunk[i] is TNoteChunk then
       with TNoteChunk(SubChunk[i]) do CheckTrue(Note = RCStrNoteChunk, 'Expected: ' + RCStrNoteChunk + ', but was: ' + Note) else
      if SubChunk[i] is TInstrumentChunk then
       with TInstrumentChunk(SubChunk[i]) do
        begin
         CheckTrue(UnshiftedNote = 1);
         CheckTrue(FineTune = 1);
         CheckTrue(Gain_dB = 1);
         CheckTrue(LowNote = 1);
         CheckTrue(HighNote = 1);
         CheckTrue(LowVelocity = 1);
         CheckTrue(HighVelocity = 1);
        end else
      if SubChunk[i] is TSamplerChunk then
       with TSamplerChunk(SubChunk[i]) do
        begin
         CheckTrue(Manufacturer = mmSequentialCircuits);
         CheckTrue(Product = 1);
         CheckTrue(SamplePeriod = 1);
         CheckTrue(MIDIUnityNote = 1);
         CheckTrue(MIDIPitchFraction = 1);
         CheckTrue(SMPTEFormat = so30Drop);
         CheckTrue(SMPTEOffset = 1);
         CheckTrue(NumSampleLoops = 1);
         CheckTrue(SamplerData = 1);
        end else
      if SubChunk[i] is TSilentChunk then
       with TSilentChunk(SubChunk[i]) do CheckTrue(NumberOfSilentSamples = 17, 'Expected: 17, but was: ' + IntToStr(NumberOfSilentSamples));
    end;
  finally
   Free;
  end;
end;

procedure TestAudioFileWav.DecodeSimpleHandler(Sender: TObject;
  const Coder: TCustomChannelDataCoder; var Position: Cardinal);
var
  Channel  : Cardinal;
begin
 assert(Coder is TCustomChannel32DataCoder);
 with TCustomChannel32DataCoder(Coder) do
  for Channel := 0 to ChannelCount - 1
   do FillChar(ChannelPointer[Channel]^[0], SampleFrames * SizeOf(Single), 0);
end;

procedure TestAudioFileWav.DecodeHandler(Sender: TObject;
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

procedure TestAudioFileWav.EncodeSimpleHandler(Sender: TObject;
  const Coder: TCustomChannelDataCoder; var Position: Cardinal);
var
  Channel  : Cardinal;
begin
 assert(Coder is TCustomChannel32DataCoder);
 with TCustomChannel32DataCoder(Coder) do
  for Channel := 0 to ChannelCount - 1
   do FillChar(ChannelPointer[Channel]^[0], SampleFrames * SizeOf(Single), 0);
end;

procedure TestAudioFileWav.EncodeHandler(Sender: TObject;
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

procedure TestAudioFileWav.TestAdvancedWriting;
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
    with FAudioFileWAV do
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
       begin
        for Sample := 0 to SampleFrames - 1 do
         begin
          CheckTrue(abs(FAudioData[Channel]^[Sample] * ChannelCount - (Channel + 1)) < TestEpsilon * ChannelCount,
            'Expected: ' + FloatToStr((Channel + 1) / ChannelCount) + ', ' +
            'but was: ' + FloatToStr(FAudioData[Channel]^[Sample]) + ' ' +
            '(Channel: ' + IntToStr(Channel) + ' Sample: ' + IntToStr(Sample) + ')');
         end;
        Dispose(FAudioData[Channel]);
       end;
     end;  
   end;

begin
 TempStream := TMemoryStream.Create;
 with TempStream do
  try
   with FAudioFileWAV do
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
     ChannelLoop;

     BitsPerSample := 64;
     TestEpsilon := 1E-7;
     ChannelLoop;

     BitsPerSample := 16;
     TestEpsilon := 5E-3;
     ChannelLoop;

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

procedure TestAudioFileWav.TestRandomAccess;
var
  TempStream  : TMemoryStream;
  Channel     : Integer;
  Sample      : Integer;
  AudioFile   : TAudioFileWAV;    

begin
 TempStream := TMemoryStream.Create;
 with TempStream do
  try
   with FAudioFileWAV do
    begin
     // setup wave file
     SampleFrames := 25000;
     ChannelCount := 1;

     OnEncode := EncodeHandler;
     OnDecode := DecodeHandler;

     BitsPerSample := 16;
     with FAudioFileWAV do
      begin
       // store data
       SetLength(FAudioData, ChannelCount);
       for Channel := 0 to ChannelCount - 1 do
        begin
         GetMem(FAudioData[Channel], SampleFrames * SizeOf(Single));
         for Sample := 0 to SampleFrames - 1
          do FAudioData[Channel]^[Sample] := Sample / SampleFrames;
        end;

       // reset stream and write to stream
       TempStream.Clear;
       SaveToStream(TempStream);
       TempStream.Position := 0;

       // clear audio data
       for Channel := 0 to ChannelCount - 1
        do FillChar(FAudioData[Channel]^, SampleFrames * SizeOf(Single), 0);

       AudioFile := TAudioFileWAV.Create(TempStream);
       try
        AudioFile.OnEncode := EncodeHandler;
        AudioFile.OnDecode := DecodeHandler;
        // decode in one go
        AudioFile.Decode(0, SampleFrames);

        // decode in two goes
        AudioFile.Decode(0, 100);
        AudioFile.Decode(100, SampleFrames - 100);

        // decode only parts
        AudioFile.Decode(0, 7000);
        AudioFile.Decode(20000, 5000);
        AudioFile.Decode(13000, 8000);
       finally
        FreeAndNil(AudioFile);
       end;

      for Channel := 0 to ChannelCount - 1 do
       begin
        for Sample := 0 to SampleFrames - 1
         do CheckTrue(abs(FAudioData[Channel]^[Sample] * SampleFrames - Sample) < 1E-3 * SampleFrames,
         'Expected: ' + FloatToStr(Sample / SampleFrames) + ', ' +
         'but was: ' + FloatToStr(FAudioData[Channel]^[Sample]) + ' ' +
         '(Sample: ' + IntToStr(Sample) + ')');

        Dispose(FAudioData[Channel]);
       end;
      end;

    end;
  finally
   Free;
  end;
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestAudioFileWav.Suite);

end.
