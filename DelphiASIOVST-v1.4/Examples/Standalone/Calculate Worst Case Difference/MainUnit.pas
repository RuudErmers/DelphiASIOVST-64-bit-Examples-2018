unit MainUnit;

interface

{$I DAV_Compiler.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, DAV_Types, DAV_AudioMemory, DAV_AudioData,
  DAV_AudioFile, DAV_AudioFileWAV, TeEngine, Series, TeeProcs, Chart;

type
  TCalculationThread = class(TThread)
  protected
    FAudioData         : array of TAudioMemory32;
    FCurrentOrder      : PIntegerArray;
    FWorstOrder        : PIntegerArray;
    FSampleFrames      : Integer;
    FTrial             : Integer;
    FWorstDifference   : Double;
    FMaximumDifference : Double;
    FAverageDifference : Double;
    FTimeStamp         : TDateTime;
    procedure Execute; override;
    procedure UpdateOrder;
    procedure CalculateDifference(out Maximum, Average: Double);
    procedure DisplayTrial;
    procedure DisplayInformation;
  public
    constructor Create(Files: TStrings);
    destructor Destroy; override;

    procedure BuildDifferenceSignal(FileName: TFileName);
    procedure SaveOrderToFile(FileName: TFileName);
  end;

  TFmCalculateWorstCaseDifference = class(TForm)
    StatusBar: TStatusBar;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    PCMain: TPageControl;
    TsMain: TTabSheet;
    ListBox: TListBox;
    BtAddFiles: TButton;
    BtClear: TButton;
    BtStartCalculation: TButton;
    BtBuildDifference: TButton;
    TsHistory: TTabSheet;
    CtHistory: TChart;
    SsWorst: TLineSeries;
    SsAverage: TLineSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtAddFilesClick(Sender: TObject);
    procedure BtClearClick(Sender: TObject);
    procedure BtStartCalculationClick(Sender: TObject);
    procedure BtBuildDifferenceClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FCalculationThread : TCalculationThread;
  end;

var
  FmCalculateWorstCaseDifference: TFmCalculateWorstCaseDifference;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  IniFiles, Math, DAV_Common;

var
  GIniFileName : TFileName;

{$IFDEF CPU64}
{$DEFINE PUREPASCAL}
{$ENDIF}

{ TCalculationThread }

constructor TCalculationThread.Create(Files: TStrings);
var
  Index            : Integer;
  GlobalSampleRate : Double;
begin
  inherited Create(True);

  SetLength(FAudioData, Files.Count);
  GetMem(FCurrentOrder, Files.Count * SizeOf(Integer));
  GetMem(FWorstOrder, Files.Count * SizeOf(Integer));

  FAudioData[0] := TAudioMemory32.Create;
  with TAudioDataCollection32.Create(nil) do
  try
    LoadFromFile(Files[0]);
    GlobalSampleRate := SampleRate;
    FSampleFrames := SampleFrames;
    FAudioData[0].SampleCount := SampleFrames;
    Move(ChannelDataPointer[0]^, FAudioData[0].DataPointer^,
      SampleFrames * SizeOf(Single));
  finally
    Free;
  end;

  for Index := 1 to Length(FAudioData) - 1 do
  begin
    FAudioData[Index] := TAudioMemory32.Create;
    with TAudioDataCollection32.Create(nil) do
    try
      LoadFromFile(Files[Index]);

      // check samplerate
      if not (SampleRate = GlobalSampleRate) then
        raise Exception.Create('Samplerate mismatch!');

      // check sampleframess
      if SampleFrames < FSampleFrames then
        FSampleFrames := SampleFrames;

      FAudioData[Index].SampleCount := Min(SampleFrames, FSampleFrames);

      Move(ChannelDataPointer[0]^, FAudioData[Index].DataPointer^,
        FAudioData[Index].SampleCount * SizeOf(Single));
    finally
      Free;
    end;
  end;

  if FSampleFrames = 0 then
    raise Exception.Create('No sample to process');

  FTrial := 0;

  // now start calculation
  Resume;
end;

destructor TCalculationThread.Destroy;
var
  Index : Integer;
begin
  FreeMem(FCurrentOrder);
  FreeMem(FWorstOrder);
  for Index := 0 to Length(FAudioData) - 1 do
    FreeAndNil(FAudioData[Index]);
  inherited;
end;

procedure TCalculationThread.UpdateOrder;
var
  Index  : Integer;
  IndPos : Integer;
  Temp   : PIntegerArray;
  Count  : Integer;
begin
  Count := Length(FAudioData);
  Assert(Count > 0);

  GetMem(Temp, Count * SizeOf(Integer));
  try
    for Index := 0 to Count - 1 do
      Temp^[Index] := Index;

    Index := 0;
    repeat
      IndPos := Random(Count);
      FCurrentOrder[Index] := Temp^[IndPos];
      if (Count - IndPos) > 0 then
        Move(Temp^[IndPos + 1], Temp^[IndPos], (Count - IndPos) * SizeOf(Integer));
      Inc(Index);
      Dec(Count);
    until Count <= 0;
  finally
    FreeMem(Temp);
  end;
end;

{$DEFINE UseDirectCalculateDifference}
{-$DEFINE PUREPASCAL}

{$IFDEF UseDirectCalculateDifference}
procedure DirectCalculateDifference(Data: PPointerArray; DataCount,
  SampleFrames: Integer; out Maximum, Average: Double);
{$IFDEF PUREPASCAL}
var
  Index       : Integer;
  SampleIndex : Integer;
  Difference  : Double;
  Sum         : array [0..1] of Single;
  SumDiff     : Extended;
begin
  SumDiff := 0;
  Maximum := 0;

  for SampleIndex := 0 to SampleFrames - 1 do
  begin
    Sum[0] := PDAVSingleFixedArray(Data^[0])^[SampleIndex];
    Sum[1] := PDAVSingleFixedArray(Data^[DataCount - 1])^[SampleIndex];
    for Index := 1 to DataCount - 1 do
    begin
      Sum[0] := Sum[0] + PDAVSingleFixedArray(Data^[Index])^[SampleIndex];
      Sum[1] := Sum[1] + PDAVSingleFixedArray(Data^[DataCount - 1 - Index])^[SampleIndex];
    end;
    Difference := Abs(Sum[0] - Sum[1]);
    SumDiff := SumDiff + Difference;
    if Difference > Maximum then
      Maximum := Difference;
  end;
{$ELSE}
asm
{$IFDEF CPU64}
{$ENDIF}
{$IFDEF CPU32}
    FLD1                             // ST(0) = 1
    MOV      [ESP - 4], ECX
    FILD     DWORD PTR [ESP - 4]     // store SampleFrames
    FDIVP                            // ST(0) = 1 / SampleFrames
    FLDZ                             // SumDiff := 0;
    FLDZ                             // Maximum := 0;
  
    PUSH     EBX
    PUSH     EDI
    PUSH     ESI
    XOR      EBX, EBX                // EBX = SampleIndex = 0

@OuterLoop:
    PUSH     ECX
    MOV      EDI, EAX                // EDI = Data^[0]
    LEA      ESI, EAX + 4 * EDX - 4  // EDI = Data^[DataCount - 1]
    MOV      ECX, EDX                // ECX = Datacount
  
    PUSH     EAX
  
    MOV      EAX, [EDI]
    FLD      DWORD PTR [EAX + 4 * EBX]
    MOV      EAX, [ESI]
    FLD      DWORD PTR [EAX + 4 * EBX]
    FXCH
    ADD      EDI, 4
    SUB      ESI, 4
    DEC      ECX

@InnerLoop:
    MOV      EAX, [EDI]
    FADD     DWORD PTR [EAX + 4 * EBX]
    FSTP     DWORD PTR [ESP - 4]
    FLD      DWORD PTR [ESP - 4]
    FXCH
  
    MOV      EAX, [ESI]
    FADD     DWORD PTR [EAX + 4 * EBX]
    FSTP     DWORD PTR [ESP - 4]
    FLD      DWORD PTR [ESP - 4]
    FXCH
  
    ADD      EDI, 4
    SUB      ESI, 4
    LOOP     @InnerLoop
  
  
    POP      EAX
  
    FSUBP                            // calculate difference
    FABS                             // calculate absolute difference
  
    FXCH     ST(2)
    FCOMI    ST(0), ST(2)            // compare values
    FCMOVB   ST(0), ST(2)            // eventually move ST(0) to ST(2)
    FXCH     ST(2)
    FADDP                            // add to sum difference
  
    INC      EBX                     // move to next sample
    POP      ECX
    LOOP     @OuterLoop
  
    POP      ESI
    POP      EDI
    POP      EBX

    FMULP    ST(2), ST(0)            // calculate maximum
    MOV      EAX, Maximum
    FSTP     QWORD PTR [EAX]
    MOV      EAX, Average
    FSTP     QWORD PTR [EAX]
{$ENDIF}
{$ENDIF}
end;
{$ENDIF}

procedure TCalculationThread.CalculateDifference(out Maximum, Average: Double);
var
  Index       : Integer;
  {$IFNDEF UseDirectCalculateDifference}
  SampleIndex : Integer;
  Sum         : array [0..1] of Single;
  {$ENDIF}
  Data        : array [0..1] of PPointerArray;
  {$IFNDEF UseDirectCalculateDifference}
  Difference  : Double;
  SumDiff     : Extended;
  {$ENDIF}
begin
  GetMem(Data[0], Length(FAudioData) * SizeOf(Pointer));
  GetMem(Data[1], Length(FAudioData) * SizeOf(Pointer));
  try
    for Index := 0 to Length(FAudioData) - 1 do
    begin
      Data[0]^[Index] := FAudioData[FCurrentOrder^[Index]].DataPointer;
      Data[1]^[Index] := FAudioData[FCurrentOrder^[Length(FAudioData) - 1 - Index]].DataPointer;
    end;

    {$IFDEF UseDirectCalculateDifference}
    DirectCalculateDifference(Data[0], Length(FAudioData), FSampleFrames, Maximum,
      Average);
    {$ELSE}
    SumDiff := 0;
    Maximum := 0;

    for SampleIndex := 0 to FSampleFrames - 1 do
    begin
      Sum[0] := PDAVSingleFixedArray(Data[0]^[0])^[SampleIndex];
      Sum[1] := PDAVSingleFixedArray(Data[1]^[0])^[SampleIndex];
      for Index := 1 to Length(FAudioData) - 1 do
      begin
        Sum[0] := Sum[0] + PDAVSingleFixedArray(Data[0]^[Index])^[SampleIndex];
        Sum[1] := Sum[1] + PDAVSingleFixedArray(Data[1]^[Index])^[SampleIndex];
      end;
      Difference := Abs(Sum[0] - Sum[1]);
      SumDiff := SumDiff + Difference;
      if Difference > Maximum then
        Maximum := Difference;
    end;
    Average := SumDiff / FSampleFrames;
    {$ENDIF}
  finally
    FreeMem(Data[0]);
    FreeMem(Data[1]);
  end;
end;

procedure TCalculationThread.DisplayTrial;
begin
  FmCalculateWorstCaseDifference.StatusBar.Panels[0].Text := 'Trial: ' +
    IntToStr(FTrial);
end;

procedure TCalculationThread.DisplayInformation;
var
  Index : Integer;
begin
  with FmCalculateWorstCaseDifference do
  begin
    StatusBar.Panels[1].Text := 'Average: ' + FloatToStrF(FAverageDifference, ffGeneral, 7, 7);
    StatusBar.Panels[2].Text := 'Maximum: ' + FloatToStrF(FMaximumDifference, ffGeneral, 7, 7) +
      ' (' + FloatToStrF(1 + Abs(Log2(FMaximumDifference + 1E-30)), ffGeneral, 4, 4) + ' Bit)';

    SsAverage.AddXY(FTrial, 1 + Abs(Log2(FAverageDifference + 1E-30)));
    SsWorst.AddXY(FTrial, 1 + Abs(Log2(FMaximumDifference + 1E-30)));
  end;

  with TStringList.Create do
  try
    for Index := 0 to Length(FAudioData) - 1 do
      Add(IntToStr(FWorstOrder[Index]));
    SaveToFile('Order.txt');
  finally
    Free;
  end;
end;

procedure TCalculationThread.BuildDifferenceSignal(
  FileName: TFileName);
var
  Index       : Integer;
  SampleIndex : Integer;
  TotalSum    : array [0..1] of Single;
  Data        : array [0..1] of PPointerArray;
  Difference  : PDAVSingleFixedArray;
begin
  with TAudioDataCollection32.Create(nil, 1, FSampleFrames) do
  try
    Difference := ChannelDataPointer[0];
    GetMem(Data[0], Length(FAudioData) * SizeOf(Pointer));
    GetMem(Data[1], Length(FAudioData) * SizeOf(Pointer));
    try
      for Index := 0 to Length(FAudioData) - 1 do
      begin
        Data[0]^[Index] := FAudioData[FWorstOrder^[Index]].DataPointer;
        Data[1]^[Index] := FAudioData[FWorstOrder^[Length(FAudioData) - 1 - Index]].DataPointer;
      end;

      for SampleIndex := 0 to FSampleFrames - 1 do
      begin
        TotalSum[0] := PDAVSingleFixedArray(Data[0]^[0])^[SampleIndex];
        TotalSum[1] := PDAVSingleFixedArray(Data[1]^[0])^[SampleIndex];
        for Index := 1 to Length(FAudioData) - 1 do
        begin
          TotalSum[0] := TotalSum[0] + PDAVSingleFixedArray(Data[0]^[Index])^[SampleIndex];
          TotalSum[1] := TotalSum[1] + PDAVSingleFixedArray(Data[1]^[Index])^[SampleIndex];
        end;
        Difference^[SampleIndex] :=TotalSum[0] - TotalSum[1];
      end;
    finally
      FreeMem(Data[0]);
      FreeMem(Data[1]);
    end;
    SaveToFile(FileName, 32, aeFloat);
  finally
    Free;
  end;
end;

procedure TCalculationThread.Execute;
var
  CurrentDifference : Double;
  Maximum, Average  : Double;
begin
  inherited;

  FTimeStamp := Now;
  Synchronize(DisplayTrial);
  repeat
    UpdateOrder;
    CalculateDifference(Maximum, Average);
    CurrentDifference := 2 * Maximum + Average;
    Inc(FTrial);

    if Now - FTimeStamp > 3E-6 then
    begin
      FTimeStamp := Now;
      Synchronize(DisplayTrial);
    end;

    if CurrentDifference > FWorstDifference then
    begin
      Move(FCurrentOrder^[0], FWorstOrder^[0], Length(FAudioData) * SizeOf(Integer));
      FMaximumDifference := Maximum;
      FAverageDifference := Average;
      FWorstDifference := CurrentDifference;
      Synchronize(DisplayInformation);
    end;
  until Terminated;
end;

procedure TCalculationThread.SaveOrderToFile(FileName: TFileName);
var
  ItemIndex : Integer;
begin
 with TStringList.Create do
 try
   for ItemIndex := 0 to Length(FAudioData) - 1 do
     Add(IntToStr(FCurrentOrder^[ItemIndex]));
 finally
   Free;
 end;
end;


{ TFmCalculateWorstCaseDifference }

procedure TFmCalculateWorstCaseDifference.FormCreate(Sender: TObject);
begin
  OpenDialog.Filter := GetSimpleFileFilter;
  SaveDialog.Filter := GetSimpleFileFilter;
end;

procedure TFmCalculateWorstCaseDifference.FormShow(Sender: TObject);
var
  Index   : Integer;
  Current : TFileName;
begin
  GIniFileName := ChangeFileExt(ParamStr(0), '.ini');

  with TIniFile.Create(GIniFileName) do
  try
    Index := 0;
    while ValueExists('Files', 'File ' + IntToStr(Index + 1)) do
    begin
      Current := ReadString('Files', 'File ' + IntToStr(Index + 1), '');
      if FileExists(Current) then
        ListBox.Items.Add(Current);
      Inc(Index);
    end;
  finally
    Free;
  end;

  BtStartCalculation.Enabled := ListBox.Count > 0;
  BtStartCalculation.Default := BtStartCalculation.Enabled;
  if BtStartCalculation.Default then
  begin
    BtAddFiles.Default := False;
    BtStartCalculation.SetFocus;
  end;
end;

procedure TFmCalculateWorstCaseDifference.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Assigned(FCalculationThread) then
  begin
    FCalculationThread.Terminate;
    FCalculationThread.WaitFor;
    FreeAndNil(FCalculationThread);
  end;
end;

procedure TFmCalculateWorstCaseDifference.BtAddFilesClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    ListBox.Items.AddStrings(OpenDialog.Files);
    BtStartCalculation.Enabled := ListBox.Count > 0;
    if BtStartCalculation.Enabled then
    begin
      BtAddFiles.Default := False;
      BtStartCalculation.Default := True;
      BtStartCalculation.SetFocus;
    end;
  end;

end;

procedure TFmCalculateWorstCaseDifference.BtBuildDifferenceClick(
  Sender: TObject);
begin
  if SaveDialog.Execute and Assigned(FCalculationThread) then
  case SaveDialog.FilterIndex of
    1 : FCalculationThread.BuildDifferenceSignal(SaveDialog.FileName);
    2 : FCalculationThread.SaveOrderToFile(SaveDialog.FileName);
  end;
end;

procedure TFmCalculateWorstCaseDifference.BtClearClick(Sender: TObject);
begin
  ListBox.Clear;
  BtStartCalculation.Enabled := False;
  BtStartCalculation.Default := False;
end;

procedure TFmCalculateWorstCaseDifference.BtStartCalculationClick(
  Sender: TObject);
var
  Index: Integer;
begin
  BtStartCalculation.Tag := 1 - BtStartCalculation.Tag;

  if BtStartCalculation.Tag = 1 then
  begin
    BtStartCalculation.Caption := '&Stop Calculation';

    SsWorst.Clear;
    SsAverage.Clear;

    with TIniFile.Create(GIniFileName) do
    try
      EraseSection('Files');
      for Index := 0 to ListBox.Count - 1 do
        WriteString('Files', 'File ' + IntToStr(Index + 1), ListBox.Items[Index]);
    finally
      Free;
    end;

    BtBuildDifference.Enabled := False;
    if Assigned(FCalculationThread) then
    begin
      FCalculationThread.Terminate;
      FCalculationThread.WaitFor;
      FreeAndNil(FCalculationThread);
    end;

    FCalculationThread := TCalculationThread.Create(ListBox.Items);
    BtBuildDifference.Enabled := True;

    TsHistory.Enabled := True;
    ListBox.Enabled := False;
  end
  else
  begin
    BtStartCalculation.Caption := '&Start Calculation';
    BtBuildDifference.Enabled := False;

    TsHistory.Enabled := False;
    ListBox.Enabled := True;

    if Assigned(FCalculationThread) then
    begin
      FCalculationThread.Terminate;
      FCalculationThread.WaitFor;
      FreeAndNil(FCalculationThread);
    end;
  end;
end;

end.
