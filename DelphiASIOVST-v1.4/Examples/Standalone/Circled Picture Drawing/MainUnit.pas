unit MainUnit;

interface

{$I DAV_Compiler.inc}

// Compiler Switches

{-$DEFINE PUREPASCAL}
{$DEFINE UseInifiles}
{$DEFINE UseInline}
{$DEFINE OptimizedErrorCalculationLoop}
{-$DEFINE OnlyFinalBlend} // define this only in case the final blend is wrong

{$IFDEF MSWINDOWS}
{$DEFINE UseInifiles}
{$ENDIF}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, Menus, ExtCtrls, StdCtrls, ComCtrls, 
  ActnList, TeEngine, Series, TeeProcs, Chart, DAV_Common, DAV_Types,
  DAV_ChunkClasses, DAV_Bindings, DAV_GuiCommon, DAV_DifferentialEvolution,
  DAV_GuiPixelMap, DAV_GuiByteMap, DAV_FixedPoint, DAV_GuiVector,
  DAV_GuiVectorPixel, DAV_GuiFilters, DAV_GuiFiltersBlur,
  DAV_GuiVectorPixelCircle, DAV_GuiVectorPixelLine, DAV_GuiFileFormats,
  DAV_GuiPng;

type
  TEvolutionThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TFmPrimitivePictureEvolution = class(TForm)
    AcBack: TAction;
    AcNext: TAction;
    AcSettings: TAction;
    AcStart: TAction;
    ActionList: TActionList;
    MainMenu: TMainMenu;
    MiBack: TMenuItem;
    MiCopyReference: TMenuItem;
    MiCostMap: TMenuItem;
    MiCrosshair: TMenuItem;
    MiCurrentBestCost: TMenuItem;
    MiDisplay: TMenuItem;
    MiDrawing: TMenuItem;
    MiEvolve: TMenuItem;
    MiExit: TMenuItem;
    MiFile: TMenuItem;
    MiLoadPopulation: TMenuItem;
    MiLog: TMenuItem;
    MiNext: TMenuItem;
    MiOpenBest: TMenuItem;
    MiOpenDrawing: TMenuItem;
    MiOpenReference: TMenuItem;
    MiPreviousBest: TMenuItem;
    MiSaveAnimation: TMenuItem;
    MiSaveDrawing: TMenuItem;
    MiSaveFramed: TMenuItem;
    MiSaveHighResolution: TMenuItem;
    MiSavePopulation: TMenuItem;
    MiSaveResult: TMenuItem;
    MiScale2x: TMenuItem;
    MiScaleHalf: TMenuItem;
    MiSettings: TMenuItem;
    MiStart: TMenuItem;
    MiStopContinue: TMenuItem;
    MiStoreLog: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    OpenDialog: TOpenDialog;
    OpenDialogPrimitives: TOpenDialog;
    SaveDialog: TSaveDialog;
    SaveDialogPrimitives: TSaveDialog;
    StatusBar: TStatusBar;
    MiBackupPopulation: TMenuItem;
    PcMain: TPageControl;
    TsDrawing: TTabSheet;
    PaintBoxDraw: TPaintBox;
    PaintBoxRef: TPaintBox;
    TsOptimizationHistory: TTabSheet;
    CtOptimizationHistory: TChart;
    SeriesHistory: TFastLineSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure AcBackExecute(Sender: TObject);
    procedure AcNextExecute(Sender: TObject);
    procedure AcSettingsExecute(Sender: TObject);
    procedure AcStartExecute(Sender: TObject);
    procedure PaintBoxDrawPaint(Sender: TObject);
    procedure PaintBoxRefPaint(Sender: TObject);
    procedure MiCopyReferenceClick(Sender: TObject);
    procedure MiCostMapClick(Sender: TObject);
    procedure MiCrosshairClick(Sender: TObject);
    procedure MiCurrentBestCostClick(Sender: TObject);
    procedure MiExitClick(Sender: TObject);
    procedure MiLoadPopulationClick(Sender: TObject);
    procedure MiLogClick(Sender: TObject);
    procedure MiOpenBestClick(Sender: TObject);
    procedure MiOpenDrawingClick(Sender: TObject);
    procedure MiOpenReferenceClick(Sender: TObject);
    procedure MiPreviousBestClick(Sender: TObject);
    procedure MiSaveAnimationClick(Sender: TObject);
    procedure MiSaveDrawingClick(Sender: TObject);
    procedure MiSaveFramedClick(Sender: TObject);
    procedure MiSaveHighResolutionClick(Sender: TObject);
    procedure MiSavePopulationClick(Sender: TObject);
    procedure MiSaveResultClick(Sender: TObject);
    procedure MiScale2xClick(Sender: TObject);
    procedure MiScaleHalfClick(Sender: TObject);
    procedure MiStopContinueClick(Sender: TObject);
    procedure MiStoreLogClick(Sender: TObject);
    procedure MiBackupPopulationClick(Sender: TObject);
  private
    FWorstCost               : Double;
    FMaximumCost             : Double;
    FMaximumRadius           : Double;
    FLastBestCosts           : Double;
    FCumulatedError          : array [0..1] of PDAVDoubleFixedArray;
    FReference               : TGuiCustomPixelMap;
    FDrawing                 : TGuiCustomPixelMap;
    FNewDrawing              : TGuiCustomPixelMap;
    FBestDrawing             : TGuiCustomPixelMap;
    FCostMap                 : TGuiCustomByteMap;
    FBackgroundColor         : TPixel32;
    FIniFileName             : TFileName;
    FDiffEvol                : TDifferentialEvolution;
    FEvolution               : TEvolutionThread;
    FCircles                 : array of TGuiPixelFilledCircle;
    FArea                    : Integer;
    FCurrentCircle           : Integer;
    FCurrentOrder            : Integer;
    FTrialCount              : Integer;
    FTrialsPerCircle         : Integer;
    FMaximumDimension        : Integer;
    FUpdateTrials            : Integer;
    FTrialsSinceUpdate       : Integer;
    FInitialSeed             : Integer;
    FNumberOfCircles         : Integer;
    FCrossover               : Single;
    FAutoNextTrial           : Boolean;
    FReinitializeCount       : Integer;
    FReInitializeIndex       : Integer;
    FCorrectColor            : Boolean;
    FCorrectRadius           : Boolean;
    FCorrectPosition         : Boolean;
    FCorrectInvisibleCircles : Boolean;
    FCostScale               : Single;
    FCostLog                 : TStringList;
    FRandomCircle            : Boolean;
    FRandomOrder             : Boolean;
    FChangeOrder             : Boolean;
    FReduceHighCosts         : Boolean;
    FWeightDither            : Boolean;
    FAutoInitialSeed         : Boolean;
    FWeight                  : Single;
    FBest                    : Single;
    FAdditional              : Single;
    FDrawDraft               : Boolean;
    FCirclesPerSecond        : Integer;
    FTimeStamp               : Int64;
    FPreferedPosition        : TPoint;
    procedure SetInitialSeed(const Value: Integer);
    procedure SetTrialsPerCircle(const Value: Integer);
    procedure SetUpdateTrials(const Value: Integer);
    procedure SetNumberOfCircles(const Value: Integer);
    procedure SetCrossover(const Value: Single);
    procedure SetWeight(const Value: Single);
    procedure SetBest(const Value: Single);
    procedure SetAdditional(const Value: Single);
    procedure SetRandomOrder(const Value: Boolean);
    procedure SetReduceHighCosts(const Value: Boolean);
    function GetImageHeight: Integer;
    function GetImageWidth: Integer;
  protected
    {$IFDEF DARWIN}
    AppMenu     : TMenuItem;
    AppAboutCmd : TMenuItem;
    AppSep1Cmd  : TMenuItem;
    AppPrefCmd  : TMenuItem;
    {$ENDIF}
    procedure AddCostLogString(Value: string);
    procedure Reinitialization;
    procedure NextTrial;
    procedure ReduceHighCostsChanged; virtual;
    procedure ResetCircles;
    procedure SaveDrawingBackup;
    procedure SavePopulationBackup; overload;
    procedure SavePopulationBackup(FileName: TFileName); overload;
    procedure LoadPopulationBackup(FileName: TFileName);
    procedure CalculateStaticCosts; virtual;
    procedure SaveDrawing(FileName: TFileName);
    procedure SaveDrawingHR(FileName: TFileName);
    procedure SaveDrawingFramed(FileName: TFileName);
    procedure SaveAnimationStatic(FileName: TFileName; ScaleFactor: Single);
    procedure SaveAnimation(FileName: TFileName; ScaleFactor: Single;
      Halflife: Integer = 0);
    procedure SaveAnimationAdvanced(FileName: TFileName; ScaleFactor: Single);
    procedure SaveAnimationBlow(FileName: TFileName; ScaleFactor: Single;
      Inverted: Boolean = False);
    procedure LoadDrawing(FileName: TFileName);
    procedure LoadBest(FileName: TFileName);
    procedure DrawPopulation(Population: TDifferentialEvolutionPopulation;
      PixelMap: TGuiCustomPixelMap);
  public
    function CalculateError(Sender: TObject; var Population: TDifferentialEvolutionPopulation): Double;
    procedure CalculateCostMap;
    procedure LoadReference(FileName: TFileName);
    procedure Evolve;
    procedure UpdateOptimizerSettings;
    procedure StartOptimization(InitializePopulation: Boolean = True);
    procedure DrawResults;

    property Additional: Single read FAdditional write SetAdditional;
    property AutoInitialSeed: Boolean read FAutoInitialSeed write FAutoInitialSeed;
    property AutoNextTrial: Boolean read FAutoNextTrial write FAutoNextTrial;
    property Best: Single read FBest write SetBest;
    property ChangeOrder: Boolean read FChangeOrder write FChangeOrder;
    property CorrectColor: Boolean read FCorrectColor write FCorrectColor;
    property CorrectInvisible: Boolean read FCorrectInvisibleCircles write FCorrectInvisibleCircles;
    property CorrectPosition: Boolean read FCorrectPosition write FCorrectPosition;
    property CorrectRadius: Boolean read FCorrectRadius write FCorrectRadius;
    property Crossover: Single read FCrossover write SetCrossover;
    property DifferentialEvolution: TDifferentialEvolution read FDiffEvol;
    property EvolutionThread: TEvolutionThread read FEvolution;
    property IniFileName: TFileName read FIniFileName;
    property InitialSeed: Integer read FInitialSeed write SetInitialSeed;
    property NumberOfCircles: Integer read FNumberOfCircles write SetNumberOfCircles;
    property RandomCircle: Boolean read FRandomCircle write FRandomCircle;
    property RandomOrder: Boolean read FRandomOrder write SetRandomOrder;
    property ReduceHighCosts: Boolean read FReduceHighCosts write SetReduceHighCosts;
    property ReinitializationCount: Integer read FReinitializeCount write FReinitializeCount;
    property TrialsPerCircle: Integer read FTrialsPerCircle write SetTrialsPerCircle;
    property UpdateTrials: Integer read FUpdateTrials write SetUpdateTrials;
    property Weight: Single read FWeight write SetWeight;
    property WeightDither: Boolean read FWeightDither write FWeightDither;

    property ImageWidth: Integer read GetImageWidth;
    property ImageHeight: Integer read GetImageHeight;
  end;

var
  FmPrimitivePictureEvolution: TFmPrimitivePictureEvolution;

type
  TErrorWeighting = function (Current, Reference: TPixel32): Single;
  TErrorWeightingLoop = function (Current, Reference: PPixel32Array;
    Size: Integer): Single;

var
  ErrorWeighting            : TErrorWeighting;
  BindingErrorWeighting     : TFunctionBinding;

  {$IFDEF OptimizedErrorCalculationLoop}
  ErrorWeightingLoop        : TErrorWeightingLoop;
  BindingErrorWeightingLoop : TFunctionBinding;
  {$ENDIF}

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, {$IFDEF UseInifiles} IniFiles, {$ENDIF} DAV_Math, DAV_Approximations,
  DAV_GuiBlend, SettingsUnit, ProgressBarUnit, AdditionalChunks, CostLogUnit,
  SaveAnimationUnit;

{ TEvolution }

procedure TEvolutionThread.Execute;
var
  Count : Integer;
begin
 Count := 0;
 while not Terminated do
  begin
   FmPrimitivePictureEvolution.Evolve;
   Inc(Count);

   if Count >= FmPrimitivePictureEvolution.UpdateTrials then
    begin
     Synchronize(FmPrimitivePictureEvolution.DrawResults);
     Count := 0;
     if FmPrimitivePictureEvolution.WindowState <> wsMinimized
      then Sleep(10);
    end;
  end;
end;

{$IFDEF DARWIN}
function GetInfoPlistString(const KeyName : string) : string;
var
  BundleRef : CFBundleRef;
  KeyRef    : CFStringRef;
  ValueRef  : CFTypeRef;
begin
  Result := '';
  BundleRef := CFBundleGetMainBundle;
  if BundleRef = nil then  {Executable not in an app bundle?}
    Exit;
  AnsiStrToCFStr(KeyName, KeyRef);
  try
    ValueRef := CFBundleGetValueForInfoDictionaryKey(BundleRef, KeyRef);
    if CFGetTypeID(ValueRef) <> CFStringGetTypeID then  {Value not a string?}
      Exit;
    Result := CFStrToAnsiStr(ValueRef);
  finally
    FreeCFRef(KeyRef);
    end;
end;
{$ENDIF}


{ TFmPrimitivePictureEvolution }

procedure TFmPrimitivePictureEvolution.FormCreate(Sender: TObject);
begin
 Randomize;

 // initialize default values
 FIniFileName := ExtractFilePath(ParamStr(0)) + 'CPD.ini';
 FAutoNextTrial := True;
 FAutoInitialSeed := False;
 FReinitializeCount := 3;
 FReInitializeIndex := 0;
 FInitialSeed := 1000;
 FTrialsPerCircle := 3000;
 FTrialsSinceUpdate := 30;
 FNumberOfCircles := 1;
 FWeight := 0.7;
 FBest := 0;
 FArea := 1;
 FCostScale := 1;
 FAdditional := 0;
 FDrawDraft := False;
 FRandomCircle := False;
 FRandomOrder := False;
 FWeightDither := False;
 FCorrectColor := True;
 FCorrectRadius := True;
 FCorrectPosition := True;
 FCorrectInvisibleCircles := True;
 FReduceHighCosts := False;

 FBackgroundColor.ARGB := $FFFFFFFF;

 // create pixel maps
 FReference := TGuiPixelMapMemory.Create;
 FDrawing := TGuiPixelMapMemory.Create;
 FNewDrawing := TGuiPixelMapMemory.Create;
 FBestDrawing := TGuiPixelMapMemory.Create;
 FCostMap := TGuiByteMapMemory.Create;

 // set paintboxes to opaque
 PaintBoxRef.ControlStyle := PaintBoxDraw.ControlStyle + [csOpaque];
 PaintBoxDraw.ControlStyle := PaintBoxDraw.ControlStyle + [csOpaque];

 // create cost log
 FCostLog := TStringList.Create;
 if FileExists(ExtractFilePath(ParamStr(0)) + 'Costs.log')
  then FCostLog.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Costs.log');
 SeriesHistory.Clear;

 // create and initialize differential evolution optimizer
 FDiffEvol := TDifferentialEvolution.Create(Self);
 with FDiffEvol do
  begin
   PopulationCount := FInitialSeed;
   VariableCount := 7 * FNumberOfCircles;
   CrossOver := 0.9;
   OnCalculateCosts := CalculateError;
  end;

 QueryPerformanceCounter(FTimeStamp);

 {$IFDEF DARWIN}
 AppMenu := TMenuItem.Create(Self);  {Application menu}
 AppMenu.Caption := #$EF#$A3#$BF;  {Unicode Apple logo char}
 MainMenu.Items.Insert(0, AppMenu);

(*
 AppAboutCmd := TMenuItem.Create(Self);
 AppAboutCmd.Caption := 'About ' + GetInfoPlistString('CFBundleName');
 AppAboutCmd.OnClick := AboutCmdClick;
 AppMenu.Add(AppAboutCmd);  {Add About as item in application menu}

 AppSep1Cmd := TMenuItem.Create(Self);
 AppSep1Cmd.Caption := '-';
 AppMenu.Add(AppSep1Cmd);
*)
 AppPrefCmd := TMenuItem.Create(Self);
 AppPrefCmd.Caption := 'Preferences...';
 AppPrefCmd.Shortcut := ShortCut(VK_OEM_COMMA, [ssMeta]);
 AppPrefCmd.OnClick := AcSettingsExecute;
 AppMenu.Add(AppPrefCmd);
 {$ENDIF}
end;

procedure TFmPrimitivePictureEvolution.FormDestroy(Sender: TObject);
var
  Index : Integer;
begin
 // stop and free evolution thread
 if Assigned(FEvolution) then
  begin
   FEvolution.Terminate;
   if FEvolution.Suspended
    then FEvolution.Suspended := False;
   FEvolution.WaitFor;
   FreeAndNil(FEvolution);
  end;

 // dispose cumulated error
 if Assigned(FCumulatedError[0]) then Dispose(FCumulatedError[0]);
 if Assigned(FCumulatedError[1]) then Dispose(FCumulatedError[1]);

 // free all circles
 for Index := 0 to Length(FCircles) - 1
  do FreeAndNil(FCircles[Index]);

 // free all pixel and byte maps
 FreeAndNil(FReference);
 FreeAndNil(FDrawing);
 FreeAndNil(FNewDrawing);
 FreeAndNil(FBestDrawing);
 FreeAndNil(FCostMap);

 // store cost log
 if MiStoreLog.Checked then
   FCostLog.SaveToFile(ExtractFilePath(ParamStr(0)) + 'Costs.log');

 // free misc.
 FreeAndNil(FDiffEvol);
 FreeAndNil(FCostLog);
end;

procedure TFmPrimitivePictureEvolution.FormShow(Sender: TObject);
begin
 {$IFDEF UseInifiles}
 with TIniFile.Create(FIniFileName) do
  try
   Left := ReadInteger('Layout', 'Left', Left);
   Top := ReadInteger('Layout', 'Top', Top);

   FAutoInitialSeed := ReadBool('Settings', 'Auto Initial Seed', FAutoInitialSeed);
   FAutoNextTrial := ReadBool('Settings', 'Auto Trials', True);
   FTrialsPerCircle := ReadInteger('Settings', 'Trials Per Circle', FTrialsPerCircle);
   FUpdateTrials := ReadInteger('Settings', 'Update Trials', FUpdateTrials);
   FInitialSeed := ReadInteger('Settings', 'Initial Seed', FInitialSeed);
   FAdditional := 0.01 * ReadInteger('Settings', 'Additional', Round(100 * FAdditional));
   FCrossover := 0.01 * ReadInteger('Settings', 'Crossover', Round(100 * FCrossover));
   FWeight := 0.01 * ReadInteger('Settings', 'Weight', Round(100 * FWeight));
   FBest := 0.01 * ReadInteger('Settings', 'Best', Round(100 * FBest));
   FReinitializeCount := ReadInteger('Settings', 'Reinitialization Count', FReinitializeCount);
   FWeightDither := ReadBool('Settings', 'Weight Dither', FWeightDither);
   FChangeOrder := ReadBool('Settings', 'Change Order', FChangeOrder);
   FRandomOrder := ReadBool('Settings', 'Random Order', FRandomOrder);
   FCorrectColor := ReadBool('Settings', 'Correct Color', FCorrectColor);
   FCorrectRadius := ReadBool('Settings', 'Correct Radius', FCorrectRadius);
   FCorrectPosition := ReadBool('Settings', 'Correct Position', FCorrectPosition);
   FCorrectInvisibleCircles := ReadBool('Settings', 'Correct Invisible Circles', FCorrectInvisibleCircles);
   FReduceHighCosts := ReadBool('Settings', 'Reduce High Costs', FReduceHighCosts);
   FRandomCircle := ReadBool('Settings', 'Random Circle', FRandomCircle);

   NumberOfCircles := ReadInteger('Settings', 'Number of Circles', FNumberOfCircles);
   LoadReference(ReadString('Recent', 'Reference', ''));
   LoadDrawing(ReadString('Recent', 'Drawing', ''));
  finally
   Free;
  end;
 {$ELSE}
 {$IFDEF Darwin}
 with TCFPreferences.Create do
  try
   Left := StrToInt(Prefs.GetAppString('Layout:Left'));
   Top := StrToInt(Prefs.GetAppString('Layout:Top'));

   FAutoInitialSeed := ReadBool('Settings', 'Auto Initial Seed', FAutoInitialSeed);
   FAutoNextTrial := ReadBool('Settings', 'Auto Trials', True);
   FTrialsPerCircle := StrToInt(Prefs.GetAppString('Settings:Trials Per Circle');
   FUpdateTrials := StrToInt(Prefs.GetAppString('Settings:Update Trials');
   FInitialSeed := StrToInt(Prefs.GetAppString('Settings:Initial Seed');
   FAdditional := 0.01 * StrToInt(Prefs.GetAppString('Settings:Additional');
   FCrossover := 0.01 * StrToInt(Prefs.GetAppString('Settings:Crossover');
   FWeight := 0.01 * StrToInt(Prefs.GetAppString('Settings:Weight');
   FBest := 0.01 * StrToInt(Prefs.GetAppString('Settings:Best');
   FReinitializeCount := StrToInt(Prefs.GetAppString('Settings:Reinitialization Count');
   FWeightDither := ReadBool('Settings', 'Weight Dither', FWeightDither);
   FChangeOrder := ReadBool('Settings', 'Change Order', FChangeOrder);
   FRandomOrder := ReadBool('Settings', 'Random Order', FRandomOrder);
   FCorrectColor := ReadBool('Settings', 'Correct Color', FCorrectColor);
   FCorrectRadius := ReadBool('Settings', 'Correct Radius', FCorrectRadius);
   FCorrectPosition := ReadBool('Settings', 'Correct Position', FCorrectPosition);
   FCorrectInvisibleCircles := ReadBool('Settings', 'Correct Invisible Circles', FCorrectInvisibleCircles);
   FReduceHighCosts := ReadBool('Settings', 'Reduce High Costs', FReduceHighCosts);
   FRandomCircle := ReadBool('Settings', 'Random Circle', FRandomCircle);

   NumberOfCircles := StrToInt(Prefs.GetAppString('Settings:Number of Circles');
   LoadReference(ReadString('Recent', 'Reference', ''));
   LoadDrawing(ReadString('Recent', 'Drawing', ''));
  finally
   Free;
  end;
 {$ENDIF}
 {$ENDIF}
end;

function TFmPrimitivePictureEvolution.GetImageHeight: Integer;
begin
 Result := FReference.Height;
end;

function TFmPrimitivePictureEvolution.GetImageWidth: Integer;
begin
 Result := FReference.Width;
end;

procedure TFmPrimitivePictureEvolution.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 {$IFDEF UseInifiles}
 with TIniFile.Create(FIniFileName) do
  try
   WriteInteger('Layout', 'Left', Left);
   WriteInteger('Layout', 'Top', Top);
  finally
   Free;
  end;
 {$ELSE}
 {$IFDEF Darwin}
 with TCFPreferences.Create do
  try
   Prefs.SetAppString('Layout:Left', IntToStr(Left));
   Prefs.SetAppString('Layout:Top', IntToStr(Top));
  finally
   Free;
  end;
 {$ENDIF}
 {$ENDIF}
end;

procedure TFmPrimitivePictureEvolution.AcBackExecute(Sender: TObject);
begin
 AcBack.Checked := True;
end;

procedure TFmPrimitivePictureEvolution.AcSettingsExecute(Sender: TObject);
begin
 with TFmSettings.Create(Self) do
  try
   if Assigned(EvolutionThread)
    then SePrimitiveCount.Enabled := EvolutionThread.Suspended
    else SePrimitiveCount.Enabled := True;
   ShowModal;
  finally
   Free;
  end;
end;

procedure TFmPrimitivePictureEvolution.AcStartExecute(Sender: TObject);
begin
 StartOptimization;
end;

procedure TFmPrimitivePictureEvolution.AcNextExecute(Sender: TObject);
begin
 AcNext.Checked := True;
end;

procedure TFmPrimitivePictureEvolution.MiBackupPopulationClick(Sender: TObject);
begin
 MiBackupPopulation.Checked := not MiBackupPopulation.Checked;
end;

procedure TFmPrimitivePictureEvolution.MiCopyReferenceClick(Sender: TObject);
begin
 FDrawing.Assign(FReference);
 FBestDrawing.Assign(FReference);
 if MiCostMap.Checked or FReduceHighCosts
  then CalculateCostMap;
 PaintBoxDraw.Invalidate;
end;

procedure TFmPrimitivePictureEvolution.MiCostMapClick(Sender: TObject);
begin
 (Sender As TMenuItem).Checked := True;
 if MiCostMap.Checked then CalculateCostMap;
 PaintBoxDraw.Invalidate;
end;

procedure TFmPrimitivePictureEvolution.MiCrosshairClick(Sender: TObject);
begin
 MiCrosshair.Checked := not MiCrosshair.Checked;
end;

procedure TFmPrimitivePictureEvolution.MiCurrentBestCostClick(Sender: TObject);
begin
 (Sender As TMenuItem).Checked := True;
 PaintBoxDraw.Invalidate;
end;

procedure TFmPrimitivePictureEvolution.MiExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmPrimitivePictureEvolution.MiOpenBestClick(Sender: TObject);
begin
 with OpenDialogPrimitives do
  begin
   if Execute
    then LoadBest(FileName);
  end;
end;

procedure TFmPrimitivePictureEvolution.MiOpenDrawingClick(Sender: TObject);
begin
 with OpenDialogPrimitives do
  begin
   if Execute then
    begin
     if FilterIndex = 1
      then LoadDrawing(FileName);
     with TIniFile.Create(FIniFileName) do
      try
       WriteString('Recent', 'Drawing', OpenDialogPrimitives.FileName);
      finally
       Free;
      end;
    end;
  end;
end;

procedure TFmPrimitivePictureEvolution.MiSaveDrawingClick(Sender: TObject);
begin
 with SaveDialogPrimitives do
  begin
   if Execute then
    begin
     if FilterIndex = 1
      then SaveDrawing(FileName);

     with TIniFile.Create(FIniFileName) do
      try
       WriteString('Recent', 'Drawing', SaveDialogPrimitives.FileName);
      finally
       Free;
      end;
    end;
  end;
end;

procedure TFmPrimitivePictureEvolution.MiSaveFramedClick(Sender: TObject);
begin
 with SaveDialog do
  begin
   if Execute
    then SaveDrawingFramed(FileName);
  end;
end;

procedure TFmPrimitivePictureEvolution.MiSaveAnimationClick(Sender: TObject);
begin
 with TFmSaveAnimation.Create(Self) do
  try
   if (ShowModal = mrOK) and DirectoryExists(EdDirectory.Text) then
    case CbStyle.ItemIndex of
      0 : SaveAnimationStatic(EdDirectory.Text, SEScale.Value * 0.01);
      1 : SaveAnimation(EdDirectory.Text, SEScale.Value * 0.01, SEHalfLife.Value);
      2 : SaveAnimationAdvanced(EdDirectory.Text, SEScale.Value * 0.01);
      3 : SaveAnimationBlow(EdDirectory.Text, SEScale.Value * 0.01);
    end;
  finally
   Free;
  end;
end;

procedure TFmPrimitivePictureEvolution.MiSaveHighResolutionClick(Sender: TObject);
begin
 with SaveDialog do
  begin
   if Execute
    then SaveDrawingHR(FileName);
  end;
end;

procedure TFmPrimitivePictureEvolution.MiSavePopulationClick(Sender: TObject);
begin
 SavePopulationBackup('Backup.pop');
end;

procedure TFmPrimitivePictureEvolution.MiLoadPopulationClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   Filter := 'Population Backup (*.pop)|*.pop';
   DefaultExt := '.pop';
   if Execute
    then LoadPopulationBackup(FileName);
  finally
   Free;
  end;
end;

procedure TFmPrimitivePictureEvolution.MiLogClick(Sender: TObject);
begin
 FmCostLog.LogCost.Lines.Assign(FCostLog);
 FmCostLog.Show;
end;

procedure TFmPrimitivePictureEvolution.MiSaveResultClick(Sender: TObject);
begin
 with SaveDialog do
  begin
   if Execute
    then FBestDrawing.SaveToFile(FileName);
  end;
end;

procedure TFmPrimitivePictureEvolution.MiOpenReferenceClick(Sender: TObject);
begin
 with OpenDialog do
  begin

   if Execute then
    begin
     LoadReference(FileName);
     with TIniFile.Create(FIniFileName) do
      try
       WriteString('Recent', 'Reference', OpenDialog.FileName);
      finally
       Free;
      end;
    end;
  end;
end;

procedure TFmPrimitivePictureEvolution.MiPreviousBestClick(Sender: TObject);
begin
 (Sender As TMenuItem).Checked := True;
 PaintBoxDraw.Invalidate;
end;

procedure TFmPrimitivePictureEvolution.MiStopContinueClick(Sender: TObject);
begin
 MiStopContinue.Tag := 1 - MiStopContinue.Tag;

 if MiStopContinue.Tag = 0 then
  begin
   if Assigned(FEvolution)
    then FEvolution.Suspended := False;

   MiSaveDrawing.Enabled := False;
   MiStopContinue.Caption := 'St&op';
   with StatusBar do
    begin
     Panels[0].Text := 'Running';
     Panels[1].Text := 'Circles: ' + IntToStr(FCurrentCircle + FNumberOfCircles);
     Panels[2].Text := 'Trials: 0';
    end;
  end
 else
  begin
   if Assigned(FEvolution)
    then FEvolution.Suspended := True;

   // save population backup
   SavePopulationBackup;

   MiSaveDrawing.Enabled := True;
   MiStopContinue.Caption := 'C&ontinue';
   StatusBar.Panels[0].Text := 'Paused';
  end;
end;

procedure TFmPrimitivePictureEvolution.SetAdditional(const Value: Single);
begin
 if (FAdditional <> Value) and (Value >= 0) and (Value <= 1) then
  begin
   FAdditional := Value;
   FDiffEvol.GainR3 := Value;
  end;
end;

procedure TFmPrimitivePictureEvolution.SetBest(const Value: Single);
begin
 if (FBest <> Value) and (Value >= 0) and (Value <= 1) then
  begin
   FBest := Value;
   FDiffEvol.GainBest := Value;
  end;
end;

procedure TFmPrimitivePictureEvolution.SetCrossover(const Value: Single);
begin
 if (FCrossover <> Value) and (Value >= 0) and (Value <= 1) then
  begin
   FCrossover := Value;
   FDiffEvol.CrossOver := Value;
  end;
end;

procedure TFmPrimitivePictureEvolution.SetInitialSeed(const Value: Integer);
begin
 if (FInitialSeed <> Value) and (Value > 0)
  then FInitialSeed := Value;
end;

procedure TFmPrimitivePictureEvolution.SetNumberOfCircles(const Value: Integer);
begin
 if (FNumberOfCircles <> Value) and (Value > 0) then
  begin
   FNumberOfCircles := Value;
   FDiffEvol.VariableCount := 7 * FNumberOfCircles;
  end;
end;

procedure TFmPrimitivePictureEvolution.SetRandomOrder(const Value: Boolean);
begin
 FRandomOrder := (FNumberOfCircles = 1) and Value;
end;

procedure TFmPrimitivePictureEvolution.SetReduceHighCosts(const Value: Boolean);
begin
 if FReduceHighCosts <> Value then
  begin
   FReduceHighCosts := Value;
   ReduceHighCostsChanged;
  end;
end;

procedure TFmPrimitivePictureEvolution.ReduceHighCostsChanged;
begin
 if FReduceHighCosts
  then CalculateCostMap;
end;

procedure TFmPrimitivePictureEvolution.SetTrialsPerCircle(const Value: Integer);
begin
 if (FTrialsPerCircle <> Value) and (Value > 0) then
  begin
   FTrialsPerCircle := Value;
   if FUpdateTrials > FTrialsPerCircle
    then FUpdateTrials := FTrialsPerCircle;
  end;
end;

procedure TFmPrimitivePictureEvolution.SetUpdateTrials(const Value: Integer);
begin
 if (FUpdateTrials <> Value) and (Value > 0) and (FUpdateTrials <= FTrialsPerCircle)
  then FUpdateTrials := Value;
end;

procedure TFmPrimitivePictureEvolution.SetWeight(const Value: Single);
begin
 if (FWeight <> Value) and (Value >= 0) and (Value <= 1) then
  begin
   FWeight := Value;
   FDiffEvol.GainR1 := -Value;
   FDiffEvol.GainR2 := Value;
  end;
end;


procedure AssignCircleData(Circle: TGuiPixelFilledCircle;
  Data: PDAV8DoubleArray);
begin
 with Circle do
  begin
   GeometricShape.CenterX := ConvertToFixed24Dot8(Data^[0]);
   GeometricShape.CenterY := ConvertToFixed24Dot8(Data^[1]);
   GeometricShape.Radius := ConvertToFixed24Dot8(Data^[2]);
   Color := HLSToRGB(Data^[3], Data^[4], Data^[5]);
   Alpha := Round($FF * Data^[6]);
  end;
end;

{$IFDEF UseInline}
function ErrorWeightingInline(Current, Reference: TPixel32): Single; inline;
var
  Value : Integer;
const
  CScale : Double = 1.5378700499807766243752402921953E-6;
begin
 Value := (Abs(Reference.B - Current.B) + Abs(Reference.G - Current.G) +
   Abs(Reference.R - Current.R));
 Result := CScale * (Value * (255 + 9 * Value));
end;
{$ENDIF}

function ErrorWeightingNative(Current, Reference: TPixel32): Single;
var
  Value : Integer;
const
  CSquaredOne255th : Double = 1.5378700499807766243752402921953E-5;
begin
 Value := (Abs(Reference.B - Current.B) + Abs(Reference.G - Current.G) +
   Abs(Reference.R - Current.R));
 Result := 0.1 * CSquaredOne255th * (Value * (255 + 9 * Value));
end;

{$IFNDEF PUREPASCAL}
function ErrorWeightingMMX(Current, Reference: TPixel32): Single;
const
  CBitMask : Integer = $00FFFFFF;
  CScale   : Single  = 1.5378700499807766243752402921953E-6;
var
  Data : Integer;
asm
    MOVD      MM0, CBitMask
    MOVD      MM1, EAX
    PAND      MM1, MM0
    MOVD      MM2, EDX
    PAND      MM2, MM0
    PSADBW    MM1, MM2
    MOVD      EAX, MM1
  
    // check if abs sum is zero
    TEST      EAX, EAX
    JS        @ZeroResult
  
    MOV       EDX, EAX
    IMUL      EAX, 9
    ADD       EAX, 255
    IMUL      EAX, EDX
    MOV       Data, EAX
    EMMS
    FILD      Data
    FMUL      CScale
  
    POP       ECX
    POP       EBP
    RET
  
@ZeroResult:
    FLDZ
end;

function ErrorWeightingSSE(Current, Reference: TPixel32): Single;
const
  CBitMask : Integer = $00FFFFFF;
  CScale   : Single  = 1.5378700499807766243752402921953E-6;
var
  Data : Integer;
asm
    MOVD      XMM0, CBitMask
    MOVD      XMM1, EAX
    PAND      XMM1, XMM0
    MOVD      XMM2, EDX
    PAND      XMM2, XMM0
    PSADBW    XMM1, XMM2
    MOVD      EAX, XMM1
  
    // check if abs sum is zero
    TEST      EAX, EAX
    JS        @ZeroResult
  
    MOV       EDX, EAX
    IMUL      EAX, 9
    ADD       EAX, 255
    IMUL      EAX, EDX
    MOV       Data, EAX
    FILD      Data
    FMUL      CScale
  
    POP       ECX
    POP       EBP
    RET
  
@ZeroResult:
    FLDZ
end;
{$ENDIF}


{$IFDEF OptimizedErrorCalculationLoop}
function ErrorWeightingLoopNative(Current, Reference: PPixel32Array; Size: Integer): Single;
var
  I     : Integer;
begin
 Result := 0;
 {$IFDEF UseInlining}
 for I := 0 to Size - 1
  do Result := Result + ErrorWeightingInline(Reference^[I], Current^[I])
 {$ELSE}
 for I := 0 to Size - 1
  do Result := Result + ErrorWeighting(Reference^[I], Current^[I])
 {$ENDIF}
end;

{$IFNDEF PUREPASCAL}
function ErrorWeightingLoopMMX(Current, Reference: PPixel32Array; Size: Integer): Single;
const
  CBitMask : Integer = $00FFFFFF;
  CScale   : Single  = 1.5378700499807766243752402921953E-6;
var
  Data : Integer;
asm
    FLDZ
    LEA       EAX, EAX + ECX * 4
    LEA       EDX, EDX + ECX * 4
    NEG       ECX
    JNL       @Done
  
    PUSH      EBX
  
@Start:
    MOVD      MM0, CBitMask
    MOVD      MM1, [EAX + 4 * ECX]
    PAND      MM1, MM0
    MOVD      MM2, [EDX + 4 * ECX]
    PAND      MM2, MM0
    PSADBW    MM1, MM2
    MOVD      EBX, MM1
  
    TEST      EBX, EBX
    JZ        @Continue
  
    MOV       Data, EBX
    IMUL      EBX, 9
    ADD       EBX, 255
    IMUL      EBX, Data
    MOV       Data, EBX
    EMMS
    FILD      Data
    FADDP
  
@Continue:
    ADD       ECX, 1
    JS        @Start
  
    POP       EBX
    FMUL      CScale
  
@Done:
end;

function ErrorWeightingLoopSSE(Current, Reference: PPixel32Array; Size: Integer): Single;
const
  CBitMask : Integer = $00FFFFFF;
  CScale   : Single  = 1.5378700499807766243752402921953E-6;
var
  Data : Integer;
asm
    FLDZ
    LEA       EAX, EAX + ECX * 4
    LEA       EDX, EDX + ECX * 4
    NEG       ECX
    JNL       @Done
  
    MOVD      XMM0, CBitMask
    PUSH      EBX
  
@Start:
    MOVD      XMM1, [EAX + 4 * ECX]
    PAND      XMM1, XMM0
    MOVD      XMM2, [EDX + 4 * ECX]
    PAND      XMM2, XMM0
    PSADBW    XMM1, XMM2
    MOVD      EBX, XMM1
  
    TEST      EBX, EBX
    JZ        @Continue
  
    MOV       Data, EBX
    IMUL      EBX, 9
    ADD       EBX, 255
    IMUL      EBX, Data
    MOV       Data, EBX
    FILD      Data
    FADDP
  
@Continue:
    ADD       ECX, 1
    JS        @Start
  
    POP       EBX
    FMUL      CScale

@Done:
end;
{$ENDIF}
{$ENDIF}

procedure TFmPrimitivePictureEvolution.ResetCircles;
var
  Index : Integer;
begin
 for Index := 0 to Length(FCircles) - 1
  do FreeAndNil(FCircles[Index]);

 // initialize first circle
 FCurrentCircle := 0;
 FCurrentOrder := 0;
end;

procedure TFmPrimitivePictureEvolution.StartOptimization(InitializePopulation: Boolean = True);
begin
 ResetCircles;

 // initialize evolution
 UpdateOptimizerSettings;
 if InitializePopulation
  then FDiffEvol.Initialize;
 CalculateStaticCosts;
 FTrialCount := 0;

 if Assigned(FEvolution) then
  begin
   FEvolution.Terminate;
   if FEvolution.Suspended
    then FEvolution.Suspended := False;
   FEvolution.WaitFor;
   FreeAndNil(FEvolution);
  end;

 FEvolution := TEvolutionThread.Create(True);
 FEvolution.Priority := tpLower;
 FEvolution.Start;
 MiStopContinue.Enabled := True;
 MiSaveDrawing.Enabled := False;
 MiSavePopulation.Enabled := True;
 MiNext.Enabled := True;
 MiBack.Enabled := True;

 // update status bar
 with StatusBar do
  begin
   Panels[0].Text := 'Running';
   Panels[1].Text := 'Circles: ' + IntToStr(FCurrentCircle + FNumberOfCircles);
   Panels[2].Text := 'Trials: ' + IntToStr(FTrialCount);
  end;
end;

procedure TFmPrimitivePictureEvolution.MiStoreLogClick(Sender: TObject);
begin
  MiStoreLog.Checked := not MiStoreLog.Checked;
end;

procedure TFmPrimitivePictureEvolution.UpdateOptimizerSettings;
var
  Index : Integer;
begin
  // calculate maximum radius
 FMaximumRadius := 1.5 * FMaximumDimension;
 if FMaximumRadius < 1 then FMaximumRadius := 1;

 with FDiffEvol do
  begin
   CrossOver := FCrossover;
   PopulationCount := FInitialSeed;
   for Index := 0 to FNumberOfCircles - 1 do
    begin
     MinConstraints[7 * Index + 0] := -0.5 * FReference.Width;
     MaxConstraints[7 * Index + 0] :=  1.5 * FReference.Width;
     MinConstraints[7 * Index + 1] := -0.5 * FReference.Height;
     MaxConstraints[7 * Index + 1] :=  1.5 * FReference.Height;
     MinConstraints[7 * Index + 2] :=  1;
     MaxConstraints[7 * Index + 2] :=  0.5 * FMaximumRadius;
     MinConstraints[7 * Index + 3] :=  0;
     MaxConstraints[7 * Index + 3] :=  1;
     MinConstraints[7 * Index + 4] :=  0;
     MaxConstraints[7 * Index + 4] :=  1;
     MinConstraints[7 * Index + 5] :=  0;
     MaxConstraints[7 * Index + 5] :=  1;
     MinConstraints[7 * Index + 6] :=  0;
     MaxConstraints[7 * Index + 6] :=  1;
    end;
  end;
end;

procedure TFmPrimitivePictureEvolution.CalculateCostMap;
var
  x, y      : Integer;
  Error     : Single;
  TrueWidth : Integer;
  PixelData : array [0..1] of PPixel32Array;
  ByteData  : PByteArray;
  ErrorByte : Byte;
  PosData   : array of TPoint;
begin
 TrueWidth := FReference.Width;
 Assert(TrueWidth = FDrawing.Width);
 Assert(TrueWidth <= FCostMap.Width);

 // build cost map
 for y := 0 to FCostMap.Height - 1 do
  begin
   PixelData[0] := FReference.ScanLine[y];
   PixelData[1] := FDrawing.ScanLine[y];
   ByteData := FCostMap.ScanLine[y];

   for x := 0 to TrueWidth - 1 do
    begin
     {$IFDEF UseInline}
     Error := ErrorWeightingInline(PixelData[1, x], PixelData[0, x]);
     {$ELSE}
     Error := ErrorWeighting(PixelData[1, x], PixelData[0, x]);
     {$ENDIF}
     ByteData[x] := Limit(Round(Error * $FF), 0, $FF);
    end;
  end;

 // apply filters
 for x := 0 to 1 do
  begin
   with TGuiStackBlurFilter.Create do
   try
    Radius := 0.01 * FMaximumDimension;
    Filter(FCostMap);
   finally
    Free;
   end;

   with TGuiNormalizeFilter.Create do
   try
    Filter(FCostMap);
   finally
    Free;
   end;
  end;

 // find higest cost positions
 SetLength(PosData, 0);
 ErrorByte := 0;
 for y := 0 to FCostMap.Height - 1 do
  begin
   ByteData := FCostMap.ScanLine[y];
   for x := 0 to TrueWidth - 1 do
    begin
     if ByteData^[x] = ErrorByte then
      begin
       SetLength(PosData, Length(PosData) + 1);
       PosData[Length(PosData) - 1].X := x;
       PosData[Length(PosData) - 1].Y := y;
      end else
     if ByteData^[x] > ErrorByte then
      begin
       SetLength(PosData, 1);
       PosData[0].X := x;
       PosData[0].Y := y;
       ErrorByte := ByteData^[x];
      end;
    end;
  end;

 FPreferedPosition := PosData[Random(Length(PosData))];
end;

function TFmPrimitivePictureEvolution.CalculateError(Sender: TObject;
  var Population: TDifferentialEvolutionPopulation): Double;
var
  TempData      : array [0..6] of Double;
  PixelData     : array [0..1] of PPixel32Array;
  PixelRange    : array [0..1] of Integer;
  CircleIndex   : Integer;
  Offset        : Integer;
  Radius        : Single;
  {$IFNDEF OptimizedErrorCalculationLoop}
  Index         : Integer;
  {$ENDIF}
  NewOffset     : Integer;
begin
 Assert(FReference <> nil);
 Assert(FDrawing <> nil);
 Assert(FNewDrawing <> nil);
 Assert(FBestDrawing <> nil);
 Radius := 1;

 with FNewDrawing do
  begin
   Assert(FReference.Width * FReference.Height = Width * Height);
   Assert(FDrawing.Width * FDrawing.Height = Width * Height);
   Assert(FNewDrawing.Width * FNewDrawing.Height = Width * Height);

   // eventually calculate radius
   if FReduceHighCosts
    then Radius := Sqrt(Sqr(Population[0] - FPreferedPosition.X) +
      Sqr(Population[1] - FPreferedPosition.Y));

   if FNumberOfCircles = 1 then
    begin
     // eventually set new x and y coordinates
     if FCorrectPosition then
      begin
       if (Population[0] < -Width) or (Population[0] > 2 * Width)
        then Population[0] := Mirror(Population[0], -0.5 * Width, 1.5 * Width);
       if (Population[1] < -Height) or (Population[1] > 2 * Height)
        then Population[1] := Mirror(Population[1], -0.5 * Height, 1.5 * Height);

       if FCorrectInvisibleCircles then
        begin
         if (Population[0] < 0) and (Population[2] < -Population[0])
          then Population[0] := -Population[2] + Random * (Width - Population[2]);
         if (Population[0] > Width) and (Population[2] < Population[0] - Width)
          then Population[0] := (Width + Population[2]) - Random * (Width - Population[2]);
         if (Population[1] < 0) and (Population[2] < -Population[1])
          then Population[1] := -Population[2] + Random * (Height - Population[2]);
         if (Population[1] > Height) and (Population[2] < Population[1] - Height)
          then Population[1] := (Height + Population[2]) - Random * (Height - Population[2]);
        end;

       if FReduceHighCosts then
        begin
         if Radius > 0.1 * FMaximumDimension then
          begin
           Population[0] := FPreferedPosition.X + 0.5 * (Population[0] - FPreferedPosition.X);
           Population[1] := FPreferedPosition.Y + 0.5 * (Population[1] - FPreferedPosition.Y);
           Radius := Sqrt(Sqr(Population[0] - FPreferedPosition.X) +
             Sqr(Population[1] - FPreferedPosition.Y));
          end;
        end;
      end;

     // eventually correct radius
     if FCorrectRadius then
      begin
       if (Population[2] < 0.5) or (Population[2] > FMaximumRadius)
        then Population[2] := 0.5 + Random * (FMaximumRadius - 1);

       if FCorrectInvisibleCircles then
        begin
         if (Population[0] < 0) and (Population[2] < -Population[0])
          then Population[2] := -Population[0] + Random * (FMaximumRadius + Population[0]);
         if (Population[0] > Width) and (Population[2] < Population[0] - Width)
          then Population[2] := (Population[0] - Width) + Random * (FMaximumRadius - Population[0] + Width);
         if (Population[1] < 0) and (Population[2] < -Population[1])
          then Population[2] := -Population[1] + Random * (FMaximumRadius + Population[1]);
         if (Population[1] > Height) and (Population[2] < Population[1] - Height)
          then Population[2] := (Population[1] - Height) + Random * (FMaximumRadius - Population[1] + Height);
        end;
      end;

     if FCorrectColor then
      begin
       if (Population[3] < 0) or (Population[3] > 1)
        then Population[3] := Mirror(Population[3]);
       if (Population[4] < 0) or (Population[4] > 1)
        then Population[4] := Mirror(Population[4]);
       if (Population[5] < 0) or (Population[5] > 1)
        then Population[5] := Mirror(Population[5]);
       if (Population[6] < 0) or (Population[6] > 1)
        then Population[6] := Mirror(Population[6]);
      end;

     if (Population[0] < -Width) or (Population[0] > 2 * Width) or
       ((Population[0] < 0) and (Population[2] < -Population[0])) or
       ((Population[0] > Width) and (Population[2] < (Population[0] - Width))) or
       (Population[1] < -Height) or (Population[1] > 2 * Height) or
       ((Population[1] < 0) and (Population[2] < -Population[1])) or
       ((Population[1] > Height) and (Population[2] < (Population[1] - Height))) or
       (Population[2] < 1) or (Population[2] > FMaximumRadius) or
       (Population[3] < 0) or (Population[3] > 1) or
       (Population[4] < 0) or (Population[4] > 1) or
       (Population[5] < 0) or (Population[5] > 1) or
       (Population[6] < 0) or (Population[6] > 1) then
      begin
       Result := FWorstCost;
       Exit;
      end;

     DrawPopulation(Population, FNewDrawing);
     Result := 0;

     PixelData[0] := FReference.DataPointer;
     PixelData[1] := DataPointer;

     PixelRange[0] := Round(Population[1] - Population[2] - 2) * Width;
     if PixelRange[0] <= 0
      then PixelRange[0] := 0
      else Result := FCumulatedError[0, PixelRange[0] - 1];

     PixelRange[1] := Round(Population[1] + Population[2] + 2) * Width;
     if PixelRange[1] >= Width * Height
      then PixelRange[1] := Width * Height
      else Result := Result + FCumulatedError[1, PixelRange[1]];

     {$IFDEF OptimizedErrorCalculationLoop}
     Result := Result + ErrorWeightingLoop(@PixelData[1, PixelRange[0]],
       @PixelData[0, PixelRange[0]], PixelRange[1] - PixelRange[0]);
     {$ELSE}

     {$IFDEF UseInline}
     for Index := PixelRange[0] to PixelRange[1] - 1
      do Result := Result + ErrorWeightingInline(PixelData[1, Index], PixelData[0, Index]);
     {$ELSE}
     for Index := PixelRange[0] to PixelRange[1] - 1
      do Result := Result + ErrorWeighting(PixelData[1, Index], PixelData[0, Index]);
     {$ENDIF}
     {$ENDIF}
    end
   else
    begin
     if FChangeOrder and (FNumberOfCircles > 2) then
      for CircleIndex := 0 to FNumberOfCircles - 1 do
       begin
        if Random > 0.1 then Break;

        // get offsets
        Offset := Random(FNumberOfCircles) * 7;
        repeat
         NewOffset := Random(FNumberOfCircles) * 7;
        until NewOffset <> Offset;

        Move(Population[Offset], TempData[0], SizeOf(TempData));
        Move(Population[NewOffset], Population[Offset], SizeOf(TempData));
        Move(TempData[0], Population[NewOffset], SizeOf(TempData));
       end;

     for CircleIndex := 0 to FNumberOfCircles - 1 do
      begin
       Offset := 7 * CircleIndex;

       if FRandomCircle and (Random < 0.01) then
        begin
         if Random < 0.3 then
          begin
           Population[Offset + 0] := (2 * Random - 0.5) * FReference.Width;
           Population[Offset + 1] := (2 * Random - 0.5) * FReference.Height;
           Population[Offset + 2] :=  1 + Random * (FMaximumRadius - 1);
          end;
         if Random < 0.7 then
          begin
           Population[Offset + 3] :=  Random;
           Population[Offset + 4] :=  Random;
           Population[Offset + 5] :=  Random;
           Population[Offset + 6] :=  Random;
          end;
        end;

       // eventually set new x and y coordinates
       if FCorrectPosition then
        begin
         if (Population[Offset] < -Width) or (Population[Offset] > 2 * Width)
          then Population[Offset] := (2 * Random - 0.5) * Width;
         if (Population[Offset + 1] < -Height) or (Population[Offset + 1] > 2 * Height)
          then Population[Offset + 1] := (2 * Random - 0.5) * Height;

         if FCorrectInvisibleCircles then
          begin
           if (Population[Offset] < 0) and (Population[Offset + 2] < -Population[Offset])
            then Population[Offset] := -Population[Offset + 2] + Random * (FMaximumRadius - Population[Offset + 2]);
           if (Population[Offset] > Width) and (Population[Offset + 2] < Population[Offset] - Width)
            then Population[Offset] := (Width + Population[Offset + 2]) - Random * (FMaximumRadius - Population[Offset + 2]);
           if (Population[Offset + 1] < 0) and (Population[Offset + 2] < -Population[Offset + 1])
            then Population[Offset + 1] := -Population[Offset + 2] + Random * (FMaximumRadius - Population[Offset + 2]);
           if (Population[Offset + 1] > Height) and (Population[Offset + 2] < Population[Offset + 1] - Height)
            then Population[Offset + 1] := (Height + Population[Offset + 2]) - Random * (FMaximumRadius - Population[Offset + 2]);
          end;
        end;

       // eventually correct radius
       if FCorrectRadius then
        begin
         if (Population[Offset + 2] < 1) or (Population[Offset + 2] > FMaximumRadius)
          then Population[Offset + 2] := 1 + Random * (FMaximumRadius - 1);

         if FCorrectInvisibleCircles then
          begin
           if (Population[Offset] < 0) and (Population[Offset + 2] < -Population[Offset])
            then Population[Offset + 2] := -Population[Offset] + Random * (FMaximumRadius + Population[Offset]);
           if (Population[Offset] > Width) and (Population[Offset + 2] < Population[Offset] - Width)
            then Population[Offset + 2] := (Population[Offset] - Width) + Random * (FMaximumRadius - Population[Offset] + Width);

           if (Population[Offset + 1] < 0) and (Population[Offset + 2] < -Population[Offset + 1])
            then Population[Offset + 2] := -Population[Offset + 1] + Random * (FMaximumRadius + Population[Offset + 1]);
           if (Population[Offset + 1] > Height) and (Population[Offset + 2] < Population[Offset + 1] - Height)
            then Population[Offset + 2] := (Population[Offset + 1] - Height) + Random * (FMaximumRadius - Population[Offset + 1] + Height);
          end;
        end;

       if FCorrectColor then
        begin
         if (Population[Offset + 3] < 0) or (Population[Offset + 3] > 1)
          then Population[Offset + 3] := Random;
         if (Population[Offset + 4] < 0) or (Population[Offset + 4] > 1)
          then Population[Offset + 4] := Random;
         if (Population[Offset + 5] < 0) or (Population[Offset + 5] > 1)
          then Population[Offset + 5] := Random;
         if (Population[Offset + 6] < 0) or (Population[Offset + 6] > 1)
          then Population[Offset + 6] := Random;
        end;

       if (Population[Offset] < -Width) or (Population[Offset] > 2 * Width) or
         ((Population[Offset] < 0) and (Population[Offset + 2] < -Population[Offset])) or
         ((Population[Offset] > Width) and (Population[Offset + 2] < (Population[Offset] - Width))) or
         (Population[Offset + 1] < -Height) or (Population[Offset + 1] > 2 * Height) or
         ((Population[Offset + 1] < 0) and (Population[Offset + 2] < -Population[Offset + 1])) or
         ((Population[Offset + 1] > Height) and (Population[Offset + 2] < (Population[Offset + 1] - Height))) or
         (Population[Offset + 2] < 1) or (Population[Offset + 2] > FMaximumRadius) or
         (Population[Offset + 3] < 0) or (Population[Offset + 3] > 1) or
         (Population[Offset + 4] < 0) or (Population[Offset + 4] > 1) or
         (Population[Offset + 5] < 0) or (Population[Offset + 5] > 1) or
         (Population[Offset + 6] < 0) or (Population[Offset + 6] > 1) or
         (Round($FF * Population[Offset + 6]) = 0) then
        begin
         Result := FWorstCost;
         Exit;
        end;
      end;

     DrawPopulation(Population, FNewDrawing);

     PixelData[0] := FReference.DataPointer;
     PixelData[1] := DataPointer;

     Result := 0;
     {$IFDEF OptimizedErrorCalculationLoop}
     Result := Result + ErrorWeightingLoop(@PixelData[1, 0], @PixelData[0, 0],
       Width * Height);
     {$ELSE}
     {$IFDEF UseInline}
     for Index := 0 to Width * Height - 1
      do Result := Result + ErrorWeightingInline(PixelData[1, Index], PixelData[0, Index]);
     {$ELSE}
     for Index := 0 to Width * Height - 1
      do Result := Result + ErrorWeighting(PixelData[1, Index], PixelData[0, Index]);
     {$ENDIF}
     {$ENDIF}
    end;
  end;

 // Result := Result * FCostScale; // <- do NOT use with current optimization

 if FReduceHighCosts and (Radius > 0.02 * FMaximumDimension)
  then Result := Result + 0.1 * (Radius - 0.02 * FMaximumDimension); // FArea *

 {$IFDEF UseApproximation}
 Result := 10 * FastLog2ContinousError4(1E-30 + Result);
 {$ELSE}
 Result := 10 * Log2(1E-30 + Result);
 {$ENDIF}
end;

procedure TFmPrimitivePictureEvolution.DrawPopulation(Population: TDifferentialEvolutionPopulation;
  PixelMap: TGuiCustomPixelMap);
var
  Index : Integer;
begin
 if FRandomOrder then
  begin
   // clear pixel map
   PixelMap.Clear(FBackgroundColor);

   // draw background circles
   if FDrawDraft then
    for Index := 0 to FCurrentOrder - 1
     do FCircles[Index].DrawDraft(PixelMap)
   else
    for Index := 0 to FCurrentOrder - 1
     do FCircles[Index].Draw(PixelMap);

   // increase circle counter
   Inc(FCirclesPerSecond, FCurrentOrder);

   // draw recent circle
   with TGuiPixelFilledCircle.Create do
    try
     GeometricShape.CenterX := ConvertToFixed24Dot8(Population[0]);
     GeometricShape.CenterY := ConvertToFixed24Dot8(Population[1]);
     GeometricShape.Radius := ConvertToFixed24Dot8(Population[2]);
     Color := HLSToRGB(Population[3], Population[4], Population[5]);
     Alpha := Round($FF * Population[6]);

     if FDrawDraft
      then DrawDraft(PixelMap)
      else Draw(PixelMap);
     Inc(FCirclesPerSecond);
    finally
     Free;
    end;

   // draw background circles
   if FDrawDraft then
    for Index := FCurrentOrder to Length(FCircles) - 1
     do FCircles[Index].DrawDraft(PixelMap)
   else
    for Index := FCurrentOrder to Length(FCircles) - 1
     do FCircles[Index].Draw(PixelMap);

   // increase circle counter
   Inc(FCirclesPerSecond, Length(FCircles) - FCurrentOrder);
  end
 else
  begin
   // copy recent drawing
   with PixelMap
    do Move(FDrawing.DataPointer^, DataPointer^, Width * Height * SizeOf(TPixel32));

   // draw latest circle
   with TGuiPixelFilledCircle.Create do
    try
     for Index := 0 to FNumberOfCircles - 1 do
      begin
       GeometricShape.CenterX := ConvertToFixed24Dot8(Population[7 * Index + 0]);
       GeometricShape.CenterY := ConvertToFixed24Dot8(Population[7 * Index + 1]);
       GeometricShape.Radius := ConvertToFixed24Dot8(Population[7 * Index + 2]);
       Color := HLSToRGB(Population[7 * Index + 3], Population[7 * Index + 4],
         Population[7 * Index + 5]);
       Alpha := Round($FF * Population[7 * Index + 6]);

       if FDrawDraft
        then DrawDraft(PixelMap)
        else Draw(PixelMap);

       // increase circle counter
       Inc(FCirclesPerSecond);
      end;
    finally
     Free;
    end;
  end;
end;

procedure TFmPrimitivePictureEvolution.DrawResults;
var
  NewTimeStamp : Int64;
  Frequency    : Int64;
  CircsPerSec  : Double;
  BestPop      : TDifferentialEvolutionPopulation;
begin
 BestPop := FDiffEvol.GetBestPopulation;
 DrawPopulation(BestPop, FBestDrawing);

 // save population backup
 if MiBackupPopulation.Checked then
   SavePopulationBackup;

 if MiCrosshair.Checked then
  with TGuiPixelThinLine.Create do
   try
    Color := clRed;
    Alpha := 192;

    GeometricShape.XA := ConvertToFixed24Dot8(BestPop[0] - BestPop[2] - Max(BestPop[2], 10));
    GeometricShape.XB := ConvertToFixed24Dot8(BestPop[0] - BestPop[2] - 2);
    GeometricShape.YA := ConvertToFixed24Dot8(BestPop[1]);
    GeometricShape.YB := ConvertToFixed24Dot8(BestPop[1]);
    Draw(FBestDrawing);

    GeometricShape.XA := ConvertToFixed24Dot8(BestPop[0] + BestPop[2] + Max(BestPop[2], 10));
    GeometricShape.XB := ConvertToFixed24Dot8(BestPop[0] + BestPop[2] + 2);
    Draw(FBestDrawing);

    GeometricShape.YA := ConvertToFixed24Dot8(BestPop[1] - BestPop[2] - Max(BestPop[2], 10));
    GeometricShape.YB := ConvertToFixed24Dot8(BestPop[1] - BestPop[2] - 2);
    GeometricShape.XA := ConvertToFixed24Dot8(BestPop[0]);
    GeometricShape.XB := ConvertToFixed24Dot8(BestPop[0]);
    Draw(FBestDrawing);

    StatusBar.Panels[6].Text := 'Radius: ' + FloatToStrF(BestPop[2], ffGeneral, 5, 5);
    GeometricShape.YA := ConvertToFixed24Dot8(BestPop[1] + BestPop[2] + Max(BestPop[2], 10));
    GeometricShape.YB := ConvertToFixed24Dot8(BestPop[1] + BestPop[2] + 2);
    Draw(FBestDrawing);
   finally
    Free;
   end;

 StatusBar.Panels[2].Text := 'Trials: ' + IntToStr(FTrialCount);
 StatusBar.Panels[3].Text := 'Costs: ' + FloatToStrF(FDiffEvol.GetBestCost - FMaximumCost, ffGeneral, 5, 5);
 StatusBar.Panels[4].Text := 'Global Costs: ' + FloatToStrF(FDiffEvol.GetBestCost, ffGeneral, 5, 5);

 // calculate circles per second
 QueryPerformanceCounter(NewTimeStamp);
 QueryPerformanceFrequency(Frequency);
 CircsPerSec := FCirclesPerSecond * Frequency / (NewTimeStamp - FTimeStamp);
 StatusBar.Panels[5].Text := 'Cicles Per Second: ' + FloatToStrF(CircsPerSec, ffGeneral, 5, 5);
 FCirclesPerSecond := 0;
 FTimeStamp := NewTimeStamp;

 PaintBoxDraw.Invalidate;
end;

procedure TFmPrimitivePictureEvolution.CalculateStaticCosts;
var
  CurrentCost : Double;
  DataSize    : Integer;
  PixelData   : array [0..1] of PPixel32Array;
  Index       : Integer;
begin
 // calculate worst cost
 {$IFDEF UseApproximation}
 FWorstCost := 10 * FastLog2ContinousError4(256 * 3 * FReference.Width * FReference.Height);
 {$ELSE}
 FWorstCost := 10 * Log2(256 * 3 * FReference.Width * FReference.Height);
 {$ENDIF}

 // calculate temporary data size
 DataSize := FReference.Width * FReference.Height * SizeOf(Double);

 // allocate memory for cumulated error
 ReallocMem(FCumulatedError[0], DataSize);
 ReallocMem(FCumulatedError[1], DataSize);

 with FDrawing do
  begin
   Assert(FReference.Width * FReference.Height = Width * Height);

   CurrentCost := 0;
   PixelData[0] := FReference.DataPointer;
   PixelData[1] := DataPointer;
   for Index := 0 to Width * Height - 1 do
    begin
     {$IFDEF UseInlining}
     CurrentCost := CurrentCost + ErrorWeightingInline(PixelData[1, Index], PixelData[0, Index]);
     {$ELSE}
     CurrentCost := CurrentCost + ErrorWeighting(PixelData[1, Index], PixelData[0, Index]);
     {$ENDIF}
     FCumulatedError[0, Index] := CurrentCost;
    end;

   if CurrentCost = 0
    then FMaximumCost := FWorstCost
    else
   {$IFDEF UseApproximation}
   FMaximumCost := 10 * FastLog2ContinousError4(CurrentCost);
   {$ELSE}
   FMaximumCost := 10 * Log2(CurrentCost);
   {$ENDIF}

   CurrentCost := 0;
   for Index := Width * Height - 1 downto 0 do
    begin
     CurrentCost := CurrentCost + ErrorWeighting(PixelData[1, Index],
       PixelData[0, Index]);
     FCumulatedError[1, Index] := CurrentCost;
    end;
  end;
end;

procedure TFmPrimitivePictureEvolution.SaveDrawingBackup;
var
  OldFileName : TFileName;
  NewFileName : TFileName;
begin
 OldFileName := 'Backup' + IntToStr(FCurrentCircle) + '.circles';
 FCurrentCircle := FCurrentCircle + FNumberOfCircles;
 NewFileName := 'Backup' + IntToStr(FCurrentCircle) + '.circles';
 SaveDrawing(NewFileName);
 if FileExists(OldFileName) and (FCurrentCircle mod 128 <> 0)
  then DeleteFile(OldFileName);

 with TIniFile.Create(FIniFileName) do
  try
   WriteString('Recent', 'Drawing', NewFileName);
  finally
   Free;
  end;
end;

procedure TFmPrimitivePictureEvolution.SavePopulationBackup;
var
  FileName : array [0..1] of TFileName;
begin
 FileName[0] := 'Backup' + IntToStr(FCurrentCircle) + '.pop';
 FileName[1] := FileName[0] + '.bak';
 if FileExists(FileName[1])
  then DeleteFile(FileName[1]);
 if FileExists(FileName[0])
  then RenameFile(FileName[0], FileName[1]);
 SavePopulationBackup(FileName[0]);
end;

procedure TFmPrimitivePictureEvolution.SavePopulationBackup(FileName: TFileName);
var
  Index             : Integer;
  VariableIndex     : Integer;
  CurrentPopulation : TPopulationChunk;
begin
 with TPopulationChunkContainer.Create do
  try
   for Index := 0 to FDiffEvol.PopulationCount - 1 do
    begin
     CurrentPopulation := TPopulationChunk.Create;
     CurrentPopulation.VariableCount := FDiffEvol.VariableCount;
     for VariableIndex := 0 to FDiffEvol.VariableCount - 1
      do CurrentPopulation.Variable[VariableIndex] := FDiffEvol.Population[Index][VariableIndex];
     AddChunk(CurrentPopulation);
    end;
   SaveToFile(FileName);
  finally
   Free;
  end;
end;

procedure TFmPrimitivePictureEvolution.MiScale2xClick(Sender: TObject);
var
  CircleIndex : Integer;
begin
 FDrawing.Clear(FBackgroundColor);
 FNewDrawing.Clear(FBackgroundColor);
 FBestDrawing.Clear(FBackgroundColor);
 for CircleIndex := 0 to Length(FCircles) - 1 do
  with FCircles[CircleIndex] do
   begin
    GeometricShape.CenterX := FixedMul(GeometricShape.CenterX, CFixed24Dot8Two);
    GeometricShape.CenterY := FixedMul(GeometricShape.CenterY, CFixed24Dot8Two);
    GeometricShape.Radius := FixedMul(GeometricShape.Radius, CFixed24Dot8Two);
    Draw(FDrawing);
    Draw(FBestDrawing);
    Draw(FNewDrawing);
   end;
end;

procedure TFmPrimitivePictureEvolution.MiScaleHalfClick(Sender: TObject);
var
  CircleIndex : Integer;
begin
 FDrawing.Clear(FBackgroundColor);
 FNewDrawing.Clear(FBackgroundColor);
 FBestDrawing.Clear(FBackgroundColor);
 for CircleIndex := 0 to Length(FCircles) - 1 do
  with FCircles[CircleIndex] do
   begin
    GeometricShape.CenterX := FixedMul(GeometricShape.CenterX, CFixed24Dot8Half);
    GeometricShape.CenterY := FixedMul(GeometricShape.CenterY, CFixed24Dot8Half);
    GeometricShape.Radius := FixedMul(GeometricShape.Radius, CFixed24Dot8Half);
    Draw(FDrawing);
    Draw(FBestDrawing);
    Draw(FNewDrawing);
   end;
end;

procedure TFmPrimitivePictureEvolution.LoadPopulationBackup(FileName: TFileName);
var
  Index         : Integer;
  VariableIndex : Integer;
begin
 with TPopulationChunkContainer.Create do
  try
   LoadFromFile(FileName);
   for Index := 0 to Count - 1 do
    for VariableIndex := 0 to Min(Population[Index].VariableCount, FDiffEvol.VariableCount) - 1
     do FDiffEvol.Population[Index][VariableIndex] := Population[Index].Variable[VariableIndex];
   FDiffEvol.Initialize(False);
   if not MiStopContinue.Enabled
    then StartOptimization(False);
  finally
   Free;
  end;
end;

procedure TFmPrimitivePictureEvolution.NextTrial;
var
  Index : Integer;
begin
 Inc(FReInitializeIndex);

 // randomize best population
 if FReInitializeIndex < FReInitializeCount then
  with FDiffEvol do
   begin
    for Index := 0 to VariableCount - 1
     do BestPopulation[Index] := MinConstraints[Index] *
       (MaxConstraints[Index] - MinConstraints[Index]);
   end
 else
  begin
   FReInitializeIndex := 0;
   FDiffEvol.Initialize;
  end;
 FTrialCount := 0;
 SeriesHistory.Clear;
end;


procedure TFmPrimitivePictureEvolution.AddCostLogString(Value: string);
begin
 Value := 'Circle ' + IntToStr(Length(FCircles)) + ': ' + Value;
 FCostLog.Add(Value);
 if FmCostLog.Visible
  then FmCostLog.LogCost.Lines.Add(Value);
end;

procedure TFmPrimitivePictureEvolution.Reinitialization;
begin
 // store cost to cost log
 AddCostLogString('Reinitialization');
 FTrialCount := 0;
 FReInitializeIndex := 0;
 FDiffEvol.Initialize;
end;

procedure TFmPrimitivePictureEvolution.Evolve;
var
  BestCosts      : Double;
  BestPopulation : TDifferentialEvolutionPopulation;
  Index          : Integer;
label
  DiffPop;
begin
 if (FReInitializeIndex > 0) and (FTrialCount >= 0) and
   (FLastBestCosts = FMaximumCost) then
  begin
   // get best population and draw best to new drawing
   BestPopulation := FDiffEvol.GetBestPopulation;

   if (FTrialCount >= FTrialsPerCircle) then
    begin
     Reinitialization;
     goto DiffPop;
    end;

   for Index := 0 to FDiffEvol.PopulationCount - 1 do
    if not CompareMem(@BestPopulation[0], @FDiffEvol.Population[Index][0],
      FDiffEvol.VariableCount * SizeOf(Double)) then goto DiffPop;

   Reinitialization;
   DiffPop:
  end;

 if AcBack.Checked and AcNext.Checked then
  begin
   AcNext.Checked := False;
   AcBack.Checked := False;
  end else
 if AcNext.Checked or (FTrialCount >= FTrialsPerCircle) or
  ((FTrialsSinceUpdate >= (0.25 * FTrialsPerCircle)) and
  (FLastBestCosts - FMaximumCost < 0) and FAutoNextTrial) then
  begin
   // store cost to cost log
   if FReInitializeIndex > 0
    then AddCostLogString(FloatToStr(FDiffEvol.GetBestCost - FMaximumCost) + ' (' +
      IntToStr(FTrialCount) + ' Trials) *')
    else AddCostLogString(FloatToStr(FDiffEvol.GetBestCost - FMaximumCost) + ' (' +
      IntToStr(FTrialCount) + ' Trials)');

   // get best population and draw best to new drawing
   BestPopulation := FDiffEvol.GetBestPopulation;
   DrawPopulation(BestPopulation, FDrawing);
   if MiCostMap.Checked or FReduceHighCosts
    then CalculateCostMap;

   SetLength(FCircles, Length(FCircles) + FNumberOfCircles);

   if FRandomOrder then
    begin
     Assert(FNumberOfCircles = 1);
     Move(FCircles[FCurrentOrder], FCircles[FCurrentOrder + 1],
       (Length(FCircles) - (FCurrentOrder + 1)) * SizeOf(TGuiPixelFilledCircle));
     FCircles[FCurrentOrder] := TGuiPixelFilledCircle.Create;
     AssignCircleData(FCircles[FCurrentOrder], @BestPopulation[0]);
     FCurrentOrder := Random(Length(FCircles) + 1);
    end
   else
    for Index := 0 to FNumberOfCircles - 1 do
     begin
      FCircles[FCurrentCircle + Index] := TGuiPixelFilledCircle.Create;
      AssignCircleData(FCircles[FCurrentCircle + Index], @BestPopulation[7 * Index]);
     end;

   // save backup
   SaveDrawingBackup;

   // update status bar
   StatusBar.Panels[1].Text := 'Circles: ' + IntToStr(FCurrentCircle + FNumberOfCircles);

   NextTrial;
   CalculateStaticCosts;
   AcNext.Checked := False;
  end else
 if AcBack.Checked then
  begin
   if (FCurrentCircle > 0) then
    begin
     Dec(FCurrentCircle, FNumberOfCircles);
     for Index := FCurrentCircle to Length(FCircles) - 1
      do FreeAndNil(FCircles[Index]);
     SetLength(FCircles, FCurrentCircle + 1);

     FTrialCount := 0;
     FDiffEvol.Initialize;
     CalculateStaticCosts;
    end;
   AcBack.Checked := False;
  end;

 // store cost log
 if MiStoreLog.Checked
  then FCostLog.SaveToFile('Costs.log');

 Randomize;
 Inc(FTrialCount);

 if FWeightDither then
  begin
   DifferentialEvolution.GainR2 := 0.5 * (1 + Random);
   DifferentialEvolution.GainR1 := - DifferentialEvolution.GainR2;
  end;

 DifferentialEvolution.Evolve;

 BestCosts := DifferentialEvolution.GetBestCost;
 if BestCosts < FLastBestCosts then
  begin
   FTrialsSinceUpdate := 0;
   SeriesHistory.AddXY(FTrialCount, BestCosts - FMaximumCost);
  end
 else Inc(FTrialsSinceUpdate);

 FLastBestCosts := BestCosts;
end;

procedure TFmPrimitivePictureEvolution.LoadReference(FileName: TFileName);
begin
 if not FileExists(FileName) then Exit;

 FReference.Clear(FBackgroundColor);
 FReference.LoadFromFile(FileName);
 with FDrawing do
  begin
   SetSize(FReference.Width, FReference.Height);
   Clear(FBackgroundColor);
  end;

 with FNewDrawing do
  begin
   SetSize(FReference.Width, FReference.Height);
   Clear(FBackgroundColor);
  end;

 with FBestDrawing do
  begin
   SetSize(FReference.Width, FReference.Height);
   Clear(FBackgroundColor);
  end;

 with FCostMap do
  begin
   SetSize(4 * ((FReference.Width + 3) div 4), FReference.Height);
   Clear;
  end;

 FMaximumDimension := Max(FReference.Width, FReference.Height);
 FArea := (FReference.Width * FReference.Height);
 FCostScale := 1 / FArea;

 PaintBoxRef.Width := FReference.Width;
 PaintBoxRef.Height := FReference.Height;
 PaintBoxDraw.Width := FReference.Width;
 PaintBoxDraw.Height := FReference.Height;
 PaintBoxDraw.Left := PaintBoxRef.Left + PaintBoxRef.Width + 8;
 ClientWidth := 2 * FReference.Width + 24;
 ClientHeight := FReference.Height + StatusBar.Height + 16 + 28;

 PaintBoxRef.Invalidate;
 PaintBoxDraw.Invalidate;
 StatusBar.Panels[0].Text := 'Ready';
end;

procedure TFmPrimitivePictureEvolution.LoadBest(FileName: TFileName);
var
  Circles : TCircleChunkContainer;
  Index   : Integer;
  Pixel   : TPixel32;
  H, S, L : Single;
begin
 try
  Circles := TCircleChunkContainer.Create;
  Circles.LoadFromFile(FileName);

  for Index := 0 to Min(FNumberOfCircles, Circles.Count) - 1 do
   with DifferentialEvolution do
    begin
     BestPopulation[7 * Index    ] := ConvertFromFixed24Dot8(Circles[Index].CenterX);
     BestPopulation[7 * Index + 1] := ConvertFromFixed24Dot8(Circles[Index].CenterY);
     BestPopulation[7 * Index + 2] := ConvertFromFixed24Dot8(Circles[Index].Radius);
     Pixel.R := Circles[Index].Color and $FF;
     Pixel.G := (Circles[Index].Color shr 8) and $FF;
     Pixel.B := (Circles[Index].Color shr 16) and $FF;

     PixelToHLS(Pixel, H, S, L);
     BestPopulation[7 * Index + 3] := H;
     BestPopulation[7 * Index + 4] := S;
     BestPopulation[7 * Index + 5] := L;
     BestPopulation[7 * Index + 6] := Circles[Index].Alpha * COne255th;
    end;
 finally
  if Assigned(Circles) then FreeAndNil(Circles);
 end;
end;

procedure TFmPrimitivePictureEvolution.LoadDrawing(FileName: TFileName);
var
  Circles : TCircleChunkContainer;
  Index   : Integer;
begin
 if not FileExists(FileName) then Exit;

 try
  Circles := TCircleChunkContainer.Create;
  Circles.LoadFromFile(FileName);

  FDrawing.Clear(FBackgroundColor);
  FCurrentCircle := Circles.Count;

  for Index := 0 to Length(FCircles) - 1
   do FreeAndNil(FCircles[Index]);

  SetLength(FCircles, Circles.Count);
  for Index := 0 to Circles.Count - 1 do
   begin
    // eventually create
    if not Assigned(FCircles[Index])
     then FCircles[Index] := TGuiPixelFilledCircle.Create;

    Assert(Circles[Index] <> nil);

    with FCircles[Index] do
     begin
      Alpha := Circles[Index].Alpha;
      Color := Circles[Index].Color;
      GeometricShape.Radius := Circles[Index].Radius;
      GeometricShape.CenterX := Circles[Index].CenterX;
      GeometricShape.CenterY := Circles[Index].CenterY;
      Draw(FDrawing);
     end;
   end;

  if MiCostMap.Checked or FReduceHighCosts
   then CalculateCostMap;

  // eventually create current circle
  if Assigned(FEvolution) then
   begin
    FEvolution.Terminate;
    if FEvolution.Suspended
     then FEvolution.Suspended := False;
    FEvolution.WaitFor;
    FreeAndNil(FEvolution);
   end;

  FTrialCount := 0;
  UpdateOptimizerSettings;
  FDiffEvol.Initialize;
  CalculateStaticCosts;

  FEvolution := TEvolutionThread.Create(True);
  FEvolution.Priority := tpLower;
  FEvolution.Start;
  FEvolution.Suspended := True;
  MiStopContinue.Enabled := True;
  MiSaveDrawing.Enabled := True;
  MiStopContinue.Tag := 1;
  MiStopContinue.Caption := 'C&ontinue';

  FBestDrawing.Assign(FDrawing);
  PaintBoxDraw.Invalidate;
 finally
  if Assigned(Circles) then FreeAndNil(Circles);
 end;
end;

procedure TFmPrimitivePictureEvolution.SaveAnimationStatic(FileName: TFileName;
  ScaleFactor: Single);
var
  Drawing  : TGuiPixelMapMemory;
  Circle   : TGuiPixelFilledCircle;
  OffsetX  : TFixed24Dot8;
  OffsetY  : TFixed24Dot8;
  Index    : Integer;
  IntScale : Integer;
begin
 with TFmProgressBar.Create(Self) do
  try
   Show;
   Drawing := TGuiPixelMapMemory.Create;
   Drawing.Width := Round(2 * ScaleFactor * FBestDrawing.Width);
   Drawing.Height := Round(2 * ScaleFactor * FBestDrawing.Height);
   Drawing.Clear(FBackgroundColor);
   Drawing.MakeOpaque;
   {$IFNDEF OnlyFinalBlend}
   Drawing.SaveToFile(FileName + '\Frame0.png');
   {$ENDIF}

   Circle := TGuiPixelFilledCircle.Create;

   IntScale := Round(ScaleFactor * (1 shl 8));
   OffsetX := ConvertToFixed24Dot8(Drawing.Width div 4);
   OffsetY := ConvertToFixed24Dot8(Drawing.Height div 4);

   ProgressBar.Max := Length(FCircles) + 16;
   for Index := 0 to Length(FCircles) - 1 do
    if Assigned(FCircles[Index]) then
     with FCircles[Index] do
      begin
       Circle.GeometricShape.Radius := FixedMul(GeometricShape.Radius, IntScale);
       Circle.GeometricShape.CenterX := FixedAdd(FixedMul(GeometricShape.CenterX, IntScale), OffsetX);
       Circle.GeometricShape.CenterY := FixedAdd(FixedMul(GeometricShape.CenterY, IntScale), OffsetY);
       Circle.Color := Color;
       Circle.Alpha := Alpha;
       Circle.Draw(Drawing);

       // finalize and save drawing
       Drawing.MakeOpaque;
       Drawing.SaveToFile(FileName + '\Frame' + IntToStr(Index + 1) + '.png');

       ProgressBar.StepIt;
       Application.ProcessMessages;
      end;

(*
   // blend over reference
   for Index := 0 to 15 do
    begin
     Drawing.Draw(FReference, Drawing.Width div 4, Drawing.Height div 4, $8);
     Drawing.MakeOpaque;
     Drawing.SaveToFile(FileName + '\Frame' + IntToStr(Length(FCircles) + Index + 1) + '.png');
     ProgressBar.StepIt;
     Application.ProcessMessages;
    end;
*)
  finally
   if Assigned(Circle) then FreeAndNil(Circle);
   if Assigned(Drawing) then FreeAndNil(Drawing);
   Free;
  end;
end;

procedure TFmPrimitivePictureEvolution.SaveAnimation(FileName: TFileName;
  ScaleFactor: Single; Halflife: Integer = 0);
var
  Drawing       : TGuiPixelMapMemory;
  BackDraw      : TGuiPixelMapMemory;
  Circle        : TGuiPixelFilledCircle;
  OffsetX       : TFixed24Dot8;
  OffsetY       : TFixed24Dot8;
  FrameIndex    : Integer;
  Index         : Integer;
  IntScale      : Integer;
  FinalRadius   : TFixed24Dot8;
  FixedOneThird : TFixed24Dot8;
  TempRect      : TRect;
  FinalAlpha    : Byte;
  HalfLifePos   : Single;
  HalfLifeIndex : Integer;
const
  pxShadeBlack32 : TPixel32 = (ARGB : $10000000);
  pxShadeWhite32 : TPixel32 = (ARGB : $0FFFFFFF);
begin
 with TFmProgressBar.Create(Self) do
  try
   Show;
   Drawing := TGuiPixelMapMemory.Create;
   Drawing.Width := Round(2 * ScaleFactor * FBestDrawing.Width);
   Drawing.Height := Round(2 * ScaleFactor * FBestDrawing.Height);
   Drawing.Clear(FBackgroundColor);
   Drawing.MakeOpaque;
   {$IFNDEF OnlyFinalBlend}
   Drawing.SaveToFile(FileName + '\Frame0.png');
   {$ENDIF}

   Circle := TGuiPixelFilledCircle.Create;

   IntScale := Round(ScaleFactor * (1 shl 8));
   OffsetX := ConvertToFixed24Dot8(Drawing.Width div 4);
   OffsetY := ConvertToFixed24Dot8(Drawing.Height div 4);

   // initialize
   FixedOneThird := ConvertToFixed24Dot8(0.3333);
   FrameIndex := 1;
   ProgressBar.Max := 2 * Length(FCircles) + 41;
   HalfLifePos := 0;
   HalfLifeIndex := 0;

   // create temporary drawing
   BackDraw := TGuiPixelMapMemory.Create;
   try
    for Index := 0 to Length(FCircles) - 1 do
     if Assigned(FCircles[Index]) then
      with FCircles[Index] do
       begin
        // general steps
        BackDraw.Assign(Drawing);
        Circle.GeometricShape.CenterX := FixedAdd(FixedMul(GeometricShape.CenterX, IntScale), OffsetX);
        Circle.GeometricShape.CenterY := FixedAdd(FixedMul(GeometricShape.CenterY, IntScale), OffsetY);
        Circle.Color := Color;

        // define final values
        FinalRadius := FixedMul(GeometricShape.Radius, IntScale);
        FinalAlpha := Alpha;

        // set start values
        Circle.GeometricShape.Radius := CFixed24Dot8One;
        Circle.Alpha := 1;

        ProgressBar.StepIt;
        Application.ProcessMessages;

        {$IFNDEF OnlyFinalBlend}
        while Circle.GeometricShape.Radius.Fixed < FinalRadius.Fixed do
         begin
          // calculate alpha
          Circle.Alpha := Round(FinalAlpha * (Circle.GeometricShape.Radius.Fixed / FinalRadius.Fixed)) and $FF;

          if (Halflife = 0) or (HalfLifePos <= 1) then
           begin
            // render circle
            Circle.Draw(Drawing);

            // finalize and save drawing
            if not Drawing.Equals(BackDraw) then
             begin
              Drawing.MakeOpaque;
              Drawing.SaveToFile(FileName + '\Frame' + IntToStr(FrameIndex) + '.png');

              // advance frame index
              Inc(FrameIndex);

              // reset drawing
              Drawing.Assign(BackDraw);
             end;
           end;

          Inc(HalfLifeIndex);
          if Halflife > 0 then
           begin
            if HalfLifePos > 1 then HalfLifePos := HalfLifePos - 1;
            HalfLifePos := HalfLifePos + 1 - (Halflife / (Halflife + HalfLifeIndex));
           end;

          // advance radius
          with Circle.GeometricShape do
           begin
            Radius := FixedAdd(Radius, FixedMul(FixedSub(FinalRadius, Radius), FixedOneThird));
            Radius := FixedMul(Radius, ConvertToFixed24Dot8(1.1));
            Radius := FixedAdd(Radius, CFixed24Dot8One);
           end;

          Application.ProcessMessages;
         end;
       {$ENDIF}

        // draw desired circle
        Circle.GeometricShape.Radius := FinalRadius;
        Circle.Alpha := FinalAlpha;
        Circle.Draw(Drawing);

        if (Halflife = 0) or (HalfLifePos <= 1) then
         begin
          {$IFNDEF OnlyFinalBlend}
          // finalize and save drawing
          Drawing.MakeOpaque;
          Drawing.SaveToFile(FileName + '\Frame' + IntToStr(FrameIndex) + '.png');
          {$ENDIF}

          Inc(FrameIndex);
         end;

        Inc(HalfLifeIndex);
        if Halflife > 0 then
         begin
          if HalfLifePos > 1 then HalfLifePos := HalfLifePos - 1;
          HalfLifePos := HalfLifePos + 1 - (Halflife / (Halflife + HalfLifeIndex));
         end;

        ProgressBar.StepIt;
        Application.ProcessMessages;
       end;

    // blend over reference
    for Index := 0 to 40 do
     begin
      TempRect := Rect(0, 0, (Drawing.Width div 4) - 1, Drawing.Height);
      Drawing.FillRect(TempRect, pxShadeBlack32);

      TempRect := Rect(Drawing.Width - (Drawing.Width div 4) + 1, 0,
        Drawing.Width, Drawing.Height);
      Drawing.FillRect(TempRect, pxShadeBlack32);

      TempRect := Rect((Drawing.Width div 4) - 1, 0,
        Drawing.Width - (Drawing.Width div 4) + 1,
        (Drawing.Height div 4) - 1);
      Drawing.FillRect(TempRect, pxShadeBlack32);

      TempRect := Rect((Drawing.Width div 4) - 1,
        Drawing.Height - (Drawing.Height div 4) + 1,
        Drawing.Width - (Drawing.Width div 4) + 1, Drawing.Height);
      Drawing.FillRect(TempRect, pxShadeBlack32);

      TempRect := Rect((Drawing.Width div 4), (Drawing.Height div 4),
        (Drawing.Width - Drawing.Width div 4),
        (Drawing.Height - Drawing.Height div 4));
      Drawing.FrameRect(TempRect, pxShadeWhite32);

      Dec(TempRect.Left);
      Dec(TempRect.Top);
      Inc(TempRect.Right);
      Inc(TempRect.Bottom);
      Drawing.FrameRect(TempRect, pxShadeWhite32);
      Drawing.FrameRect(TempRect, pxShadeWhite32);

      Dec(TempRect.Left);
      Dec(TempRect.Top);
      Inc(TempRect.Right);
      Inc(TempRect.Bottom);
      Drawing.FrameRect(TempRect, pxShadeWhite32);

      Drawing.MakeOpaque;
      Drawing.SaveToFile(FileName + '\Frame' + IntToStr(FrameIndex) + '.png');
      Inc(FrameIndex);

      ProgressBar.StepIt;
      Application.ProcessMessages;
     end;
   finally
    FreeAndNil(BackDraw);
   end;
  finally
   if Assigned(Circle) then FreeAndNil(Circle);
   if Assigned(Drawing) then FreeAndNil(Drawing);
   Free;
  end;
end;

procedure TFmPrimitivePictureEvolution.SaveAnimationAdvanced(FileName: TFileName;
  ScaleFactor: Single);
var
  Drawing       : TGuiPixelMapMemory;
  BackDraw      : TGuiPixelMapMemory;
  Circle        : TGuiPixelFilledCircle;
  FinalCircle   : TGuiPixelFilledCircle;
  Offset        : TFixed24Dot8Point;
  FrameIndex    : Integer;
  Index         : Integer;
  SlotIndex     : Integer;
  SlotList      : TList;
  IntScale      : Integer;
  Ratio         : Single;
  FTotalRadius  : Single;
  FinalRadius   : TFixed24Dot8;
  FixedScale    : array [0..1] of TFixed24Dot8;
  TempRect      : TRect;
const
  pxShadeBlack32 : TPixel32 = (ARGB : $10000000);
  pxShadeWhite32 : TPixel32 = (ARGB : $0FFFFFFF);
begin
 // initialize
 FixedScale[0] := ConvertToFixed24Dot8(0.2);
 FixedScale[1] := ConvertToFixed24Dot8(1.1);
 FrameIndex := 1;
 FTotalRadius  := 0;

 with TFmProgressBar.Create(Self) do
  try
   Show;
   Drawing := TGuiPixelMapMemory.Create;
   Drawing.Width := Round(2 * ScaleFactor * FBestDrawing.Width);
   Drawing.Height := Round(2 * ScaleFactor * FBestDrawing.Height);
   Drawing.Clear(FBackgroundColor);
   Drawing.MakeOpaque;
   {$IFNDEF OnlyFinalBlend}
   Drawing.SaveToFile(FileName + '\Frame0.png');
   {$ENDIF}

   ProgressBar.Max := Length(FCircles) + 41;
   IntScale := Round(ScaleFactor * (1 shl 8));
   Offset.X := ConvertToFixed24Dot8(Drawing.Width div 4);
   Offset.Y := ConvertToFixed24Dot8(Drawing.Height div 4);

   // create temporary drawing
   BackDraw := TGuiPixelMapMemory.Create;
   BackDraw.Assign(Drawing);

   // create slotlist
   SlotList := TList.Create;

   Index := 0;
   while Index < Length(FCircles) do
   begin
    // reset drawing
    Drawing.Assign(BackDraw);

    FTotalRadius := 0.7 * FTotalRadius;

    // eventually create new circle
    if (SlotList.Count < 32) and ((SlotList.Count = 0) or (FTotalRadius < 64))
      and (Index + SlotList.Count < Length(FCircles)) then
     begin
      Circle := TGuiPixelFilledCircle.Create;
      with FCircles[Index + SlotList.Count] do
       begin
        Circle.GeometricShape.CenterX := FixedAdd(FixedMul(GeometricShape.CenterX, IntScale), Offset.X);
        Circle.GeometricShape.CenterY := FixedAdd(FixedMul(GeometricShape.CenterY, IntScale), Offset.Y);
        Circle.GeometricShape.Radius := CFixed24Dot8One;
        Circle.Color := Color;
        Circle.Alpha := $FF;
        SlotList.Add(Circle);
        FTotalRadius := FTotalRadius + ConvertFromFixed24Dot8(GeometricShape.Radius);
       end;
      ProgressBar.StepIt;
      Application.ProcessMessages;
     end;

    // iterate through all circles in the slot list
    SlotIndex := 0;
    while SlotIndex < SlotList.Count do
     begin
      Circle := TGuiPixelFilledCircle(SlotList.Items[SlotIndex]);

      // calculate alpha
      FinalCircle := FCircles[Index + SlotIndex];
      FinalRadius := FixedMul(FinalCircle.GeometricShape.Radius, IntScale);
      Ratio := (Circle.GeometricShape.Radius.Fixed / FinalRadius.Fixed);
      Assert(Ratio <= 1);
      Assert(Ratio >= 0);
      Circle.Alpha := Round(FinalCircle.Alpha * Ratio) and $FF;

      Circle.Draw(Drawing);

      // check if circle still needs to be kept in list
      if (Circle.GeometricShape.Radius.Fixed = FinalRadius.Fixed) and (SlotIndex = 0) then
       begin
        BackDraw.Assign(Drawing);
        TGuiPixelFilledCircle(SlotList.Items[SlotIndex]).Free;
        SlotList.Delete(0);
        Inc(Index);
        Continue;
       end;

      if Circle.GeometricShape.Radius.Fixed < FinalRadius.Fixed then
       with Circle.GeometricShape do
        begin
         Radius := FixedAdd(Radius, FixedMul(FixedSub(FinalRadius, Radius), FixedScale[0]));
         Radius := FixedMul(Radius, FixedScale[1]);
         Radius := FixedAdd(Circle.GeometricShape.Radius, CFixed24Dot8One);
        end;

      if Circle.GeometricShape.Radius.Fixed > FinalRadius.Fixed
       then Circle.GeometricShape.Radius := FinalRadius;

      Inc(SlotIndex);
     end;

    {$IFNDEF OnlyFinalBlend}
    // finalize and save drawing
    Drawing.MakeOpaque;
    Drawing.SaveToFile(FileName + '\Frame' + IntToStr(FrameIndex) + '.png');
    {$ENDIF}
    Inc(FrameIndex);
   end;

   // blend over reference
   for Index := 0 to 40 do
    begin
     TempRect := Rect(0, 0, (Drawing.Width div 4) - 1, Drawing.Height);
     Drawing.FillRect(TempRect, pxShadeBlack32);

     TempRect := Rect(Drawing.Width - (Drawing.Width div 4) + 1, 0,
       Drawing.Width, Drawing.Height);
     Drawing.FillRect(TempRect, pxShadeBlack32);

     TempRect := Rect((Drawing.Width div 4) - 1, 0,
       Drawing.Width - (Drawing.Width div 4) + 1,
       (Drawing.Height div 4) - 1);
     Drawing.FillRect(TempRect, pxShadeBlack32);

     TempRect := Rect((Drawing.Width div 4) - 1,
       Drawing.Height - (Drawing.Height div 4) + 1,
       Drawing.Width - (Drawing.Width div 4) + 1, Drawing.Height);
     Drawing.FillRect(TempRect, pxShadeBlack32);

     TempRect := Rect((Drawing.Width div 4), (Drawing.Height div 4),
       (Drawing.Width - Drawing.Width div 4),
       (Drawing.Height - Drawing.Height div 4));
     Drawing.FrameRect(TempRect, pxShadeWhite32);

     Dec(TempRect.Left);
     Dec(TempRect.Top);
     Inc(TempRect.Right);
     Inc(TempRect.Bottom);
     Drawing.FrameRect(TempRect, pxShadeWhite32);
     Drawing.FrameRect(TempRect, pxShadeWhite32);

     Dec(TempRect.Left);
     Dec(TempRect.Top);
     Inc(TempRect.Right);
     Inc(TempRect.Bottom);
     Drawing.FrameRect(TempRect, pxShadeWhite32);

     Drawing.MakeOpaque;
     Drawing.SaveToFile(FileName + '\Frame' + IntToStr(FrameIndex) + '.png');
     Inc(FrameIndex);

     ProgressBar.StepIt;
     Application.ProcessMessages;
    end;
  finally
   if Assigned(BackDraw) then FreeAndNil(BackDraw);
   if Assigned(SlotList) then FreeAndNil(SlotList);
   if Assigned(Drawing) then FreeAndNil(Drawing);
   Free;
  end;
end;

procedure TFmPrimitivePictureEvolution.SaveAnimationBlow(FileName: TFileName;
  ScaleFactor: Single; Inverted: Boolean = False);
var
  Drawing       : TGuiPixelMapMemory;
  Offset        : TFixed24Dot8Point;
  FrameIndex    : Integer;
  Iterations    : Integer;
  Index         : Integer;
  IntScale      : Integer;
  Ratio         : Single;
  ScaledRatio   : TFixed24Dot8;
  MaxRadius     : TFixed24Dot8;
  TempRect      : TRect;
  Circles       : array of TGuiPixelFilledCircle;
const
  pxShadeBlack32 : TPixel32 = (ARGB : $10000000);
  pxShadeWhite32 : TPixel32 = (ARGB : $0FFFFFFF);
begin
 if Length(FCircles) = 0
  then Exit;

 with TFmProgressBar.Create(Self) do
  try
   Show;
   Drawing := TGuiPixelMapMemory.Create;
   Drawing.Width := Round(2 * ScaleFactor * FBestDrawing.Width);
   Drawing.Height := Round(2 * ScaleFactor * FBestDrawing.Height);
   Drawing.Clear(FBackgroundColor);
   Drawing.MakeOpaque;
   {$IFNDEF OnlyFinalBlend}
   Drawing.SaveToFile(FileName + '\Frame0.png');
   {$ENDIF}

   IntScale := Round(ScaleFactor * (1 shl 8));
   Offset.X := ConvertToFixed24Dot8(Drawing.Width div 4);
   Offset.Y := ConvertToFixed24Dot8(Drawing.Height div 4);

   // find maximum radius and create circles
   MaxRadius := FCircles[0].GeometricShape.Radius;
   SetLength(Circles, Length(FCircles));
   Circles[0] := TGuiPixelFilledCircle.Create;
   Circles[0].Assign(FCircles[0]);
   Circles[0].GeometricShape.CenterX := FixedAdd(FixedMul(Circles[0].GeometricShape.CenterX, IntScale), Offset.X);
   Circles[0].GeometricShape.CenterY := FixedAdd(FixedMul(Circles[0].GeometricShape.CenterY, IntScale), Offset.Y);
   for Index := 1 to Length(FCircles) - 1 do
    begin
     if FCircles[Index].GeometricShape.Radius > MaxRadius
      then MaxRadius := FCircles[Index].GeometricShape.Radius;
     Circles[Index] := TGuiPixelFilledCircle.Create;
     Circles[Index].Assign(FCircles[Index]);
     Circles[Index].GeometricShape.CenterX := FixedAdd(FixedMul(Circles[Index].GeometricShape.CenterX, IntScale), Offset.X);
     Circles[Index].GeometricShape.CenterY := FixedAdd(FixedMul(Circles[Index].GeometricShape.CenterY, IntScale), Offset.Y);
    end;
   Iterations := FixedFloor(0.25 * MaxRadius);
   ProgressBar.Max := Iterations + 43;

   FrameIndex := 1;
   while FrameIndex < Iterations do
    begin
     Drawing.Clear(FBackgroundColor);
     Drawing.MakeOpaque;

     Ratio := FrameIndex / (Iterations - 1);
     Assert(Ratio <= 1);
     Assert(Ratio >= 0);
     ScaledRatio := FixedMul(ConvertToFixed24Dot8(Ratio), IntScale);

     for Index := 0 to Length(Circles) - 1 do
      begin
       Circles[Index].GeometricShape.Radius := FixedMul(FCircles[Index].GeometricShape.Radius, ScaledRatio);
       Circles[Index].Alpha := Round(FCircles[Index].Alpha * Sqrt(Ratio)) and $FF;
       Circles[Index].Draw(Drawing);
      end;

     {$IFNDEF OnlyFinalBlend}
     // finalize and save drawing
     Drawing.MakeOpaque;
     if Inverted then
       Drawing.SaveToFile(FileName + '\Frame' + IntToStr(Iterations - FrameIndex) + '.png')
     else
       Drawing.SaveToFile(FileName + '\Frame' + IntToStr(FrameIndex) + '.png');
     {$ENDIF}

     Inc(FrameIndex);
     ProgressBar.StepIt;
     Application.ProcessMessages;
    end;

   for Index := 0 to Length(Circles) - 1 do
     FreeAndNil(Circles[Index]);

   // blend over reference
   if not Inverted then
    for Index := 0 to 40 do
     begin
      TempRect := Rect(0, 0, (Drawing.Width div 4) - 1, Drawing.Height);
      Drawing.FillRect(TempRect, pxShadeBlack32);

      TempRect := Rect(Drawing.Width - (Drawing.Width div 4) + 1, 0,
        Drawing.Width, Drawing.Height);
      Drawing.FillRect(TempRect, pxShadeBlack32);

      TempRect := Rect((Drawing.Width div 4) - 1, 0,
        Drawing.Width - (Drawing.Width div 4) + 1,
        (Drawing.Height div 4) - 1);
      Drawing.FillRect(TempRect, pxShadeBlack32);

      TempRect := Rect((Drawing.Width div 4) - 1,
        Drawing.Height - (Drawing.Height div 4) + 1,
        Drawing.Width - (Drawing.Width div 4) + 1, Drawing.Height);
      Drawing.FillRect(TempRect, pxShadeBlack32);

      TempRect := Rect((Drawing.Width div 4), (Drawing.Height div 4),
        (Drawing.Width - Drawing.Width div 4),
        (Drawing.Height - Drawing.Height div 4));
      Drawing.FrameRect(TempRect, pxShadeWhite32);

      Dec(TempRect.Left);
      Dec(TempRect.Top);
      Inc(TempRect.Right);
      Inc(TempRect.Bottom);
      Drawing.FrameRect(TempRect, pxShadeWhite32);
      Drawing.FrameRect(TempRect, pxShadeWhite32);

      Dec(TempRect.Left);
      Dec(TempRect.Top);
      Inc(TempRect.Right);
      Inc(TempRect.Bottom);
      Drawing.FrameRect(TempRect, pxShadeWhite32);

      Drawing.MakeOpaque;
      Drawing.SaveToFile(FileName + '\Frame' + IntToStr(FrameIndex) + '.png');
      Inc(FrameIndex);

      ProgressBar.StepIt;
      Application.ProcessMessages;
     end;
   ProgressBar.Position := ProgressBar.Max;
   Application.ProcessMessages;
  finally
   if Assigned(Drawing) then FreeAndNil(Drawing);
   Free;
  end;
end;

procedure TFmPrimitivePictureEvolution.SaveDrawing(FileName: TFileName);
var
  Circles : TCircleChunkContainer;
  Circle  : TCircleChunk;
  Index   : Integer;
begin
 try
  Circles := TCircleChunkContainer.Create;

  for Index := 0 to Length(FCircles) - 1 do
   if Assigned(FCircles[Index]) then
    begin
     Circle := TCircleChunk.Create;
     Circle.Alpha := FCircles[Index].Alpha;
     Circle.Color := FCircles[Index].Color;
     Circle.Radius := FCircles[Index].GeometricShape.Radius;
     Circle.CenterX := FCircles[Index].GeometricShape.CenterX;
     Circle.CenterY := FCircles[Index].GeometricShape.CenterY;
     Circles.AddChunk(Circle);
    end;

  Circles.SaveToFile(FileName);
 finally
  if Assigned(Circles) then FreeAndNil(Circles);
 end;
end;

procedure TFmPrimitivePictureEvolution.SaveDrawingHR(FileName: TFileName);
var
  DrawingHR : TGuiPixelMapMemory;
  Circle    : TGuiPixelFilledCircle;
  Index     : Integer;
const
  CScaleFactor = 4;
begin
 try
  DrawingHR := TGuiPixelMapMemory.Create;
  DrawingHR.Width := CScaleFactor * FBestDrawing.Width;
  DrawingHR.Height := CScaleFactor * FBestDrawing.Height;
  DrawingHR.Clear(FBackgroundColor);

  Circle := TGuiPixelFilledCircle.Create;
  try
   for Index := 0 to Length(FCircles) - 1 do
    if Assigned(FCircles[Index]) then
     begin
      Circle.Assign(FCircles[Index]);
      with Circle do
       try
        GeometricShape.Radius := FixedMul(GeometricShape.Radius, CScaleFactor shl 8);
        GeometricShape.CenterX := FixedMul(GeometricShape.CenterX, CScaleFactor shl 8);
        GeometricShape.CenterY := FixedMul(GeometricShape.CenterY, CScaleFactor shl 8);
        Draw(DrawingHR);
       except
        raise Exception.CreateFmt('Error drawing circle %d', [Index + 1]);
       end;
     end;
  finally
   FreeAndNil(Circle);
  end;

  // make opaque
  DrawingHR.MakeOpaque;
  DrawingHR.SaveToFile(FileName);
 finally
  if Assigned(DrawingHR) then FreeAndNil(DrawingHR);
 end;
end;

procedure TFmPrimitivePictureEvolution.SaveDrawingFramed(FileName: TFileName);
var
  Drawing  : TGuiPixelMapMemory;
  Circle   : TGuiPixelFilledCircle;
  Index    : Integer;
  TempRect : TRect;
begin
 try
  Drawing := TGuiPixelMapMemory.Create;
  Drawing.Width := 3 * FBestDrawing.Width;
  Drawing.Height := 3 * FBestDrawing.Height;
  Drawing.Clear(FBackgroundColor);

  Circle := TGuiPixelFilledCircle.Create;
  try
   for Index := 0 to Length(FCircles) - 1 do
    if Assigned(FCircles[Index]) then
     begin
      Circle.Assign(FCircles[Index]);
      with Circle do
       begin
        GeometricShape.Radius := FixedMul(GeometricShape.Radius, 2 shl 8);
        GeometricShape.CenterX := FixedAdd(ConvertToFixed24Dot8(Drawing.Width div 6), FixedMul(GeometricShape.CenterX, 2 shl 8));
        GeometricShape.CenterY := FixedAdd(ConvertToFixed24Dot8(Drawing.Height div 6), FixedMul(GeometricShape.CenterY, 2 shl 8));
        Draw(Drawing);
       end;
     end;
  finally
   FreeAndNil(Circle);
  end;

  TempRect := Rect(0, 0, (Drawing.Width div 6) - 1, Drawing.Height);
  Drawing.FillRect(TempRect, pxSemiBlack32);

  TempRect := Rect(Drawing.Width - (Drawing.Width div 6) + 1, 0, Drawing.Width, Drawing.Height);
  Drawing.FillRect(TempRect, pxSemiBlack32);

  TempRect := Rect((Drawing.Width div 6) - 1, 0,
    Drawing.Width - (Drawing.Width div 6) + 1, (Drawing.Height div 6) - 1);
  Drawing.FillRect(TempRect, pxSemiBlack32);

  TempRect := Rect((Drawing.Width div 6) - 1,
    Drawing.Height - (Drawing.Height div 6) + 1,
    Drawing.Width - (Drawing.Width div 6) + 1, Drawing.Height);
  Drawing.FillRect(TempRect, pxSemiBlack32);

  TempRect := Rect((Drawing.Width div 6), (Drawing.Height div 6),
    (Drawing.Width - Drawing.Width div 6),
    (Drawing.Height - Drawing.Height div 6));
  Drawing.FrameRect(TempRect, pxSemiWhite32);

  Dec(TempRect.Left);
  Dec(TempRect.Top);
  Inc(TempRect.Right);
  Inc(TempRect.Bottom);
  Drawing.FrameRect(TempRect, pxWhite32);

  Dec(TempRect.Left);
  Dec(TempRect.Top);
  Inc(TempRect.Right);
  Inc(TempRect.Bottom);
  Drawing.FrameRect(TempRect, pxSemiWhite32);

  // make opaque
  Drawing.MakeOpaque;
  Drawing.SaveToFile(FileName);
 finally
  if Assigned(Drawing) then FreeAndNil(Drawing);
 end;
end;

procedure TFmPrimitivePictureEvolution.PaintBoxDrawPaint(Sender: TObject);
begin
 if MiCurrentBestCost.Checked
  then FBestDrawing.PaintTo(PaintBoxDraw.Canvas);
 if MiPreviousBest.Checked
  then FDrawing.PaintTo(PaintBoxDraw.Canvas);
 if MiCostMap.Checked
  then FCostMap.PaintTo(PaintBoxDraw.Canvas);
end;

procedure TFmPrimitivePictureEvolution.PaintBoxRefPaint(Sender: TObject);
begin
 FReference.PaintTo(PaintBoxRef.Canvas);
end;

procedure BindFunctions;
begin
 // create function binding for error weighting function
 BindingErrorWeighting := TFunctionBinding.Create(@@ErrorWeighting, @ErrorWeightingNative);
 with BindingErrorWeighting do
  begin
   Add(@ErrorWeightingNative);
   {$IFNDEF PUREPASCAL}
   Add(@ErrorWeightingMMX, [pfSSE2]);
   Add(@ErrorWeightingSSE, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for error weighting function
 BindingErrorWeightingLoop := TFunctionBinding.Create(@@ErrorWeightingLoop, @ErrorWeightingLoopNative);
 with BindingErrorWeightingLoop do
  begin
   Add(@ErrorWeightingLoopNative);
   {$IFNDEF PUREPASCAL}
   Add(@ErrorWeightingLoopMMX, [pfMMX]);
   Add(@ErrorWeightingLoopSSE, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;
end;

procedure UnbindFunctions;
begin
 FreeAndNil(BindingErrorWeighting);
 FreeAndNil(BindingErrorWeightingLoop);
end;

initialization
  BindFunctions;

finalization
  UnbindFunctions;

end.
