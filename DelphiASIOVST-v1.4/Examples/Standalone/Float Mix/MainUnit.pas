unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DAV_AudioData, DAV_AudioFile, DAV_AudioFileWAV,
  DAV_AudioFileAIFF, DAV_GuiImageControl, DAV_GuiPngList, DAV_GuiCustomControl,
  DAV_GuiFader;

type
  EFloatMix = class(Exception);
  TFloatType =(ft8bit, ft16bit, ft32bit, ft64bit, ft80bit);

  TFileProperties = class(TObject)
  private
    FLevel : Double;
    FGain  : Double;
    procedure SetLevel(const Value: Double);
  protected
    procedure LevelChanged; virtual;
  public
    constructor Create; virtual;

    property Level: Double read FLevel write SetLevel;
    property Gain: Double read FGain;
  end;

  TFmFloatMix = class(TForm)
    BtAdd: TButton;
    BtMix: TButton;
    BtOutputfile: TButton;
    EdLevel: TEdit;
    EdOutputFile: TEdit;
    FdLevel: TGuiFader;
    GuiPNGList: TGuiPNGList;
    LbAccuracy: TLabel;
    LbInputFiles: TLabel;
    LbOutput: TLabel;
    ListInputFiles: TListBox;
    OpenDialog: TOpenDialog;
    Rb16Bit: TRadioButton;
    Rb32Bit: TRadioButton;
    Rb64Bit: TRadioButton;
    Rb80Bit: TRadioButton;
    Rb8Bit: TRadioButton;
    SaveDialog: TSaveDialog;
    procedure BtAddClick(Sender: TObject);
    procedure BtMixClick(Sender: TObject);
    procedure BtOutputfileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListInputFilesClick(Sender: TObject);
    procedure ListInputFilesEnter(Sender: TObject);
    procedure ListInputFilesExit(Sender: TObject);
    procedure Rb16BitClick(Sender: TObject);
    procedure Rb32BitClick(Sender: TObject);
    procedure Rb64BitClick(Sender: TObject);
    procedure Rb80BitClick(Sender: TObject);
    procedure Rb8BitClick(Sender: TObject);
    procedure FdLevelChange(Sender: TObject);
    procedure EdLevelEnter(Sender: TObject);
    procedure EdLevelExit(Sender: TObject);
    procedure EdLevelChange(Sender: TObject);
  private
    FFloatType : TFloatType;
    procedure CheckMixPossible;
    procedure CheckFileSelected;
  public
    { Public-Deklarationen }
  end;

var
  FmFloatMix: TFmFloatMix;

implementation

{$R *.dfm}

uses
  Math, DAV_Common, DAV_HalfFloat, DAV_MiniFloat;

{ TFileProperties }

constructor TFileProperties.Create;
begin
  FLevel := 0;
end;

procedure TFileProperties.LevelChanged;
begin
  FGain := dB_to_Amp(FLevel);
end;

procedure TFileProperties.SetLevel(const Value: Double);
begin
  if FLevel <> Value then
  begin
    FLevel := Value;
    LevelChanged;
  end;
end;


{ TFmFloatMix }

procedure TFmFloatMix.FdLevelChange(Sender: TObject);
begin
  if ListInputFiles.ItemIndex >= 0 then
    with ListInputFiles, TFileProperties(Items.Objects[ItemIndex]) do
    begin
      Level := -FdLevel.Value;
      EdLevel.Text := FloatToStrF(RoundTo(Level, -1), ffGeneral, 4, 4) + ' dB';
    end;
end;

procedure TFmFloatMix.FormCreate(Sender: TObject);
begin
  OpenDialog.Filter := GetSimpleFileFilter;
  SaveDialog.Filter := GetSimpleFileFilter;
//  ShowMessage(FloatToStr(Amp_To_dB(2.5)));
end;

procedure TFmFloatMix.BtAddClick(Sender: TObject);
var
  Index : Integer;
begin
  if OpenDialog.Execute then
  begin
    ListInputFiles.Items.AddStrings(OpenDialog.Files);
    for Index := 0 to ListInputFiles.Items.Count - 1 do
    begin
      if not Assigned(ListInputFiles.Items.Objects[Index]) then
        ListInputFiles.Items.Objects[Index] := TFileProperties.Create;
      TFileProperties(ListInputFiles.Items.Objects[Index]).Level := 0;
    end;
    CheckMixPossible;
  end;
end;

procedure TFmFloatMix.BtMixClick(Sender: TObject);
var
  InputFiles     : array of TAudioDataCollection32;
  CurrentInput   : Single;
  InputFileIndex : Integer;
  SampleIndex    : Integer;
  ChannelIndex   : Integer;
  ScaleFactor    : Extended;
  Value80        : Extended;
  Value64        : Double absolute Value80;
  Value32        : Single absolute Value80;
  Value16        : THalfFloat absolute Value80;
  Value8         : TMiniFloat absolute Value80;
begin
  try
    with TAudioDataCollection32.Create(nil) do
    try
      // load input files
      SetLength(InputFiles, ListInputFiles.Count);
      for InputFileIndex := 0 to Length(InputFiles) - 1 do
      begin
        InputFiles[InputFileIndex] := TAudioDataCollection32.Create(nil);
        InputFiles[InputFileIndex].LoadFromFile(ListInputFiles.Items[InputFileIndex]);
        if (InputFileIndex = 0) then
          SampleRate := InputFiles[0].SampleRate
        else
          if SampleRate <> InputFiles[InputFileIndex].SampleRate then
            raise EFloatMix.Create('Samplerate mismatch');

        if (InputFileIndex = 0) then
          ChannelCount := InputFiles[0].ChannelCount
        else
          if ChannelCount <> InputFiles[InputFileIndex].ChannelCount then
            raise EFloatMix.CreateFmt('Channel mismatch (%d)', [ChannelCount]);

        if InputFiles[InputFileIndex].SampleFrames > SampleFrames then
          SampleFrames := InputFiles[InputFileIndex].SampleFrames;
      end;

      for ChannelIndex := 0 to ChannelCount - 1 do
      begin
        for SampleIndex := 0 to SampleFrames - 1 do
        begin
          // set mix value to zero
          case FFloatType of
            ft8bit  : Value8  := SingleToMiniFloat(0);
            ft16bit : Value16 := SingleToHalfFloat(0);
            ft32bit : Value32 := 0;
            ft64bit : Value64 := 0;
            ft80bit : Value80 := 0;
          end;

          for InputFileIndex := 0 to Length(InputFiles) - 1 do
          begin
            with ListInputFiles.Items do
              ScaleFactor := TFileProperties(Objects[InputFileIndex]).Level;

            if SampleIndex < InputFiles[InputFileIndex].SampleFrames then
              begin
                CurrentInput := InputFiles[InputFileIndex].ChannelDataPointer[
                  ChannelIndex]^[SampleIndex];

                case FFloatType of
                  ft8bit : Value8 := SingleToMiniFloat(
                    MiniFloatToSingle(Value8) + CurrentInput);
                  ft16bit : Value16 := SingleToHalfFloat(
                    FastHalfFloatToSingle(Value16) + CurrentInput);
                  ft32bit : Value32 := Value32 + CurrentInput;
                  ft64bit : Value64 := Value64 + CurrentInput;
                  ft80bit : Value80 := Value80 + CurrentInput;
                end;
              end;
          end;

          case FFloatType of
            ft8bit : ChannelDataPointer[ChannelIndex]^[SampleIndex] :=
              MiniFloatToSingle(Value8);
            ft16bit : ChannelDataPointer[ChannelIndex]^[SampleIndex] :=
              FastHalfFloatToSingle(Value16);
            ft32bit : ChannelDataPointer[ChannelIndex]^[SampleIndex] := Value32;
            ft64bit : ChannelDataPointer[ChannelIndex]^[SampleIndex] := Value64;
            ft80bit : ChannelDataPointer[ChannelIndex]^[SampleIndex] := Value80;
          end;
        end;
      end;

      SaveToFile(EdOutputFile.Text);
    finally
      Free;
    end;
  except
    on E: EFloatMix do ShowMessage(E.Message);
  end;
end;

procedure TFmFloatMix.BtOutputfileClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    EdOutputFile.Text := SaveDialog.FileName;
    CheckMixPossible;
  end;
end;

procedure TFmFloatMix.CheckMixPossible;
begin
  BtMix.Enabled := (EdOutputFile.Text <> '') and (ListInputFiles.Count > 0) and
    (Rb8Bit.Checked or Rb16Bit.Checked or Rb32Bit.Checked or
    Rb64Bit.Checked or Rb80Bit.Checked);
end;

procedure TFmFloatMix.EdLevelChange(Sender: TObject);
var
  Text  : string;
  dBPos : Integer;
begin
  EdLevel.OnChange := nil;
  Text := EdLevel.Text;
  dBPos := Pos('dB', Text);
  if dBPos > 0 then
    Delete(Text, dBPos, 2);

  FdLevel.Value := StrToFloat(Trim(Text));
  EdLevel.OnChange := EdLevelChange;
end;

procedure TFmFloatMix.EdLevelEnter(Sender: TObject);
begin
  EdLevel.OnChange := EdLevelChange;
end;

procedure TFmFloatMix.EdLevelExit(Sender: TObject);
begin
  EdLevel.OnChange := nil;
end;

procedure TFmFloatMix.ListInputFilesClick(Sender: TObject);
begin
  CheckFileSelected;
end;

procedure TFmFloatMix.ListInputFilesEnter(Sender: TObject);
begin
  CheckFileSelected;
end;

procedure TFmFloatMix.ListInputFilesExit(Sender: TObject);
begin
  CheckFileSelected;
end;

procedure TFmFloatMix.CheckFileSelected;
begin
  FdLevel.Visible := (ListInputFiles.ItemIndex >= 0);
  EdLevel.Visible := FdLevel.Visible;
  if FdLevel.Visible then
    with ListInputFiles, TFileProperties(Items.Objects[ItemIndex]) do
    begin
      FdLevel.Value := Level;
      EdLevel.Text := FloatToStrF(RoundTo(Level, -1), ffGeneral, 4, 4) + ' dB';
    end;
end;

procedure TFmFloatMix.Rb8BitClick(Sender: TObject);
begin
  FFloatType := ft8bit;
  CheckMixPossible;
end;

procedure TFmFloatMix.Rb16BitClick(Sender: TObject);
begin
  FFloatType := ft16bit;
  CheckMixPossible;
end;

procedure TFmFloatMix.Rb32BitClick(Sender: TObject);
begin
  FFloatType := ft32bit;
  CheckMixPossible;
end;

procedure TFmFloatMix.Rb64BitClick(Sender: TObject);
begin
  FFloatType := ft64bit;
  CheckMixPossible;
end;

procedure TFmFloatMix.Rb80BitClick(Sender: TObject);
begin
  FFloatType := ft80bit;
  CheckMixPossible;
end;

end.
