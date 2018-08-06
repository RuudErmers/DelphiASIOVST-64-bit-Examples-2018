unit HEmain;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, ComCtrls, Spin, Grids, 
  Calendar, DAV_Types, DAV_DspHrtf,
  DAV_AudioData, DAV_GuiBaseControl, DAV_GuiAudioDataDisplay, DAV_AudioFileWAV,
  DAV_AudioFileAIFF;

type
  TFmHRTFEditor = class(TForm)
    ADHRIR: TAudioDataCollection32;
    AudioDataDisplayLeft: TGuiAudioDataDisplay;
    AudioDataDisplayRight: TGuiAudioDataDisplay;
    BtExport: TButton;
    BtImport: TButton;
    Calendar: TCalendar;
    EdADConverter: TEdit;
    EdAmplifier: TEdit;
    EdAuthor: TEdit;
    EdContext: TEdit;
    EdCopyright: TEdit;
    EdDAConverter: TEdit;
    EdDescription: TEdit;
    EdLoudspeaker: TEdit;
    EdManufacturer: TEdit;
    EdMeasurementType: TEdit;
    EdMicNotes: TEdit;
    EdMicType: TEdit;
    EdNameID: TEdit;
    EdNotes: TEdit;
    EdRoomType: TEdit;
    EdSignalType: TEdit;
    EdTitle: TEdit;
    LbADConverter: TLabel;
    LbAmplifier: TLabel;
    LbAuthor: TLabel;
    LbAzimuth: TLabel;
    LbAzimuthUnit: TLabel;
    LbContext: TLabel;
    LbCopyright: TLabel;
    LbDAConverter: TLabel;
    LbDate: TLabel;
    LbDescription: TLabel;
    LbDistance: TLabel;
    LbDistanceUnit: TLabel;
    LbFullDate: TLabel;
    LbHeight: TLabel;
    LbHeightUnit: TLabel;
    LbHrtfIndex: TLabel;
    LbLength: TLabel;
    LbLengthUnit: TLabel;
    LbLoudSpeaker: TLabel;
    LbManufacturer: TLabel;
    LbMicNotes: TLabel;
    LbMicrophoneDepth: TLabel;
    LbMicrophoneDepthUnit: TLabel;
    LbMicrophoneType: TLabel;
    LbMicType: TLabel;
    LbMonth: TLabel;
    LbMonthName: TLabel;
    LbNameID: TLabel;
    LbNotes: TLabel;
    LbPolar: TLabel;
    LbPolarUnit: TLabel;
    LbRoomType: TLabel;
    LbSex: TLabel;
    LbSignalType: TLabel;
    LbTitle: TLabel;
    LbWidth: TLabel;
    LbWidthUnit: TLabel;
    LbYear: TLabel;
    MainMenu: TMainMenu;
    MIAutoconvertOldHRTFfiles: TMenuItem;
    MIExit: TMenuItem;
    MIFile: TMenuItem;
    MIImportDiffuse: TMenuItem;
    MIImportETI: TMenuItem;
    MIImportIRCAM: TMenuItem;
    MINew: TMenuItem;
    MIOpen: TMenuItem;
    MISave: TMenuItem;
    MISaveAs: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    OpenDialog: TOpenDialog;
    OpenDialogWAV: TOpenDialog;
    PageControl: TPageControl;
    RbFemale: TRadioButton;
    RbGeneric: TRadioButton;
    RbMale: TRadioButton;
    RbUnknown: TRadioButton;
    SaveDialog: TSaveDialog;
    SaveDialogWAV: TSaveDialog;
    SEAzimuth: TSpinEdit;
    SEDistance: TSpinEdit;
    SEHeight: TSpinEdit;
    SEHrtfIndex: TSpinEdit;
    SELength: TSpinEdit;
    SEMicDepth: TSpinEdit;
    SEMonth: TSpinEdit;
    SEPolar: TSpinEdit;
    SEWidth: TSpinEdit;
    SEYear: TSpinEdit;
    TSGeneralInfo: TTabSheet;
    TSHrtfData: TTabSheet;
    TSMeasurementInfo: TTabSheet;
    TSMicrophoneInfo: TTabSheet;
    TSOutboardInfo: TTabSheet;
    TSRoomInfo: TTabSheet;
    TSSubjectInfo: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtExportClick(Sender: TObject);
    procedure BtImportClick(Sender: TObject);
    procedure CalendarChange(Sender: TObject);
    procedure EdADConverterChange(Sender: TObject);
    procedure EdAmplifierChange(Sender: TObject);
    procedure EdAuthorChange(Sender: TObject);
    procedure EdContextChange(Sender: TObject);
    procedure EdCopyrightChange(Sender: TObject);
    procedure EdDAConverterChange(Sender: TObject);
    procedure EdDescriptionChange(Sender: TObject);
    procedure EdLoudspeakerChange(Sender: TObject);
    procedure EdManufacturerChange(Sender: TObject);
    procedure EdMeasurementTypeChange(Sender: TObject);
    procedure EdMicNotesChange(Sender: TObject);
    procedure EdMicTypeChange(Sender: TObject);
    procedure EdNameIDChange(Sender: TObject);
    procedure EdNotesChange(Sender: TObject);
    procedure EdRoomTypeChange(Sender: TObject);
    procedure EdSignalTypeChange(Sender: TObject);
    procedure EdTitleChange(Sender: TObject);
    procedure MIAutoconvertOldHRTFfilesClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure MIImportDiffuseClick(Sender: TObject);
    procedure MIImportETIClick(Sender: TObject);
    procedure MIImportIRCAMClick(Sender: TObject);
    procedure MINewClick(Sender: TObject);
    procedure MIOpenClick(Sender: TObject);
    procedure MISaveAsClick(Sender: TObject);
    procedure MISaveClick(Sender: TObject);
    procedure RbFemaleClick(Sender: TObject);
    procedure RbGenericClick(Sender: TObject);
    procedure RbMaleClick(Sender: TObject);
    procedure RbUnknownClick(Sender: TObject);
    procedure SEDistanceChange(Sender: TObject);
    procedure SEHeightChange(Sender: TObject);
    procedure SEHrirPosChange(Sender: TObject);
    procedure SEHrtfIndexChange(Sender: TObject);
    procedure SELengthChange(Sender: TObject);
    procedure SEMonthChange(Sender: TObject);
    procedure SEWidthChange(Sender: TObject);
    procedure SEYearChange(Sender: TObject);
    procedure TSHrtfDataResize(Sender: TObject);
  private
    FFileName : TFileName;
    FHRTFFile : THrtfs;
    procedure UpdateFullDateCaption;
    procedure CopyDataToGUIElelemts;
    procedure HRTFFileChanged;
  end;

var
  FmHRTFEditor: TFmHRTFEditor;

implementation

{$R *.dfm}

uses
  Filectrl, Registry, DAV_DLLResources, DAV_ChunkClasses, HEeti;

const
  CDegToRad : Single = 2 * Pi / 360;
  CRadToDeg : Single = 360 / 2 * Pi;

procedure TFmHRTFEditor.FormCreate(Sender: TObject);
begin
 FHRTFFile := THrtfs.Create;
end;

procedure TFmHRTFEditor.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FHRTFFile);
end;

procedure TFmHRTFEditor.EdADConverterChange(Sender: TObject);
begin
 FHRTFFile.ADConverter := EdADConverter.Text;
end;

procedure TFmHRTFEditor.EdAmplifierChange(Sender: TObject);
begin
 FHRTFFile.Amplifier := EdAmplifier.Text;
end;

procedure TFmHRTFEditor.EdAuthorChange(Sender: TObject);
begin
 FHRTFFile.Author := EdAuthor.Text;
end;

procedure TFmHRTFEditor.EdContextChange(Sender: TObject);
begin
 FHRTFFile.Context := EdContext.Text;
end;

procedure TFmHRTFEditor.EdCopyrightChange(Sender: TObject);
begin
 FHRTFFile.Copyright := EdCopyright.Text;
end;

procedure TFmHRTFEditor.EdDAConverterChange(Sender: TObject);
begin
 FHRTFFile.DAConverter := EdDAConverter.Text;
end;

procedure TFmHRTFEditor.EdDescriptionChange(Sender: TObject);
begin
 FHRTFFile.SubjectDescription := EdDescription.Text;
end;

procedure TFmHRTFEditor.EdLoudspeakerChange(Sender: TObject);
begin
 FHRTFFile.Loudspeaker := EdLoudspeaker.Text;
end;

procedure TFmHRTFEditor.EdManufacturerChange(Sender: TObject);
begin
 FHRTFFile.MicManufacturer := EdManufacturer.Text;
end;

procedure TFmHRTFEditor.EdMicNotesChange(Sender: TObject);
begin
 FHRTFFile.MicNotes := EdMicNotes.Text;
end;

procedure TFmHRTFEditor.EdMeasurementTypeChange(Sender: TObject);
begin
 FHRTFFile.MeasurementType := EdMeasurementType.Text;
end;

procedure TFmHRTFEditor.EdMicTypeChange(Sender: TObject);
begin
 FHRTFFile.MicType := EdMicType.Text;
end;

procedure TFmHRTFEditor.EdNameIDChange(Sender: TObject);
begin
 FHRTFFile.SubjectID := EdNameID.Text;
end;

procedure TFmHRTFEditor.EdNotesChange(Sender: TObject);
begin
 FHRTFFile.Notes := EdNotes.Text;
end;

procedure TFmHRTFEditor.EdRoomTypeChange(Sender: TObject);
begin
 FHRTFFile.RoomType := EdRoomType.Text;
end;

procedure TFmHRTFEditor.EdSignalTypeChange(Sender: TObject);
begin
 FHRTFFile.ExcitationType := EdSignalType.Text;
end;

procedure TFmHRTFEditor.EdTitleChange(Sender: TObject);
begin
 FHRTFFile.Title := EdTitle.Text;
end;

procedure TFmHRTFEditor.BtImportClick(Sender: TObject);
begin
 with OpenDialogWAV do
  if Execute then
   begin
    ADHRIR.LoadFromFile(FileName);
    FHRTFFile.AddChunk(TCustomHrir.Create(SEAzimuth.Value * CDegToRad,
      SEPolar.Value * CDegToRad, 44100, ADHRIR.SampleFrames,
      ADHRIR[0].ChannelDataPointer, ADHRIR[1].ChannelDataPointer));
   end;
end;

procedure TFmHRTFEditor.BtExportClick(Sender: TObject);
begin
 with SaveDialogWAV do
  if Execute
   then ADHRIR.SaveToFile(FileName);
end;

procedure TFmHRTFEditor.CalendarChange(Sender: TObject);
begin
 UpdateFullDateCaption;
 FHRTFFile.Date := Calendar.CalendarDate;
end;

procedure TFmHRTFEditor.UpdateFullDateCaption;
begin
 LbFullDate.Caption := DateToStr(Calendar.CalendarDate);
end;

procedure TFmHRTFEditor.FormShow(Sender: TObject);
begin
 UpdateFullDateCaption;

(*
 MIImportETIClick(Sender);
 SEAzimuth.Value := 1;
 SEPolar.Value := 1;
*)
end;

procedure TFmHRTFEditor.MIAutoconvertOldHRTFfilesClick(Sender: TObject);
var
  Dir : string;
  SR  : TSearchRec;
  fn  : TFileName;
begin
 Dir := 'C:\Users\Christian Budde\Projects\VSTPack\Resources\HRTFs';
 if FindFirst(Dir + '\*.HRTF', 0, SR) = 0 then
  repeat
   FHRTFFile.LoadFromFile(Dir + '\' + SR.Name);
   fn := Dir + '\' + Copy(SR.Name, 1, 8) + '.hrco'; 
   FHRTFFile.SaveToFile(fn);
  until FindNext(SR) <> 0;
 FindClose(sr);
end;

procedure TFmHRTFEditor.MIExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmHRTFEditor.MIImportDiffuseClick(Sender: TObject);
var
  Dir    : string;
  SR     : TSearchRec;
  ADC    : TAudioDataCollection32;
  FN     : TFileName;
  i      : array [0..1] of Integer;
  s      : string;
begin
// Dir := 'C:\Users\Christian Budde\Projects\VSTPack\Examples\Plugins\HRTF 3D\HRTFs\ToDo\Diffuse';
 with TRegIniFile.Create do
  try
   if OpenKey('SOFTWARE\Delphi ASIO & VST Project\HRTF Editor\', True)
    then Dir := ReadString('Diffuse Import', 'Directory', '');
   SelectDirectory('Select a directory', '', Dir);
   WriteString('Diffuse Import', 'Directory', Dir);
  finally
   Free;
  end;

 if DirectoryExists(Dir) then
  begin
   FHRTFFile.ClearHrirs;
   ADC := TAudioDataCollection32.Create(Self);
   try
    if FindFirst(Dir + '\*.wav', 0, SR) = 0 then
     repeat
      ADC.LoadFromFile(Dir + '\' + SR.Name);

      FN := ExtractFileName(SR.Name);

      i[0] := Pos('e', FN);
      s    := copy(FN, 2, i[0] - 2);
      i[1] := StrToInt(s);

      s    := copy(FN, i[0] + 1, Pos('a', FN) - (i[0] + 1));
      i[0] := StrToInt(s);

      if i[0] > 180 then i[0] := i[0] - 360;

      FHRTFFile.AddChunk(TCustomHrir.Create(i[0] * CDegToRad, i[1] * CDegToRad, 44100,
        ADC.SampleFrames, ADC[0].ChannelDataPointer, ADC[1].ChannelDataPointer));

      FHRTFFile.AddChunk(TCustomHrir.Create(-i[0] * CDegToRad, i[1] * CDegToRad, 44100,
        ADC.SampleFrames, ADC[1].ChannelDataPointer, ADC[0].ChannelDataPointer));

     until FindNext(SR) <> 0;
    FindClose(sr);

    ADHRIR.SampleFrames := ADC.SampleFrames;
   finally
    FreeAndNil(ADC);
   end;
  end;
 HRTFFileChanged;
end;

procedure TFmHRTFEditor.MIImportIRCAMClick(Sender: TObject);
var
  Dir    : string;
  SR     : TSearchRec;
  ADC    : TAudioDataCollection32;
  FN     : TFileName;
  i      : array [0..1] of Integer;
begin
 with TRegIniFile.Create do
  try
   if OpenKey('SOFTWARE\Delphi ASIO & VST Project\HRTF Editor\', True)
    then Dir := ReadString('IRCAM Import', 'Directory', '');
   SelectDirectory('Select a directory', '', Dir);
   WriteString('IRCAM Import', 'Directory', Dir);
  finally
   Free;
  end;

 if DirectoryExists(Dir) then
  begin
   FHRTFFile.ClearHrirs;
   ADC := TAudioDataCollection32.Create(Self);
   try
    if FindFirst(Dir + '\*.wav', 0, SR) = 0 then
     repeat
      ADC.LoadFromFile(Dir + '\' + SR.Name);

      FN := ExtractFileName(SR.Name);

      if Length(FN) < 8 then
       begin
        i[0] := StrToInt(FN[2] + FN[3] + FN[4]);
        i[1] := 0;
       end
      else
       begin
        i[0] := StrToInt(FN[19] + FN[20] + FN[21]);
        i[1] := StrToInt(FN[24] + FN[25] + FN[26]);
       end;

      while i[0] > 180 do i[0] := i[0] - 360;
      while i[1] > 180 do i[1] := i[1] - 360;

      FHRTFFile.AddChunk(TCustomHrir.Create(i[0] * CDegToRad, i[1] * CDegToRad, 44100,
        ADC.SampleFrames, ADC[0].ChannelDataPointer, ADC[1].ChannelDataPointer));

     until FindNext(SR) <> 0;
    FindClose(sr);

    ADHRIR.SampleFrames := ADC.SampleFrames;
   finally
    FreeAndNil(ADC);
   end;
  end;
 HRTFFileChanged;
end;

procedure TFmHRTFEditor.HRTFFileChanged;
begin
 LbHrtfIndex.Enabled   := FHRTFFile.HrirCount > 0;
 SEHrtfIndex.Enabled   := LbHrtfIndex.Enabled;
 LbAzimuth.Enabled     := LbHrtfIndex.Enabled;
 SEAzimuth.Enabled     := LbHrtfIndex.Enabled;
 LbPolar.Enabled       := LbHrtfIndex.Enabled;
 SEPolar.Enabled       := LbHrtfIndex.Enabled;
 LbPolarUnit.Enabled   := LbHrtfIndex.Enabled;
 LbAzimuthUnit.Enabled := LbHrtfIndex.Enabled;

 if SEHrtfIndex.Enabled
  then SEHrtfIndex.MaxValue := FHRTFFile.HrirCount - 1;

 if LbHrtfIndex.Enabled then SEHrtfIndexChange(Self);
end;

procedure TFmHRTFEditor.MIImportETIClick(Sender: TObject);
var
  Dir    : string;
  SR     : TSearchRec;
  ADC    : TAudioDataCollection32;
  FN     : TFileName;
  DelPos : Integer;
  AzmStr : string;
  PolStr : string;
  RadStr : string;
  i      : array [0..2] of Integer;
begin
(*
 with TFmEtiImport.Create(Self) do
  try
   ShowModal;
  finally
   Free;
  end;
*)

 with TRegIniFile.Create do
  try
   if OpenKey('SOFTWARE\Delphi ASIO & VST Project\HRTF Editor\', True)
    then Dir := ReadString('ETI Import', 'Directory', '');
   SelectDirectory('Select a directory', '', Dir);
   WriteString('ETI Import', 'Directory', Dir);
  finally
   Free;
  end;

 if DirectoryExists(Dir) then
  begin
   FHRTFFile.ClearHrirs;
   ADC := TAudioDataCollection32.Create(Self);
   try
    if FindFirst(Dir + '\*.wav', 0, SR) = 0 then
     repeat
      ADC.LoadFromFile(Dir + '\' + SR.Name);

      FN := ExtractFileName(SR.Name);

      DelPos := Pos('_', FN);
      AzmStr := Copy(FN, 1, DelPos - 1);

      Delete(FN, 1, DelPos);
      DelPos := Pos('_', FN);
      PolStr := Copy(FN, 1, DelPos - 1);

      Delete(FN, 1, DelPos);
      DelPos := Pos('.', FN);
      RadStr := Copy(FN, 1, DelPos - 1);

      i[0] := -StrToInt(AzmStr);
      i[1] := StrToInt(PolStr);
      i[2] := StrToInt(RadStr);

      while i[0] > 180 do i[0] := i[0] - 360;
      while i[1] > 180 do i[1] := i[1] - 180;
      while i[0] < -180 do i[0] := i[0] + 360;
      while i[1] < -180 do i[1] := i[1] + 180;

      FHRTFFile.AddChunk(TCustomHrir.Create(i[0] * CDegToRad,
        i[1] * CDegToRad, i[2], 44100, ADC.SampleFrames,
        ADC[0].ChannelDataPointer, ADC[1].ChannelDataPointer));

      if (i[0] <> -i[0]) and (abs(i[0]) < 180)
       then FHRTFFile.AddChunk(TCustomHrir.Create(-i[0] * CDegToRad,
              i[1] * CDegToRad, i[2], 44100, ADC.SampleFrames,
              ADC[1].ChannelDataPointer, ADC[0].ChannelDataPointer));

     until FindNext(SR) <> 0;
    FindClose(sr);

    ADHRIR.SampleFrames := ADC.SampleFrames;
   finally
    FreeAndNil(ADC);
   end;
  end;
 HRTFFileChanged;
end;

procedure TFmHRTFEditor.MINewClick(Sender: TObject);
begin
 FFileName := '';
 MISave.Enabled := False;
 FHRTFFile.Clear;
end;

procedure TFmHRTFEditor.CopyDataToGUIElelemts;
begin
 // copy data to GUI elements
 with FHRTFFile do
  begin
   EdADConverter.Text := ADConverter;
   EdAmplifier.Text := Amplifier;
   EdAuthor.Text := Author;
   EdContext.Text := Context;
   EdCopyright.Text := Copyright;
   EdDAConverter.Text := DAConverter;
   EdDescription.Text := SubjectDescription;
   EdLoudspeaker.Text := Loudspeaker;
   EdManufacturer.Text := MicManufacturer;
   EdMicNotes.Text := MicNotes;
   EdMicType.Text := MicType;
   EdNameID.Text := SubjectID;
   EdNotes.Text := Notes;
   EdRoomType.Text := RoomType;
   EdTitle.Text := Title;
   EdSignalType.Text := ExcitationType;
   EdMeasurementType.Text := MeasurementType;
   RbUnknown.Checked := SubjectSex = stUnknown;
   RbGeneric.Checked := SubjectSex = stGeneric;
   RbMale.Checked := SubjectSex = stMale;
   RbFemale.Checked := SubjectSex = stFemale;
   SEHeight.Value := round(100 * RoomHeight);
   SELength.Value := round(100 * RoomLength);
   SEWidth.Value := round(100 * RoomWidth);
   SEDistance.Value := round(100 * FHRTFFile.Distance);
  end;
end;

procedure TFmHRTFEditor.MIOpenClick(Sender: TObject);
begin
 with OpenDialog do
  if Execute then
   begin
    FHRTFFile.LoadFromFile(FileName);
    FFileName := FileName;
    MISave.Enabled := FFileName <> '';
    CopyDataToGUIElelemts;
    HRTFFileChanged;
   end;
end;

procedure TFmHRTFEditor.MISaveAsClick(Sender: TObject);
var
  VST : TPEResourceModule;
  RS  : TResourceStream;
  RD  : TResourceDetails;
  MS  : TMemoryStream;
  i   : Integer;
begin
 with SaveDialog do
  if Execute then
   case FilterIndex of
    1 : begin
         FHRTFFile.SaveToFile(FileName);
         FFileName := FileName;
         MISave.Enabled := FFileName <> '';
        end;
    2 : begin
         VST := TPEResourceModule.Create;

         RS := TResourceStream.Create(HInstance, 'HRTF3D', 'DLL');
         try
          VST.LoadFromStream(RS);
         finally
          FreeAndNil(RS);
         end;

         for i := 0 to VST.ResourceCount - 1 do
          if (VST.ResourceDetails[i].ResourceType = 'HRTF') and
            (VST.ResourceDetails[i].ResourceName = 'DEFAULT') then
           begin
            VST.DeleteResource(i);
            break;
           end;


         MS := TMemoryStream.Create;
         FHRTFFile.SaveToStream(MS);
         with MS do
          try
           RD := TResourceDetails.CreateResourceDetails(VST, 0, 'DEFAULT', 'HRTF', Size, Memory);
           VST.AddResource(RD);
          finally
           FreeAndNil(MS);
          end;

          VST.SortResources;
          VST.SaveToFile(FileName);
        end;
   end;
end;

procedure TFmHRTFEditor.MISaveClick(Sender: TObject);
begin
 if FFileName <> ''
  then FHRTFFile.SaveToFile(FFileName);
end;

procedure TFmHRTFEditor.RbFemaleClick(Sender: TObject);
begin
 FHRTFFile.SubjectSex := stFemale;
end;

procedure TFmHRTFEditor.RbGenericClick(Sender: TObject);
begin
 FHRTFFile.SubjectSex := stGeneric;
end;

procedure TFmHRTFEditor.RbMaleClick(Sender: TObject);
begin
 FHRTFFile.SubjectSex := stMale;
end;

procedure TFmHRTFEditor.RbUnknownClick(Sender: TObject);
begin
 FHRTFFile.SubjectSex := stUnknown;
end;

procedure TFmHRTFEditor.SEHrirPosChange(Sender: TObject);
begin
 FHRTFFile.InterpolateHrir(SEAzimuth.Value * CDegToRad,
   SEPolar.Value * CDegToRad, ADHRIR.SampleFrames,
   ADHRIR[0].ChannelDataPointer, ADHRIR[1].ChannelDataPointer);
 AudioDataDisplayLeft.Invalidate;
 AudioDataDisplayRight.Invalidate;
end;

procedure TFmHRTFEditor.SEDistanceChange(Sender: TObject);
begin
 FHRTFFile.Distance := 0.01 * SEDistance.Value;
end;

procedure TFmHRTFEditor.SEHeightChange(Sender: TObject);
begin
 FHRTFFile.RoomHeight := 0.01 * SEHeight.Value;
end;

procedure TFmHRTFEditor.SEHrtfIndexChange(Sender: TObject);
begin
 assert(SEHrtfIndex.Value < FHRTFFile.HrirCount);
 FHRTFFile.GetHrirByIndex(SEHrtfIndex.Value, ADHRIR.SampleFrames,
   ADHRIR[0].ChannelDataPointer, ADHRIR[1].ChannelDataPointer);
 AudioDataDisplayLeft.Invalidate;
 AudioDataDisplayRight.Invalidate;

 SEPolar.OnChange := nil;
 SEAzimuth.OnChange := nil;
 try
  with FHRTFFile.Hrir[SEHrtfIndex.Value] do
   begin
    SEAzimuth.Value := round(360 / (2 * Pi) * Azimuth);
    SEPolar.Value := round(360 / (2 * Pi) * Polar);
   end;
 finally
  SEAzimuth.OnChange := SEHrirPosChange;
  SEPolar.OnChange := SEHrirPosChange;
 end;
end;

procedure TFmHRTFEditor.SELengthChange(Sender: TObject);
begin
 FHRTFFile.RoomLength := 0.01 * SELength.Value;
end;

procedure TFmHRTFEditor.SEWidthChange(Sender: TObject);
begin
 FHRTFFile.RoomWidth := 0.01 * SEWidth.Value;
end;

procedure TFmHRTFEditor.SEMonthChange(Sender: TObject);
begin
 Calendar.Month := SEMonth.Value;
end;

procedure TFmHRTFEditor.SEYearChange(Sender: TObject);
begin
 Calendar.Year := SEYear.Value;
end;

procedure TFmHRTFEditor.TSHrtfDataResize(Sender: TObject);
begin
 AudioDataDisplayLeft.Height := (TSHrtfData.Height - SEHrtfIndex.Height - 16) div 2;
 AudioDataDisplayRight.Height := AudioDataDisplayLeft.Height;
 AudioDataDisplayRight.Top := AudioDataDisplayLeft.Top + AudioDataDisplayLeft.Height + 6;
end;

end.
