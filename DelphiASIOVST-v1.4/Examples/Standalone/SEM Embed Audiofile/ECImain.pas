unit ECImain;

interface

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, DAV_DLLResources;

type
  TFmSemEmbedAudioFile = class(TForm)
    ListBox: TListBox;
    MainMenu: TMainMenu;
    MIAddWAV: TMenuItem;
    MIExit: TMenuItem;
    MIFile: TMenuItem;
    MIOpenSEM: TMenuItem;
    MISave: TMenuItem;
    MISaveAs: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    OpenDialogSEM: TOpenDialog;
    OpenDialogWAV: TOpenDialog;
    SaveDialogSEM: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MIAddWAVClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure MIOpenSEMClick(Sender: TObject);
    procedure MISaveAsClick(Sender: TObject);
    procedure MISaveClick(Sender: TObject);
  private
    FFileName : TFileName;
  public
    FSEModule : TPEResourceModule;
  end;

var
  FmSemEmbedAudioFile: TFmSemEmbedAudioFile;

implementation

{$R *.dfm}

procedure TFmSemEmbedAudioFile.FormCreate(Sender: TObject);
var
  RS : TResourceStream;
begin
 FSEModule := TPEResourceModule.Create;
 RS := TResourceStream.Create(HInstance, 'AudioFileOscillator', 'DLL');
 try
  FSEModule.LoadFromStream(RS);
 finally
  FreeAndNil(RS);
 end;
end;

procedure TFmSemEmbedAudioFile.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FSEModule);
end;

procedure TFmSemEmbedAudioFile.MIOpenSEMClick(Sender: TObject);
var
  i : Integer;
begin
 with OpenDialogSEM do
  if Execute then
   try
    ListBox.Clear;

    FSEModule.LoadFromFile(FileName);
    SaveDialogSEM.FileName := FileName;

    for i := 0 to FSEModule.ResourceCount - 1 do
     if FSEModule.ResourceDetails[i].ResourceType = 'WAVETABLE'
      then ListBox.Items.Add(FSEModule.ResourceDetails[i].ResourceName);

    FFileName := FileName;
   finally
    MISave.Enabled := FileExists(FFileName);
   end;
end;

procedure TFmSemEmbedAudioFile.MISaveAsClick(Sender: TObject);
begin
 with SaveDialogSEM do
  if Execute then
   try
    FSEModule.SortResources;
    FSEModule.SaveToFile(FileName);
    FFileName := FileName;
   finally
    MISave.Enabled := FileExists(FFileName);
   end;
end;

procedure TFmSemEmbedAudioFile.MISaveClick(Sender: TObject);
begin
 if FFileName <> ''
  then FSEModule.SaveToFile(FFileName)
  else MISave.Enabled := False;
end;

function RemoveFileExt(Filename: TFileName): TFileName;
var
  DotPos : Integer;
begin
 result := Filename;
 DotPos := Pos('.', result);
 if DotPos > 0
  then result := Copy(result, 1, DotPos - 1);
 result := Uppercase(result); 
end;

procedure TFmSemEmbedAudioFile.MIAddWAVClick(Sender: TObject);
var
  i  : Integer;
  RD : TResourceDetails;
begin
 with OpenDialogWAV do
  if Execute then
   for i := 0 to Files.Count - 1 do
    begin
     with TMemoryStream.Create do
      try
       ListBox.Items.Add(ExtractFileName(Files[i]));
       LoadFromFile(Files[i]);
       RD := TResourceDetails.CreateResourceDetails(FSEModule, 0,
         RemoveFileExt(ExtractFileName(Files[i])), 'WAVETABLE', Size, Memory);
       FSEModule.AddResource(RD);
      finally
       Free;
      end;
    end;
end;

procedure TFmSemEmbedAudioFile.MIExitClick(Sender: TObject);
begin
 Close;
end;

end.
