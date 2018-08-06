unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DAV_GuiPng, DAV_GuiPixelMap, DAV_GuiFileFormats;

type
  TFmFileFormatTest = class(TForm)
    PaintBox: TPaintBox;
    OpenDialog: TOpenDialog;
    procedure PaintBoxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
  private
    FPixelMap : TGuiCustomPixelMap;
  public
    { Public-Deklarationen }
  end;

var
  FmFileFormatTest: TFmFileFormatTest;

implementation

{$R *.dfm}

procedure TFmFileFormatTest.FormCreate(Sender: TObject);
begin
 FPixelMap := TGuiPixelMapMemory.Create;
end;

procedure TFmFileFormatTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FPixelMap);
end;

procedure TFmFileFormatTest.PaintBoxClick(Sender: TObject);
begin
 with OpenDialog do
  if Execute then
   begin
    FPixelMap.LoadFromFile(OpenDialog.FileName);
    Self.ClientWidth := FPixelMap.Width + 16;
    Self.ClientHeight := FPixelMap.Height + 16;
    PaintBox.Invalidate;
   end;
end;

procedure TFmFileFormatTest.PaintBoxPaint(Sender: TObject);
begin
 if Assigned(FPixelMap)
  then FPixelMap.PaintTo(PaintBox.Canvas);
end;

end.

