unit DAV_TestGuiPngDisplay;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, PngImage, DAV_GuiCommon, DAV_GuiPng, DAV_GuiPixelMap;

type
  TFmDisplay = class(TForm)
    BtNo: TButton;
    BtYes: TButton;
    Image: TImage;
    LbQuestion: TLabel;
    LbRenderer: TLabel;
    RbInternal: TRadioButton;
    RbPngImage: TRadioButton;
    procedure RbInternalClick(Sender: TObject);
    procedure RbPngImageClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FReference : TGuiCustomPixelMap;
    FInternal  : TGuiCustomPixelMap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Reference: TGuiCustomPixelMap read FReference write FReference;
    property Internal: TGuiCustomPixelMap read FInternal write FInternal;
  end;

var
  FmDisplay: TFmDisplay;

implementation

{$R *.dfm}

constructor TFmDisplay.Create(AOwner: TComponent);
begin
 inherited;
 FInternal := TGuiPixelMapMemory.Create;
 FReference := TGuiPixelMapMemory.Create;
end;

destructor TFmDisplay.Destroy;
begin
 FreeAndNil(FInternal);
 FreeAndNil(FReference);
 inherited;
end;

procedure TFmDisplay.FormShow(Sender: TObject);
begin
 FInternal.PaintTo(Image.Canvas);
end;

procedure TFmDisplay.RbInternalClick(Sender: TObject);
begin
 FInternal.PaintTo(Image.Canvas);
 Image.Invalidate;
end;

procedure TFmDisplay.RbPngImageClick(Sender: TObject);
begin
 FReference.PaintTo(Image.Canvas);
 Image.Invalidate;
end;

end.
