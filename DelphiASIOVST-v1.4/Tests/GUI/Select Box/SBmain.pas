unit SBmain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DAV_GuiBaseControl, DAV_GuiSelectBox, DAV_GuiCustomControl;

type
  TFmSelectBoxTest = class(TForm)
    LbAntiAlias: TLabel;
    LbArrowWidth: TLabel;
    LbLineWidth: TLabel;
    LbRadius: TLabel;
    SbAntiAlias: TGuiSelectBox;
    SbArrowWidth: TGuiSelectBox;
    SbCornerRadius: TGuiSelectBox;
    SbLineWidth: TGuiSelectBox;
    procedure FormShow(Sender: TObject);
    procedure SbLineWidthChange(Sender: TObject);
    procedure SbCornerRadiusChange(Sender: TObject);
    procedure SbArrowWidthChange(Sender: TObject);
  end;

var
  FmSelectBoxTest: TFmSelectBoxTest;

implementation

{$R *.dfm}

procedure TFmSelectBoxTest.FormShow(Sender: TObject);
begin
 SBCornerRadius.ItemIndex := Round(SbAntiAlias.BorderRadius - 1);
 SBLineWidth.ItemIndex := Round(SbAntiAlias.BorderWidth) - 1;
 SbArrowWidth.ItemIndex := SbAntiAlias.ArrowWidth - 1;
end;

procedure TFmSelectBoxTest.SbArrowWidthChange(Sender: TObject);
begin
 SbAntiAlias.ArrowWidth := SbArrowWidth.ItemIndex + 1;
 SBCornerRadius.ArrowWidth := SbAntiAlias.ArrowWidth;
 SBLineWidth.ArrowWidth := SbAntiAlias.ArrowWidth;
 SbArrowWidth.ArrowWidth := SbAntiAlias.ArrowWidth;
end;

procedure TFmSelectBoxTest.SbCornerRadiusChange(Sender: TObject);
begin
 SbAntiAlias.BorderRadius := SBCornerRadius.ItemIndex + 1;
 SBCornerRadius.BorderRadius := SbAntiAlias.BorderRadius;
 SBLineWidth.BorderRadius := SbAntiAlias.BorderRadius;
 SbArrowWidth.BorderRadius := SbAntiAlias.BorderRadius;
end;

procedure TFmSelectBoxTest.SbLineWidthChange(Sender: TObject);
begin
 SbAntiAlias.BorderWidth := SBLineWidth.ItemIndex + 1;
 SBCornerRadius.BorderWidth := SbAntiAlias.BorderWidth;
 SBLineWidth.BorderWidth := SbAntiAlias.BorderWidth;
 SbArrowWidth.BorderWidth := SbAntiAlias.BorderWidth;
end;

end.
