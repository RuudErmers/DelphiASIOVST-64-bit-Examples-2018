unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DAV_GuiCustomControl, DAV_GuiImageControl, DAV_GuiFader, DAV_GuiPngList,
  DAV_GuiImageList, DAV_GuiGraphicControl, DAV_GuiLabel;

type
  TFmFaderDemo = class(TForm)
    VerticalFader: TGuiFader;
    HorizontalFader: TGuiFader;
    PNGList: TGuiPNGList;
    GuiFader1: TGuiFader;
    LbFaders: TGuiLabel;
  end;

var
  FmFaderDemo: TFmFaderDemo;

implementation

{$R *.dfm}

end.

