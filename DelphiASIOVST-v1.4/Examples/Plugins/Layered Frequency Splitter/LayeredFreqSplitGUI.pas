unit LayeredFreqSplitGUI;

{$I DAV_Compiler.inc}

interface

uses 
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Forms,
  DAV_Types, DAV_VSTModule;

type
  TFmLayeredFreqSplit = class(TForm)

  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

end.
