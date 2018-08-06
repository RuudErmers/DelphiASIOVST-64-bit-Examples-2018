unit ProgressBarUnit;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, ComCtrls;

type
  TFmProgressBar = class(TForm)
    ProgressBar: TProgressBar;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

end.

