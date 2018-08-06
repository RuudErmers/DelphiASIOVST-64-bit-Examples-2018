unit URMCVSTView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, URMCBaseControlPanel,URMCControls;

type
  TOnPCCChanged = procedure (Sender:TObject;pcc,value: Integer) of object;
  TOnUINoteEvent = procedure (Sender:TObject;pitch:byte;velo: single) of object;
  TRMCVSTView = class (TRMCBaseControlPanel)
  private
  protected
  public
   { property }  OnPCCChanged: TOnPCCChanged;
   { property }  OnNoteUIOn:  TOnUINoteEvent;
   { property }  OnNoteUIOff: TOnUINoteEvent;

    procedure ChangePCC(pcc,value: Integer); virtual;abstract;
 end;

implementation


end.
