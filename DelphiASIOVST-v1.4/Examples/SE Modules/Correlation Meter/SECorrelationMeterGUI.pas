unit SECorrelationMeterGUI;

interface

uses
  Windows, Classes, Controls, DAV_SEModule, DAV_SEGUI, DAV_GuiCorrelationMeter,
  SECorrelationMeterModule;

const
  pinEnumOut = 2;

type
  TSECorrelationMeterGui = class(TSEGUIBase)
  private
    FCorrelationMeter : TGuiCorrelationMeter;
    function Handle: THandle;
    function InvalidateControl: Integer;
  protected
    procedure GuiWindowOpen(WI: PSEWndInfo); override;
    procedure GuiPaint(hDC: HDC; wi: PSEWndInfo); override;
    procedure GuiModuleMsg(AUserMsgID, ALength: Integer; AData: Pointer); override;
    procedure GuiWindowClose(WI: PSEWndInfo); override;
    procedure GuiPinValueChange(CurrentPin: TSEGuiPin); override;
  public
    constructor Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer); override;
    destructor Destroy; override;
  end;

implementation

uses
  DAV_Types, SysUtils, Graphics, DAV_GuiBaseControl;

constructor TSECorrelationMeterGui.Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer);
begin
 inherited;
 CallHost(seGuiHostSetWindowSize, 64, 64);
 CallHost(seGuiHostSetWindowType, 1); // 0 = Draw on SE's window (default), 1 = HWND based

// CallHost(seGuiHostSetWindowFlags, Integer(HWF_RESIZEABLE));
end;

destructor TSECorrelationMeterGui.Destroy;
begin
 if Assigned(FCorrelationMeter)
  then FreeAndNil(FCorrelationMeter);
 inherited;
end;

procedure TSECorrelationMeterGui.GuiWindowClose(WI: PSEWndInfo);
begin
 if Assigned(FCorrelationMeter)
  then FreeAndNil(FCorrelationMeter);
 inherited;
end;

procedure TSECorrelationMeterGui.GuiWindowOpen(WI: PSEWndInfo);
begin
 FCorrelationMeter := TGuiCorrelationMeter.Create(nil);
 with FCorrelationMeter do
  begin
   ParentWindow := HWND(CallHost(seGuiHostGetWindowHandle, WI.ContextHandle));
   Align := alClient;
  end;
 inherited;
end;

procedure TSECorrelationMeterGui.GuiPaint(hDC: HDC; wi :PSEWndInfo);
begin
 if Assigned(FCorrelationMeter) then
  with FCorrelationMeter do
   begin
    if Width <> wi.Width then Width := wi.Width;
    if Height <> wi.Height then Height := wi.Height;
   end;
end;

procedure TSECorrelationMeterGui.GuiPinValueChange(CurrentPin: TSEGuiPin);
begin
 inherited;
 case CurrentPin.PinIndex of
  0: FCorrelationMeter.Direction := TCorrelationMeterDirection(CurrentPin.ValueAsInteger);
  1: FCorrelationMeter.Correlation := CurrentPin.ValueAsSingle;
 end;
end;

procedure TSECorrelationMeterGui.GuiModuleMsg(AUserMsgID, ALength: Integer; AData: Pointer);
begin
 InvalidateControl;
end;

function TSECorrelationMeterGui.Handle: THandle;
begin
 Result := CallHost(seGuiHostGetHandle);
end;

function TSECorrelationMeterGui.InvalidateControl: Integer;
begin
 Result := CallHost(seGuiHostRequestRepaint);
end;

end.
