unit SEIntToListGUI;

interface

uses
  Windows, DAV_SECommon, DAV_SEModule, DAV_SEGUI, SEIntToListModule;

const
  pinIn   = 0;
  pinOut  = 1;
  pinMode = 2;

type
  TSEIntToListGui = class(TSEGUIBase)
  protected
    procedure GuiPinValueChange(CurrentPin: TSeGuiPin); override;
  public
    constructor Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer); override;
  end;

implementation

uses
  SysUtils;

constructor TSEIntToListGui.Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer);
begin
 inherited;
end;

procedure TSEIntToListGui.GuiPinValueChange(CurrentPin: TSeGuiPin);
var
  Mode      : Integer;
  InValue   : Integer;
  OutValue  : Integer;
  it        : TItEnumList;
  ExtraData : TSeSdkString2;
begin
 inherited;
 Mode := Pin[pinMode].ValueAsInteger;

 case CurrentPin.PinIndex of
  pinIn,
  pinMode:
   begin
    InValue := Pin[pinIn].ValueAsInteger;
    if (Mode = 0) then // calc what value this Index maps to
     begin
      ExtraData := Pin[pinOut].getExtraData;
      it := TItEnumList.Create(ExtraData);
      it.First;
      while (not it.IsDone) and (it.CurrentItem^.Index <> InValue) do it.Next;
      if not it.IsDone
       then Pin[pinOut].ValueAsInteger :=  it.CurrentItem^.Value;
     end else Pin[pinOut].ValueAsInteger := InValue;
   end;
  pinOut:
   begin
    OutValue := Pin[pinOut].ValueAsInteger;
    if Mode = 0 then // calc what Index this value maps to
     begin
      ExtraData := Pin[pinOut].getExtraData;
      it := TItEnumList.Create(ExtraData);
      it.First;
      while (not it.IsDone) and (it.CurrentItem^.Index <> OutValue) do it.Next;
      if not it.IsDone
       then Pin[pinIn].ValueAsInteger := it.CurrentItem^.value;
     end else Pin[pinIn].ValueAsInteger := OutValue;
   end;
 end;
end;

end.
