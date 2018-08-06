unit DAV_GuiAudioDataDisplayCursor;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common;

type
  TCustomGuiAudioDataDisplayCursor = class(TPersistent)
  private
    FSampleActive  : Int64;
    FSamplePassive : Int64;
    FOnChanged     : TNotifyEvent;
    procedure SetSampleActive(const Value: Int64);
    procedure SetSamplePassive(const Value: Int64);
    function GetSampleLower: Int64;
    function GetSampleUpper: Int64;
  protected
    procedure CursorChanged; virtual;
    procedure SampleActiveChanged; virtual;
    procedure SamplePassiveChanged; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    property SampleActive: Int64 read FSampleActive write SetSampleActive;
    property SamplePassive: Int64 read FSamplePassive write SetSamplePassive;
    property SampleLower: Int64 read GetSampleLower;
    property SampleUpper: Int64 read GetSampleUpper;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TGuiAudioDataDisplayCursor = class(TCustomGuiAudioDataDisplayCursor)
  published
    property SampleActive;
    property SamplePassive;
    property SampleLower;
    property SampleUpper;
  end;

implementation

{ TCustomGuiAudioDataDisplayCursor }

procedure TCustomGuiAudioDataDisplayCursor.SetSampleActive(const Value: Int64);
begin
 if FSampleActive <> Value then
  begin
   FSampleActive := Value;
   SampleActiveChanged;
  end;
end;

procedure TCustomGuiAudioDataDisplayCursor.SetSamplePassive(const Value: Int64);
begin
 if FSamplePassive <> Value then
  begin
   FSamplePassive := Value;
   SamplePassiveChanged;
  end;
end;

function TCustomGuiAudioDataDisplayCursor.GetSampleLower: Int64;
begin
 if FSampleActive < FSamplePassive
  then result := FSampleActive
  else result := FSamplePassive;
end;

function TCustomGuiAudioDataDisplayCursor.GetSampleUpper: Int64;
begin
 if FSampleActive > FSamplePassive
  then result := FSampleActive
  else result := FSamplePassive;
end;

procedure TCustomGuiAudioDataDisplayCursor.SampleActiveChanged;
begin
 CursorChanged;
end;

procedure TCustomGuiAudioDataDisplayCursor.SamplePassiveChanged;
begin
 CursorChanged;
end;

procedure TCustomGuiAudioDataDisplayCursor.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomGuiAudioDataDisplayCursor then
  with TCustomGuiAudioDataDisplayCursor(Dest) do
   begin
    FSampleActive := Self.FSampleActive;
    FSamplePassive := Self.FSamplePassive;
    FOnChanged := Self.FOnChanged;
   end
 else inherited;
end;

procedure TCustomGuiAudioDataDisplayCursor.CursorChanged;
begin
 if assigned(FOnChanged)
  then FOnChanged(Self);
end;

end.
