unit URMCPropertyEditor;
interface
uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.StdCtrls,
  DesignIntf, DesignEditors,Graphics,Types, VCLEditors,URMCPropertyForm;

Type
  TRMCStyleEditor = class(TClassProperty )
  private
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

type
  TRMCDefaultEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  procedure Register;

implementation

uses Dialogs,URMCControls, URMCConstants,TypInfo;

function DeEditor(RMCElement:TRMCElement):boolean;
begin
  with TRMCPropertyForm.Create(NIL) do
  begin
    result:=Execute(RMCElement);
    Free;
  end;
end;

function TRMCStyleEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end {GetAttributes};

procedure TRMCStyleEditor.Edit;
begin
  ShowMessage('Please doubleclick control... ');
end;


// Returns TMySetProperty as the property editor used for Options in TButtonEx class
function MyCustomPropMapper(Obj: TPersistent; PropInfo: PPropInfo): TPropertyEditorClass;
begin
  Result := nil;
  if Assigned(Obj) and (Obj is TRMCElement) and SameText(String(PropInfo.Name), 'Options') then begin
    Result := TRMCStyleEditor;
  end;
end;

procedure Register;
begin
//  RegisterPropertyEditor(TypeInfo(TOptions), TRMCElement, 'Options', TRMCStyleEditor);
  RegisterComponentEditor(TRMCElement, TRMCDefaultEditor);
  RegisterPropertyMapper(MyCustomPropMapper);
end;


{ TRMCDefaultEditor }

procedure TRMCDefaultEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0: begin
          if DeEditor(Component as TRMCElement ) then
            Designer.Modified;
       end;
  end;

end;

function TRMCDefaultEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  case Index of
    0: Result := 'Edit Shape';
  end;
end;

function TRMCDefaultEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.

