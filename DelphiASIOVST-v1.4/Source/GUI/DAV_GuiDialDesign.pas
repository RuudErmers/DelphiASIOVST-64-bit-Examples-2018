unit DAV_GuiDialDesign;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LazIDEIntf, PropEdits,{$ELSE} {$IFDEF DELPHI6_UP}
  DesignIntf, DesignEditors, {$ELSE} DsgnIntf, {$ENDIF} {$ENDIF}
  Classes, TypInfo, DAV_Classes, DAV_GuiDesign, DAV_GuiDial,
  DAV_GuiDialRenderer;

type
  TPrimitiveClassProperty = class(TCustomClassProperty)
  protected
    class function GetClassList: TClassList; override;
    function GetObject: TObject; override;
    procedure SetClassName(const CustomClass: string); override;
  end;

implementation

{ TPrimitiveClassProperty }

class function TPrimitiveClassProperty.GetClassList: TClassList;
begin
  Result := PrimitiveClassList;
end;

function TPrimitiveClassProperty.GetObject: TObject;
begin
  Result := TGuiDialLayerCollectionItem(GetComponent(0)).Primitive;
end;

procedure TPrimitiveClassProperty.SetClassName(const CustomClass: string);
begin
  TGuiDialLayerCollectionItem(GetComponent(0)).PrimitiveClassName := CustomClass;
end;

end.
