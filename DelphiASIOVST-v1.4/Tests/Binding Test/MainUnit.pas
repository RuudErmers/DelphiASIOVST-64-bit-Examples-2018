unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DAV_Bindings;

type
  TFmBindingTest = class(TForm)
    LbFunctionResult: TLabel;
    LbResult: TLabel;
    procedure FormCreate(Sender: TObject);
  end;

var
  FmBindingTest : TFmBindingTest;

  Test            : function (A, B: Integer): Integer;
  TestBinding     : TFunctionBinding;
  TestBindingList : TFunctionBindingList;


implementation

{$R *.dfm}

function Test_Pascal(A, B: Integer): Integer;
begin
 Result := A + B;
end;

function Test_DummyMMX(A, B: Integer): Integer;
begin
 Result := A + B;
end;

function Test_DummySSE(A, B: Integer): Integer;
begin
 Result := A + B;
end;

procedure TFmBindingTest.FormCreate(Sender: TObject);
begin
 LbResult.Caption := IntToStr(Test(2, 3));

 Assert(GetBinding(@@Test) <> nil);
 Assert(GetBinding(@Test_Pascal) <> nil);
 Assert(GetBinding(@Test_DummyMMX) <> nil);
 Assert(GetBinding(@Test_DummySSE) <> nil);
end;

initialization
 TestBinding     := TFunctionBinding.Create(@@Test, @Test_Pascal);
 TestBindingList := TFunctionBindingList.Create;
 TestBindingList.AddBinding(TestBinding);
 with TestBinding do
   begin
    Add(@Test_Pascal);
    Add(@Test_DummyMMX, [pfMMX]);
    Add(@Test_DummySSE, [pfSSE]);
    Rebind([pfMMX]);
   end;

finalization
  FreeAndNil(TestBindingList);
  FreeAndNil(TestBinding);

end.
