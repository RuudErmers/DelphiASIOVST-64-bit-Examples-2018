unit TestDAV_Bindings;

interface

uses
  TestFramework, Classes, DAV_Bindings, DAV_ProcessorInfo;

type
  TBindingTestFunction = function: Integer;

  TestTFunctionBinding = class(TTestCase)
  strict private
    FFunctionBinding: TFunctionBinding;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestClearAdd;
    procedure TestRebind;
  end;

  TestTFunctionBindingList = class(TTestCase)
  strict private
    FFunctionBindingList: TFunctionBindingList;
    FFunctionBinding: TFunctionBinding;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHasBinding;
    procedure TestGetBindingByPointer;
    procedure TestAddRemoveBinding;
    procedure TestRebind;
  end;

implementation

var
  GBindingTestFunction: TBindingTestFunction;

function TestBinding1: Integer;
begin
  Result := 1;
end;

function TestBinding2: Integer;
begin
  Result := 2;
end;



procedure TestTFunctionBinding.SetUp;
begin
 FFunctionBinding := TFunctionBinding.Create(@@GBindingTestFunction, @TestBinding1);
 with FFunctionBinding do
 begin
   Add(@TestBinding1, []);
   Add(@TestBinding2, [pfMMX]);
 end;
end;

procedure TestTFunctionBinding.TearDown;
begin
 FFunctionBinding.Free;
 FFunctionBinding := nil;
end;

procedure TestTFunctionBinding.TestClearAdd;
begin
 FFunctionBinding.Clear;

 CheckEquals(0, FFunctionBinding.FunctionCount);

 with FFunctionBinding do
 begin
   Add(@TestBinding1, []);
   Add(@TestBinding2, [pfMMX]);
   RebindProcessorSpecific;
 end;

 CheckEquals(2, FFunctionBinding.FunctionCount);
end;

procedure TestTFunctionBinding.TestRebind;
begin
  FFunctionBinding.Rebind([]);
  CheckEquals(1, GBindingTestFunction);

  FFunctionBinding.Rebind([pfMMX]);
  CheckEquals(2, GBindingTestFunction);
end;


{ TestTFunctionBindingList }

procedure TestTFunctionBindingList.SetUp;
var
  FunctionBinding : TFunctionBinding;
begin
  FFunctionBindingList := TFunctionBindingList.Create;

  FFunctionBinding := TFunctionBinding.Create(@@GBindingTestFunction);
  with FFunctionBinding do
  begin
    // add two test functions
    Add(@TestBinding1, []);
    Add(@TestBinding2, [pfMMX]);

    // rebind
    Rebind([]);
  end;
  FFunctionBindingList.AddBinding(FFunctionBinding);
end;

procedure TestTFunctionBindingList.TearDown;
begin
  FFunctionBindingList.Free;
  FFunctionBindingList := nil;
end;

procedure TestTFunctionBindingList.TestHasBinding;
begin
  CheckTrue(FFunctionBindingList.HasBinding(FFunctionBinding));
end;

procedure TestTFunctionBindingList.TestGetBindingByPointer;
begin
  with FFunctionBindingList do
  begin
    CheckTrue(GetBindingByPointer(@@GBindingTestFunction) <> nil);
    CheckTrue(GetBindingByPointer(@TestBinding1) <> nil);
    CheckTrue(GetBindingByPointer(@TestBinding2) <> nil);
  end;
end;

procedure TestTFunctionBindingList.TestAddRemoveBinding;
begin
  FFunctionBindingList.RemoveBinding(FFunctionBinding);
  CheckEquals(0, FFunctionBindingList.BindingCount);

  FFunctionBindingList.AddBinding(FFunctionBinding);
  CheckEquals(1, FFunctionBindingList.BindingCount);
end;

procedure TestTFunctionBindingList.TestRebind;
begin
  FFunctionBindingList.Rebind([]);

  CheckEquals(1, GBindingTestFunction);

  FFunctionBindingList.Rebind([pfMMX]);

  CheckEquals(2, GBindingTestFunction);
end;


initialization
  RegisterTest(TestTFunctionBinding.Suite);
  RegisterTest(TestTFunctionBindingList.Suite);
end.

